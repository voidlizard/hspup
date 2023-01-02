{-# Language OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Text
import Data.Char qualified as Char
import Data.Data
import Data.Functor
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.Maybe
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Options.Applicative as O
import Prettyprinter
import Safe
import System.Directory
import System.FilePath.Posix
import System.FilePattern.Directory

data Opts = OptHs Bool
            deriving (Eq,Ord,Show,Data)


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hspup"
  <> progDesc "making haskelling easier"
  )
  where
    parser ::  O.Parser (IO ())
    parser = hsubparser (  command "touch"  (info pTouch  (progDesc "touch dir/file recursively"))
                        <> command "module" (info pModule (progDesc "get module name from path or file"))
                        <> command "path"   (info pPath    (progDesc "guess module path from name"))
                        )

    pTouch = do
      dir <- flag False True ( short 'd' <>  long "directory" )
      hs <-  flag False True ( short 'H' <> long "hs" )
      n <- strArgument ( metavar "NAME" )
      pure $ runTouch dir hs n

    pModule = do
      n <- optional $ strArgument ( metavar "PATH" )
      pure $ runModule n

    pPath= do
      n <- optional $ strArgument ( metavar "NAME" )
      pure $ runPath n


itsModule :: Text -> Bool
itsModule s | Text.null $ Text.filter (=='/') s = True
            | takeExtension (Text.unpack s) == ".hs" = True
            | otherwise = False



validModule :: Text -> Bool
validModule s = either (const False) (const True) (parseOnly p s)
  where
    p = satisfy Char.isUpper *> many (letter <|> digit <|> char '.') >> endOfInput


runModule :: Maybe FilePath -> IO ()

runModule (Just fp) = putStrLn (makeHaskellModule fp)

runModule Nothing = getContents >>= putStrLn . makeHaskellModule


runPath :: Maybe FilePath -> IO ()
runPath fp = maybe fromStdin pure fp >>= makeGuess
  where
    fromStdin = getContents

    makeGuess :: FilePath -> IO ()
    makeGuess n = void $ runMaybeT $ do
      name <- MaybeT $ pure
                     $ headMay
                     $ filter validModule
                     $ foldMap Text.words
                     $ Text.lines (Text.pack n)

      -- liftIO $ print name

      let fn = makeFileName (Text.unpack name)
      let path = "**/" <> fn

      variants <- MaybeT $ liftIO
                         $ getDirectoryFilesIgnore "." [path] [".*/**", "dist-newstyle/**"] <&> listToMaybe

      liftIO $ putStr variants


runTouch :: Bool -> Bool -> FilePath -> IO ()


runTouch True _ s = do
  dir <- canonicalizePath s
  createDirectoryIfMissing True dir
  makeRelativeToCurrentDirectory dir >>= putStrLn

runTouch _ True s = do
  (dir, hs) <- canonicalizePath s <&> makeHaskellPath

  let path = dir </> hs
  exists <- doesFileExist path

  unless exists $ do
    createDirectoryIfMissing True (dir </> dropFileName hs)
    writeFile  path $ show
                    $ pretty "module" <+> pretty (makeHaskellModule hs)
                                      <+> "where"
    makeRelativeToCurrentDirectory path >>= putStrLn

runTouch False False s = do
  let (dir',file)  = splitFileName s
  dir <- canonicalizePath dir'

  let path = dir </> file

  exists <- doesFileExist path

  unless exists $ do
    createDirectoryIfMissing True dir
    Text.writeFile  path ""
    makeRelativeToCurrentDirectory path >>= putStrLn

makeFileName :: String -> FilePath
makeFileName f = addExtension (Text.unpack $ Text.replace "." "/" tf) ".hs"
  where
    tf = Text.pack f

makeHaskellModule :: FilePath -> String
makeHaskellModule s = s0
  where
    s0  = dropExtension $ Text.unpack
                        $ Text.replace "/" "."
                        $ Text.dropWhile (not . Char.isUpper) (Text.pack s)

makeHaskellPath :: FilePath -> (FilePath, FilePath)
makeHaskellPath fp = (pref, hsfile)
  where
    hsfile = addExtension (dropExtension (Text.unpack hscap)) ".hs"
    hscap = Text.intercalate "/" (map capitalize (Text.splitOn "/" (Text.pack hs)))
    (pref,hs) = go ("","") fp
    go acc [] = acc
    go (p,s) ( '/' : c : rest ) | Char.isUpper c = (p <> ['/'], s <> (c : rest))
    go (p,s) (c : rest) = go (p <> [c], s) rest

capitalize :: Text -> Text
capitalize x = case Text.uncons x of
  Nothing -> x
  Just (h,t) -> Text.cons  (Char.toUpper h) t



