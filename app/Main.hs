module Main where

import Data.Functor
import Control.Monad
import Data.Attoparsec.Text
import Data.Char qualified as Char
import Data.Data
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import Options.Applicative as O
import Safe
import System.Directory
import System.FilePath.Posix
import Prettyprinter

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
    parser = hsubparser ( command "touch" (info pTouch (progDesc "touch dir/file recursively"))
                        )

    pTouch = do
      dir <- flag False True ( short 'd' <>  long "directory" )
      hs <-  flag False True ( short 'H' <> long "hs" )
      n <- strArgument ( metavar "NAME" )
      pure $ runTouch dir hs n


itsModule :: Text -> Bool
itsModule s | Text.null $ Text.filter (=='/') s = True
            | takeExtension (Text.unpack s) == ".hs" = True
            | otherwise = False



validModule :: Text -> Bool
validModule s = either (const False) (const True) (parseOnly p s)
  where
    p = satisfy Char.isUpper *> many (letter <|> digit <|> char '.') >> endOfInput


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



