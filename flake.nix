{
description = "hbsync-fetch";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/896219e5bde6efac72198550454e9dd9b5ed9ac9";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:

 haskell-flake-utils.lib.simpleCabalProject2flake {
   inherit self nixpkgs;
   systems = [ "x86_64-linux" ];
   name = "hspup";

   packageNames = [
     "hspup"
   ];

   packageDirs = {
     "hspup" = ".";
   };


   packagePostOverrides = { pkgs }: with pkgs; with haskell.lib; [
    disableExecutableProfiling
    disableLibraryProfiling
    dontBenchmark
    dontCoverage
    dontDistribute
    dontHaddock
    dontHyperlinkSource
    doStrip
    enableDeadCodeElimination
    justStaticExecutables

    dontCheck
   ];

   shellExtBuildInputs = {pkgs}: with pkgs;  [
     haskellPackages.haskell-language-server
   ];

 };


}
