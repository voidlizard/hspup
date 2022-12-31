{
description = "hspup";

inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:
  haskell-flake-utils.lib.simpleCabal2flake {
      inherit self nixpkgs;
      systems = [ "x86_64-linux" ];

      name = "hspup";

      shellWithHoogle = true;

      haskellFlakes = with inputs; [
      ];

      hpPreOverrides = { pkgs }: new: old:
        with pkgs.haskell.lib;
        with haskell-flake-utils.lib;
        tunePackages pkgs old {
          mr-env            = [ (jailbreakUnbreak pkgs) dontCheck ];
          thread-supervisor = [ (jailbreakUnbreak pkgs) dontCheck ];
          higgledy          = [ (jailbreakUnbreak pkgs) dontCheck ];
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

      shellExtBuildInputs = {pkgs}: with pkgs; [
        haskellPackages.haskell-language-server
      ];

  };
}

