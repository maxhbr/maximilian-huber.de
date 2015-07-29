{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, blaze-html, blaze-markup, clay
      , directory, filepath, markdown, split, stdenv, text, unix
      }:
      mkDerivation {
        pname = "maximilian-huber";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base blaze-html blaze-markup clay directory filepath markdown split
          text unix
        ];
        homepage = "maximilian-huber.de";
        description = "my homepage";
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
