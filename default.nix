{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, mtl, primitive, stdenv, tasty
      , tasty-hunit, tasty-quickcheck, transformers
      }:
      mkDerivation {
        pname = "stt";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base mtl primitive ];
        testHaskellDepends = [
          base tasty tasty-hunit tasty-quickcheck transformers
        ];
        description = "A monad transformer version of the ST monad";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
