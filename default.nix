{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, data-reify, mtl, stdenv, vector-space
      }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.4.1";
        src = ./.;
        libraryHaskellDepends = [ base data-reify mtl vector-space ];
        homepage = "https://github.com/sleexyz/hylogen";
        description = "Purely functional GLSL embedded in Haskell";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
