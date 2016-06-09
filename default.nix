{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, data-reify, stdenv, vector-space }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.4.0";
        src = ./.;
        libraryHaskellDepends = [ base data-reify vector-space ];
        homepage = "https://github.com/sleexyz/hylogen";
        description = "an EDSL for live-coding fragment shaders";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
