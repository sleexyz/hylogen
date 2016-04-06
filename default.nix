{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, filepath, hinotify, network
      , process, random, stdenv, text, vector-space, websockets
      }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.0.5";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base vector-space ];
        executableHaskellDepends = [
          base bytestring filepath hinotify network process random text
          websockets
        ];
        homepage = "https://github.com/sleexyz/hylogen";
        description = "a tiny EDSL for live-coding fragment shaders";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
