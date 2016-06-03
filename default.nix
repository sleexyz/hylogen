{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, data-reify, filepath
      , fsnotify, hint, http-types, process, stdenv, text, vector-space
      , wai, warp, websockets
      }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.3.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base data-reify vector-space ];
        executableHaskellDepends = [
          aeson base bytestring filepath fsnotify hint http-types process
          text wai warp websockets
        ];
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
