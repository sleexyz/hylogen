{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, filepath, fsnotify
      , http-types, process, stdenv, text, vector-space, data-reify
      , wai, wai-app-static, warp, websockets
      }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.0.9";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base vector-space ];
        executableHaskellDepends = [
          base bytestring filepath fsnotify http-types process
          text wai wai-app-static warp websockets data-reify
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
