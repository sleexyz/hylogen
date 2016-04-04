{ mkDerivation, base, filepath, hinotify, process, random
      , stdenv, text, websockets
      }:
      mkDerivation {
        pname = "hylogen";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          base filepath hinotify process random text websockets
        ];
        homepage = "https://github.com/sleexyz/hylogen";
        description = "glsl edsl";
        license = stdenv.lib.licenses.unfree;
      }
