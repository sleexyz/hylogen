# [*H Y L O G E N*](https://hylogen.com)  
[![Hackage Status](https://img.shields.io/hackage/v/hylogen.svg)](https://hackage.haskell.org/package/hylogen)

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

Hylogen is a purely functional language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding fragment shaders, featuring:

- a simple and pure syntax
- standard operators (`+`, `*`, [`*^`,  `<.>`](https://hackage.haskell.org/package/vector-space))
- compat. w/ your fav haskell goodies (higher-order functions, ADTS, swanky polymorphism).

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

It comes with `hylide`, an accompanying rendering environment featuring:
- *hot-reloading*
- audio-reactive primitives
- texture backbuffering

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)


## Install
```
cabal update
cabal install hylogen
```

This will install the hylogen package and `hylide`, the rendering environment.

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## Usage

```haskell
-- ./Main.hs
module Main where
import Hylogen.WithHylide

color = vec4 (a, a, a, 1)
  where
    a = cos(uvN !X * sin(time / 10) * 10 + mouse !X)
      + sin(uvN !Y * sin(time / 10) * 10 + mouse !Y)

main = putStrLn . toGLSL $ color
```

#### run hylide...

```
hylide Main.hs
```

#### ... live-code!
Go to [localhost:5678](http://localhost:5678) in your browser.

You will now see your changes to `Main.hs` propagate to your WebGL rendering environment!

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. Live-coding audio-reactive shaders!
- [data-reify](https://hackage.haskell.org/package/data-reify) by Andy Gill.  Makes compile times combinatorially faster

## Resources
- **[examples](https://github.com/sleexyz/hylogen-yay)**
- [hackage](https://hackage.haskell.org/package/hylogen)
- [hylide](https://github.com/sleexyz/hylide)


