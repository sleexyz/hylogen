# [*H Y L O G E N*](https://hylogen.com)

Hylogen is a purely functional language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding fragment shaders, featuring:

- pure, simple, type-safe syntax
- compatability with
  - standard typeclass operators (`+`, `*`, [`*^`,  `<.>`](https://hackage.haskell.org/package/vector-space))
  - your fav haskell goodies (higher-order functions, ADTS, swanky polymorphism).

<br/>

It comes with `hyde`, the canonical rendering environment, featuring:
- *hot-reloading*
- audio-reactive primitives
- texture backbuffering

<br/>


## Install
```
cabal update
cabal install hylogen
```

This will install the hylogen libraries as well as `hyde`, the rendering environment.

<br/>

## Usage

```haskell
-- ./Main.hs
module Main where
import Hylogen

color = vec4 (a, a, a, 1)
  where
    a = cos(X uvN * sin(time/ 10) * 10 + X mouse)
      + sin(Y uvN * sin(time / 10) * 10 + Y mouse)

main = putStrLn . toGLSL $ color
```

#### 1. run hyde...

```
hyde Main.hs
```

#### 2. ... live-code!
Go to [localhost:5678](http://localhost:5678) in your browser.

You will now see your changes to `Main.hs` propagate to your WebGL rendering environment!

<br/>

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. Live-coding audio-reactive shaders!
- [Type-Safe Observable Sharing](https://pdfs.semanticscholar.org/4838/bd0a91b3058b467fa31ad9e0810121b46388.pdf) by Andy Gill. [`data-reify`](https://hackage.haskell.org/package/data-reify) made compile times combinatorially faster!

## Resources
- [hackage](https://hackage.haskell.org/package/hylogen)

- [examples](https://github.com/sleexyz/hylogen-yay)
