# [*H Y L O G E N*](https://hylogen.com)

Hylogen is a purely functional language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding fragment shaders.

<br/>

It comes with [`hylogen-app`](https://github.com/sleexyz/hylogen-app), the standard rendering environment featuring:
- hot-reloading
- audio-reactive primitives
- texture backbuffering

<br/>


## Features

- *embedded in Haskell*
- simple and concise syntax
- compatible w/ standard Haskell operators (`+`, `*`, `*^`,  `<.>`)

## Setup
```
cabal update
cabal install hylogen
```

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

#### 1. run hylogen-app

```
hylogen-app Main.hs
```

#### 2. play!
Go to [localhost:5678](http://localhost:5678) in your browser.

You will now see your changes to `Main.hs` propagate to your WebGL rendering environment!

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. Live-coding audio-reactive shaders!
- [Type-Safe Observable Sharing](https://pdfs.semanticscholar.org/4838/bd0a91b3058b467fa31ad9e0810121b46388.pdf) by Andy Gill. [`data-reify`](https://hackage.haskell.org/package/data-reify) made compile times combinatorially faster!

## resources
- [hackage](https://hackage.haskell.org/package/hylogen)

- [examples](https://github.com/sleexyz/hylogen-yay)
