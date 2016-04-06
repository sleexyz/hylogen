# [*H Y L O G E N*](https://hylogen.com)

Hylogen is a tiny language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding visuals.

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


main = putStrLn $ toGLSL $ Vec4 (a, a, a, 1)
  where
    a = cos(X uv * sin(time/ 10) * 10 + X mouse)
      + sin(Y uv * sin(time / 10) * 10 + Y mouse)
```

#### 1. run hylogen

```
hylogen Main.hs
```

#### 2. play!
Go to [localhost:5678](http://localhost:5678) in your browser.

Changes in `Main.hs` will now be propagated in realtime to your shader!

## inspiration
- [The_Force](https://github.com/shawnlawson/The_Force).

