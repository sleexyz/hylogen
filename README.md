# [*H Y L O G E N*](http://hylogen.com)

Hylogen is a tiny language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding visuals.

## setup
```
cabal update
cabal install ghcid hylogen
```
## use

```haskell
-- ./Main.hs
module Main where

import Hylogen


main = writeFile "./shader.glsl" . toGLSL $ Vec4 (a, a, a, 1)
  where
    a = cos(X uv * sin(time/ 10) * 10 + X mouse)
      + sin(Y uv * sin(time / 10) * 10 + Y mouse)
```
Open up [hylogen.com](http://hylogen.com) in your browser.

In a terminal:
```
ghcid Main.hs -T main
```

In another terminal:
```
hylogen shader.glsl
```



Hooray! Now, whatever changes you make to `Main.hs` will be reflected live in your realtime visuals!

## inspiration
- [The_Force](https://github.com/shawnlawson/The_Force).

