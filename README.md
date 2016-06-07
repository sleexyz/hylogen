# [*H Y L O G E N*](https://hylogen.com) 

[![Hackage Status](https://img.shields.io/hackage/v/hylogen.svg)](https://hackage.haskell.org/package/hylogen)
[![Join the chat at https://gitter.im/sleexyz/hylogen](https://badges.gitter.im/sleexyz/hylogen.svg)](https://gitter.im/sleexyz/hylogen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

![](https://thumbs.gfycat.com/SoftAdeptAlaskajingle-size_restricted.gif)

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

Hylogen is a purely functional language [embedded in Haskell](https://wiki.haskell.org/Embedded_domain_specific_language) for live-coding fragment shaders, featuring:

- simple and pure syntax
- standard operators (`+`, `*`, [`*^`,  `<.>`](https://hackage.haskell.org/package/vector-space))
- compat. w/ your fav haskell goodies (higher-order functions, ADTS, swanky polymorphism).
- compiles to GLSL

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

It comes with `hylide`, a live WebGL renderer with:
- hot-reloading
- audio-reactive primitives
- texture backbuffering

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)


## Install
```
cabal update
cabal install hylogen
```

This will install the Hylogen package and hylide, the live renderer.

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## Usage
Here's a simple Hylogen program, saved as `Example.hs`:

```haskell

module Example where
import Hylogen.WithHylide

color :: Vec4
color = vec4 (a, a, a, 1)
  where
    k = 20
    f = (*k) . sin . (/k)
    a = sum [ cos (x_ uvN * f time + x_ mouse )
            , sin (y_ uvN * f time + y_ mouse )
            ]

output :: Program
output = toProgram color
```

Run hylide:

```
$ hylide Main.hs
```

Now go to [localhost:5678](http://localhost:5678) in your browser. You'll see a live rendering of the generated GLSL:

```GLSL
void main() {
    float _7 = uvN.x;
    float _10 = (time / 20.0);
    float _9 = sin(_10);
    float _8 = (_9 * 20.0);
    float _6 = (_7 * _8);
    float _11 = mouse.x;
    float _5 = (_6 + _11);
    float _4 = cos(_5);
    float _3 = (0.0 + _4);
    float _15 = uvN.y;
    float _18 = (time / 20.0);
    float _17 = sin(_18);
    float _16 = (_17 * 20.0);
    float _14 = (_15 * _16);
    float _19 = mouse.y;
    float _13 = (_14 + _19);
    float _12 = sin(_13);
    float _2 = (_3 + _12);
    vec4 _1 = vec4(_2, _2, _2, 1.0);

    gl_FragColor = _1;
}
```

Hylide will recompile on file changes, sending fresh shaders to the WebGL context via websockets.




![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. Live-coding audio-reactive shaders!
- [data-reify](https://hackage.haskell.org/package/data-reify) by Andy Gill, to keep intermediate AST representations from exploding by preserving the GHC heap's internal sharing

## Resources
- [demo reel](https://hylogen.com)
- [examples](https://github.com/sleexyz/hylogen-yay)
- [hackage](https://hackage.haskell.org/package/hylogen)
- [hylide](https://github.com/sleexyz/hylide)


