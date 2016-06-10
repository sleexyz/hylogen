# *H Y L O G E N*  [![Hackage Status](https://img.shields.io/hackage/v/hylogen.svg)](https://hackage.haskell.org/package/hylogen) [![Join the chat at https://gitter.im/sleexyz/hylogen](https://badges.gitter.im/sleexyz/hylogen.svg)](https://gitter.im/sleexyz/hylogen?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


![](https://thumbs.gfycat.com/SoftAdeptAlaskajingle-size_restricted.gif)

Hylogen is a purely functional shader language embedded in Haskell that compiles to GLSL. It functions as a powerful alternative to GLSL by leveraging many features of Haskell, including:

**Type inference** - Write more concise code by allowing Haskell to infer the types of your expressions.

- GLSL:      `vec4 foo = vec4(3.0);`
- Hylogen:   `foo = 3`


**Higher-order functions** - Use your standard Haskell goodies: `map`, `foldl`/`foldr`, `$`, `.`,[ `&`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Function.html#v:-38-), etc, or write your own!

**Modules** - Split up complex shaders into multiple files. Write your own high level libraries and `import` them as modules.

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

[**Hylide**](https://github.com/sleexyz/hylide) is a WebGL renderer designed for livecoding shaders with Hylogen, featuring hot-reloading, audio-reactivity and texture backbuffering. However, Hylogen is a general purpose shader language; it can be used anywhere GLSL is used.

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

---
![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)


[**Demo Reel**](https://hylogen.com)

[Changelog](https://github.com/sleexyz/hylogen/CHANGELOG.md) - **NEW:** Hylogen/Hylide split in Hylogen 0.1.4

Hylogen is in alpha! Feature requests, questions, and discussion welcome on [github issues](https://github.com/sleexyz/hylogen/issues)

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

---
![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)



## Install
1. Install the [Haskell Platform](https://www.haskell.org/platform/)
2. `cabal update && cabal install hylogen hylide`

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## Example
Here's a simple Hylogen shader to be used with Hylide, saved as `Example.hs`:

```haskell

module Example where
import Hylogen.WithHylide

output :: Program
output = toProgram color

color :: Vec4
color = vec4 (a, a, a, 1)
  where
    k = 20
    f = (*k) . sin . (/k)
    a = sum [ cos (x_ uvN * f time + x_ mouse )
            , sin (y_ uvN * f time + y_ mouse )
            ]
```

Run Hylide:

```
$ hylide Example.hs
```

Now go to [localhost:5678](http://localhost:5678) in your browser. You'll see a live rendering of the corresponding generated GLSL:

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

Hylide will recompile on changes to the Haskell source, sending generated GLSL to the WebGL client via websockets.




![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. Initial inspiration for Hylogen/Hylide. Live-coding of audio-reactive shaders!
- [data-reify](https://hackage.haskell.org/package/data-reify) by Andy Gill, to keep intermediate AST representations from exploding by preserving the GHC heap's internal sharing

## Links
- [Demo reel](https://hylogen.com)
- [Examples](https://github.com/sleexyz/hylogen-yay)
- [Hackage](https://hackage.haskell.org/package/hylogen)
- [Hylide](https://github.com/sleexyz/hylide)


![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)


Conceived of at the [Recurse Center](https://www.recurse.com/) :)
