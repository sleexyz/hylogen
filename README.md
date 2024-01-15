<p align="center">
  <img src="./docs/demo.gif"></img>
</p>

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)
<h1 align="center"><i>H Y L O G E N</i></h1>
<p align="center">An embedding of GLSL in Haskell.</p>

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

<h1 align="center"><i>H Y L I D E</i></h1>
<p align="center">A live coding environment for writing shaders with Hylogen.</p>

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

---

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

* **Demo Reel:** [hylogen.com](https://hylogen.com)
* **Hylogen** -  [![Hackage Status](https://img.shields.io/hackage/v/hylogen.svg)](https://hackage.haskell.org/package/hylogen)
* **Hylide** -  [![Hackage Status](https://img.shields.io/hackage/v/hylide.svg)](https://hackage.haskell.org/package/hylide)


![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

---

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)



## Install

Build from source, using [Stack](https://docs.haskellstack.org/en/stable/README/):

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. `git clone https://github.com/sleexyz/hylogen.git`
3. `cd hylogen`
4. `stack build`


This is the recommended method of using Hylogen, because using Stack you can share your projects and be confident that others will be using the same version of Hylogen as you. To install it to your system so it can be used anywhere, not just inside the `hylogen` folder, use `stack install` instead of `stack build`.

Hylogen can also be installed to your system using the [Haskell Platform](https://www.haskell.org/platform/):

1. Install the [Haskell Platform](https://www.haskell.org/platform/)
2. `cabal update && cabal install hylogen hylide`


![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## Example

Here's a simple Hylogen shader to be used with Hylide, saved as `Example.hs`
(available in the `examples/` directory):

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
$ stack exec hylide Example.hs
```

If Hylide was installed using the haskell platform, you can omit `stack exec`:

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

Hylide will recompile on changes to the Haskell source, sending generated GLSL to the WebGL client over websockets.




![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)

## References
- [The_Force](https://github.com/shawnlawson/The_Force) by Shawn Lawson. This was the initial inspiration for Hylide.
- [data-reify](https://hackage.haskell.org/package/data-reify) for type-safe observable sharing.

![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)


Conceived of at the [Recurse Center](https://www.recurse.com/) :)
