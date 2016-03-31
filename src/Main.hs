module Main where

import Hylogen
import Data.Function
import Data.Monoid





rot :: Vec1 -> Vec2 -> Vec2
rot phi a = Vec2 ( sin phi * (a&X)
                   + cos phi * (a&Y)
                 , (-1) * cos phi * (a&X)
                   + sin phi * (a&Y)
                 )

sigmoid :: Vec1 -> Vec1
sigmoid x = recip (1 + exp (negate x))



trippy :: Vec4
trippy = Vec4 (val, val, val, 1)
  where
    phi = atan (X uv / Y uv)
    val = cos (uv & X & trans)
      * sin (uv & Y & trans)
    trans x = x * tan (time / 10) ** 10 * speed
    speed = 1000

main :: IO ()
-- main = run $ trippy
main = run $ Vec4 (0, 0, 0, 1)
  where
    val = sin(time * (uv & Y)) + cos (time * (uv & X))
    r = val * (uv & X)
    g = val
    b = val
