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

radius :: Vec2 -> Vec1
radius v2= sqrt((X v2) ** 2 + (Y v2) ** 2)



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
main = run $ Vec4 (r, g, b, 1)
  where
    ty = time * 0.1
    val = tanh(radius uv * sin(time * 1))
    r = val
    g = val * 0.2
    b = val * 0.5
