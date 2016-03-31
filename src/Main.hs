module Main where

import Hylogen
import Data.Function
import Data.Monoid





testExpr :: Vec1
testExpr = Vec2 0.1 (sin 0.1) & X

testExpr2 :: Vec1
testExpr2 = sin 0.1

rot :: Vec1 -> Vec2 -> Vec2
rot phi a = Vec2 (sin phi * (a&X) + cos phi * (a & Y)) ((-1) * cos phi * (a & X) + sin phi * (a & Y))


main :: IO ()
main = run $
  Vec4 val val val 1
  where

    phi = atan (X uv / Y uv)
    val = cos (uv & X & trans)
      * sin (uv & Y & trans)
    trans x = x * tan (time / 10) ** 10 * speed
    speed = 100
