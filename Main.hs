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

-- angle :: Vec2 -> Vec1
-- angle v2 = atan ()

radius :: Vec2 -> Vec1
radius v2 = sqrt(X v2 ** 2 + Y v2 ** 2)

linexp :: (Vec1, Vec1, Vec1, Vec1) -> Vec1 -> Vec1
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Vec1, Vec1, Vec1, Vec1) -> Vec1 -> Vec1
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))



concentric :: Vec4
concentric = Vec4 (r, g, b, 1)
  where
    val = fract(radius uv * 10 + time)
    r = val
    g = val * 0.2
    b = val * 0.5

world :: Vec4
world = Vec4 (r, g, b, 1)
  where
    gap = 10
    m = sin(time * 0.1) & linexp (-1, 1, 10e1, 10e10)
    ratemul = 0.5
    val   = cos(radius uv * m + time + sin(time + gap) * ratemul)
    val'  = cos(radius uv * m + time + sin(time + gap ** 2) * ratemul)
    val'' = cos(radius uv * m + time + sin(time + gap ** 3) * ratemul)
    r = val ** 2
    g = val' ** 2
    b = val'' ** 2

main :: IO ()
main = run $ world

