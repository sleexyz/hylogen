{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Types
import Data.VectorSpace


inverseSqrt :: (HyloPrim a) => a -> a
inverseSqrt = vuop "inversesqrt"

fract :: (HyloPrim a) => a -> a
fract = vuop "fract"

floor_ :: (HyloPrim a) => a -> a
floor_ = vuop "floor"

ceil_ :: (HyloPrim a) => a -> a
ceil_ = vuop "ceil"

min_ :: (HyloPrim a) => a -> a -> a
min_ = vboppre "min"

max_:: (HyloPrim a) => a -> a -> a
max_ = vboppre "max"

clamp :: (HyloPrim a) => a -> a -> a -> a
clamp x y z = (z `min_` y) `max_` x


linexp :: (Floating a) => (a, a, a, a) -> a -> a
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Floating a) => (a, a, a, a) -> a -> a
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))


time :: Vec1
time = V1u "time"

uv :: Vec2
uv = V2u "uv()"

uvN :: Vec2
uvN = V2u "uvN"

resolution :: Vec2
resolution = V2u "resolution"

mouse :: Vec2
mouse = V2u "mouse"


audio :: Vec4
audio = V4u "audio"

backBuffer :: Texture
backBuffer = Tu "backBuffer"

mix :: Vec1 -> Vec4 -> Vec4 -> Vec4
mix p a b = p *^ a + (1 - p) *^ b

-- | Booly's

true :: Booly
true = Bu "true"

false :: Booly
false = Bu "false"

eq :: (HyloPrim v) => v -> v -> Booly
eq = Bcomp "=="

neq :: (HyloPrim v) => v -> v -> Booly
neq = Bcomp "!="

lt :: (HyloPrim v) => v -> v -> Booly
lt = Bcomp "<"

gt :: (HyloPrim v) => v -> v -> Booly
gt = Bcomp ">"

leq :: (HyloPrim v) => v -> v -> Booly
leq = Bcomp "<="

geq :: (HyloPrim v) => v -> v -> Booly
geq = Bcomp ">="
