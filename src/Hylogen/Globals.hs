{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hylogen.Globals where

import Hylogen.Types
import Data.VectorSpace
import GHC.TypeLits




inverseSqrt = vop1'' "inversesqrt"
fract = vop1'' "fract"
floor_ = vop1'' "fract"
ceil_ = vop1'' "ceil"
min_ = vop2pre'' "min"
max_ = vop2pre'' "max"
clamp x y z = (z `min_` y) `max_` x


linexp :: (Floating a) => (a, a, a, a) -> a -> a
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Floating a) => (a, a, a, a) -> a -> a
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))


time :: Vec1
time = vu "time"

-- TODO: flip these definitions! Normalized means ??
uv :: Vec2
uv = vu "uv()"

uvN :: Vec2
uvN = vu "uvN"

resolution :: Vec2
resolution = vu "resolution"

mouse :: Vec2
mouse = vu "mouse"


audio :: Vec4
audio = vu "audio"

-- backBuffer :: Texture
-- backBuffer = Tu "backBuffer"

-- channel1 :: Texture
-- channel1 = Tu "channel1"

mix :: Vec1 -> Vec4 -> Vec4 -> Vec4
mix p a b = p *^ a + (1 - p) *^ b

-- -- | Booly's

true :: Booly
true = bu "true"

false :: Booly
false = bu "false"

eq :: (Veccable v) => Vec v -> Vec v -> Booly
eq = bcomp "=="

neq :: (Veccable v) => Vec v -> Vec v -> Booly
neq = bcomp "!="

lt :: (Veccable v) => Vec v -> Vec v -> Booly
lt = bcomp "<"

gt :: (Veccable v) => Vec v -> Vec v -> Booly
gt = bcomp ">"

leq :: (Veccable v) => Vec v -> Vec v -> Booly
leq = bcomp "<="

geq :: (Veccable v) => Vec v -> Vec v -> Booly
geq = bcomp ">="


-- texture2D :: Texture -> Vec2 -> Vec4
-- texture2D = Texture2D
