{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hylogen.Globals where

import Hylogen.Types
import Data.VectorSpace
import GHC.TypeLits




inverseSqrt :: forall n. (Veccable n) => Vec n -> Vec n
inverseSqrt = op1'' "inversesqrt"

fract :: forall n. (Veccable n) => Vec n -> Vec n
fract = op1'' "fract"

floor_:: forall n. (Veccable n) => Vec n -> Vec n
floor_ = op1'' "fract"

ceil_ :: forall n. (Veccable n) => Vec n -> Vec n
ceil_ = op1'' "ceil"

min_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
min_ = op2pre'' "min"

max_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
max_ = op2pre'' "max"

clamp :: forall n. (Veccable n) => (Vec n, Vec n) -> Vec n -> Vec n
clamp (x, y) z = (z `min_` y) `max_` x


linexp :: (Floating a) => (a, a, a, a) -> a -> a
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Floating a) => (a, a, a, a) -> a -> a
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))


time :: Vec1
time = uniform "time"

-- TODO: flip these definitions! Normalized means ??
uv :: Vec2
uv = uniform "uv()"

uvN :: Vec2
uvN = uniform "uvN"

resolution :: Vec2
resolution = uniform "resolution"

mouse :: Vec2
mouse = uniform "mouse"


audio :: Vec4
audio = uniform "audio"

backBuffer :: Texture
backBuffer = uniform "backBuffer"

channel1 :: Texture
channel1 = uniform "channel1"

mix :: Vec1 -> Vec4 -> Vec4 -> Vec4
mix p a b = p *^ a + (1 - p) *^ b

true :: Booly
true = uniform "true"

false :: Booly
false = uniform "false"

eq :: (Veccable v) => Vec v -> Vec v -> Booly
eq = op2' "=="

neq :: (Veccable v) => Vec v -> Vec v -> Booly
neq = op2' "!="

lt :: (Veccable v) => Vec v -> Vec v -> Booly
lt = op2' "<"

gt :: (Veccable v) => Vec v -> Vec v -> Booly
gt = op2' ">"

leq :: (Veccable v) => Vec v -> Vec v -> Booly
leq = op2' "<="

geq :: (Veccable v) => Vec v -> Vec v -> Booly
geq = op2' ">="

texture2D :: Texture -> Vec2 -> Vec4
texture2D = op2pre "texture2D"

select :: forall a
          . (ToGLSLType a)
          => Booly -> Expr a -> Expr a -> Expr a
select a b c = Expr t (Tree (Select, toGLSLType t, "") ([toMono a, toMono b, toMono c]))
  where t = tag :: a
