{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Hylogen.Globals where

import Hylogen.Types
import Hylogen.Expr


-- Geometric functions

len :: forall n. (Veccable n) => Vec n -> Vec1
len = op1pre "length"

distance :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
distance = op2pre'' "distance"

cross :: Vec3 -> Vec3 -> Vec3
cross = op2pre'' "cross"

normalize :: forall n. (Veccable n) => Vec n -> Vec n
normalize = op1pre'' "normalize"

faceForward :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
faceForward = op3pre'' "faceforward"

reflect :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
reflect = op2pre'' "reflect"

refract :: forall n. (Veccable n) => Vec n -> Vec n -> Vec1 -> Vec n
refract = op3pre "refract"


inverseSqrt :: forall n. (Veccable n) => Vec n -> Vec n
inverseSqrt = op1pre'' "inversesqrt"

fract :: forall n. (Veccable n) => Vec n -> Vec n
fract = op1pre'' "fract"

mod_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
mod_ = op2pre'' "mod"

floor_:: forall n. (Veccable n) => Vec n -> Vec n
floor_ = op1pre'' "floor"

ceil_ :: forall n. (Veccable n) => Vec n -> Vec n
ceil_ = op1pre'' "ceil"

min_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
min_ = op2pre'' "min"

max_ :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n
max_ = op2pre'' "max"

clamp :: forall n. (Veccable n) => Vec n -> Vec n -> Vec n -> Vec n
clamp x y z = (z `min_` y) `max_` x


linexp :: (Floating a) => (a, a, a, a) -> a -> a
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Floating a) => (a, a, a, a) -> a -> a
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))



mix :: (Veccable n) => Vec1 -> Vec n -> Vec n -> Vec n
mix p x y = op3pre "mix" x y p
-- mix p x y = x ^* (1 - p) + y ^* p

true :: Booly
true = uniform "true"

false :: Booly
false = uniform "false"

bcomp :: (Veccable v) => String -> Vec v -> Vec v -> Booly
bcomp str x y = product $ zipWith (op2' str) (toList x) (toList y)


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

texture2D :: Texture -> Vec2 -> Vec4
texture2D = op2pre "texture2D"

sel :: forall a
          . (ToGLSLType a)
          => Booly -> Expr a -> Expr a -> Expr a
sel a b c = Expr t (Tree (Select, toGLSLType t, "") [toMono a, toMono b, toMono c])
  where t = tag :: a
