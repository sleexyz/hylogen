{-# LANGUAGE DeriveFunctor#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Monoid


class Show a => HasX a
class HasX a => HasY a
class HasY a => HasZ a
class HasZ a => HasW a


data Vec1 where
  Vec1 :: Float -> Vec1
  V1u :: String -> Vec1
  V1uop :: String -> Vec1 -> Vec1
  V1uoppre :: String -> Vec1 -> Vec1
  V1bop :: String -> Vec1 -> Vec1 -> Vec1
  V1boppre :: String -> Vec1 -> Vec1 -> Vec1
  X :: (HasX a) => a -> Vec1
  Y :: (HasY a) => a -> Vec1
  Z :: (HasZ a) => a -> Vec1
  W :: (HasW a) => a -> Vec1

instance Show Vec1 where
  show expr = case expr of
    Vec1 x -> show x
    V1u x -> x
    V1uop u x -> u <> "(" <> show x <> ")"
    V1uoppre u x -> "(" <> u <> show x <> ")"
    V1bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V1boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    X x ->  show x <> ".x"
    Y x ->  show x <> ".y"
    Z x ->  show x <> ".z"
    W x ->  show x <> ".w"


instance Num Vec1 where
  (+) = V1bop "+"
  (*) = V1bop "*"
  negate = V1uoppre "-"
  abs = V1uop "abs"
  signum = V1uop "sign"
  fromInteger = Vec1  . fromInteger


instance Fractional Vec1 where
  (/) = V1bop "/"
  recip = V1bop "/" 1
  fromRational = Vec1 . fromRational

instance Floating Vec1 where
  pi = V1u "pi"
  exp = V1uop "exp"
  log = V1uop "log"
  sqrt = V1uop "sqrt"
  (**) = V1boppre "pow"
  sin = V1uop "sin"
  cos = V1uop "cos"
  tan = V1uop "tan"
  asin = V1uop "asin"
  acos = V1uop "acos"
  atan = V1uop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))


-- | Vec2:

data Vec2 where
  Vec2 :: (Vec1, Vec1) -> Vec2
  V2u :: String -> Vec2

instance Show Vec2 where
  show expr = case expr of
    Vec2 (x, y) -> "vec2(" <> show x <> ", " <> show y <> ")"
    V2u x -> x

instance HasX Vec2
instance HasY Vec2


-- | Vec3:

data Vec3 where
  Vec3 :: (Vec1, Vec1, Vec1) -> Vec3
  V3u :: String -> Vec3

instance Show Vec3 where
  show expr = case expr of
    Vec3 (x, y, z) -> "vec3(" <> show x <> ", " <> show y <> ", " <> show z <> ")"
    V3u x -> x

instance HasX Vec3
instance HasY Vec3
instance HasZ Vec3


-- | Vec4:

data Vec4 where
  Vec4 :: (Vec1, Vec1, Vec1, Vec1) -> Vec4
  V4u :: String -> Vec4

instance Show Vec4 where
  show expr = case expr of
    Vec4 (x, y, z, w) -> "vec4(" <> show x <> ", " <> show y <> ", " <> show z <> ", " <> show w <> ")"
    V4u x -> x

instance HasX Vec4
instance HasY Vec4
instance HasZ Vec4
instance HasW Vec4
