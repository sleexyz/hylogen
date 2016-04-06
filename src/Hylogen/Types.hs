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

module Hylogen.Types where

import Data.Monoid


class Show a => HasX a
class HasX a => HasY a
class HasY a => HasZ a
class HasZ a => HasW a


-- Scalable!
-- class VectorSpace a where
--   copy :: Vec1 -> a
--   (~*) :: Vec1 -> a


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
  V2uop :: String -> Vec2 -> Vec2
  V2uoppre :: String -> Vec2 -> Vec2
  V2bop :: String -> Vec2 -> Vec2 -> Vec2
  V2boppre :: String -> Vec2 -> Vec2 -> Vec2

instance Show Vec2 where
  show expr = case expr of
    Vec2 (x, y) -> "vec2(" <> show x <> ", " <> show y <> ")"
    V2u x -> x
    V2uop u x -> u <> "(" <> show x <> ")"
    V2uoppre u x -> "(" <> u <> show x <> ")"
    V2bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V2boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"

instance HasX Vec2
instance HasY Vec2

instance Num Vec2 where
  (+) = V2bop "+"
  (*) = V2bop "*"
  negate = V2uoppre "-"
  abs = V2uop "abs"
  signum = V2uop "sign"
  fromInteger = (\x -> Vec2 (x, x)) . fromInteger


instance Fractional Vec2 where
  (/) = V2bop "/"
  recip = V2bop "/" 1
  fromRational = (\x -> Vec2 (x, x)) . fromRational

instance Floating Vec2 where
  pi = V2u "pi"
  exp = V2uop "exp"
  log = V2uop "log"
  sqrt = V2uop "sqrt"
  (**) = V2boppre "pow"
  sin = V2uop "sin"
  cos = V2uop "cos"
  tan = V2uop "tan"
  asin = V2uop "asin"
  acos = V2uop "acos"
  atan = V2uop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))


-- | Vec3:

data Vec3 where
  Vec3 :: (Vec1, Vec1, Vec1) -> Vec3
  V3u :: String -> Vec3
  V3uop :: String -> Vec3 -> Vec3
  V3uoppre :: String -> Vec3 -> Vec3
  V3bop :: String -> Vec3 -> Vec3 -> Vec3
  V3boppre :: String -> Vec3 -> Vec3 -> Vec3

instance Show Vec3 where
  show expr = case expr of
    Vec3 (x, y, z) -> "vec3(" <> show x <> ", " <> show y <> ", " <> show z <> ")"
    V3u x -> x
    V3uop u x -> u <> "(" <> show x <> ")"
    V3uoppre u x -> "(" <> u <> show x <> ")"
    V3bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V3boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"

instance HasX Vec3
instance HasY Vec3
instance HasZ Vec3

instance Num Vec3 where
  (+) = V3bop "+"
  (*) = V3bop "*"
  negate = V3uoppre "-"
  abs = V3uop "abs"
  signum = V3uop "sign"
  fromInteger = (\x -> Vec3 (x, x, x)) . fromInteger


instance Fractional Vec3 where
  (/) = V3bop "/"
  recip = V3bop "/" 1
  fromRational = (\x -> Vec3 (x, x, x)) . fromRational

instance Floating Vec3 where
  pi = V3u "pi"
  exp = V3uop "exp"
  log = V3uop "log"
  sqrt = V3uop "sqrt"
  (**) = V3boppre "pow"
  sin = V3uop "sin"
  cos = V3uop "cos"
  tan = V3uop "tan"
  asin = V3uop "asin"
  acos = V3uop "acos"
  atan = V3uop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))


-- | Vec4:

data Vec4 where
  Vec4 :: (Vec1, Vec1, Vec1, Vec1) -> Vec4
  V4u :: String -> Vec4
  V4uop :: String -> Vec4 -> Vec4
  V4uoppre :: String -> Vec4 -> Vec4
  V4bop :: String -> Vec4 -> Vec4 -> Vec4
  V4boppre :: String -> Vec4 -> Vec4 -> Vec4


instance Show Vec4 where
  show expr = case expr of
    Vec4 (x, y, z, w) -> "vec4(" <> show x <> ", " <> show y <> ", " <> show z <> ", " <> show w <> ")"
    V4u x -> x
    V4uop u x -> u <> "(" <> show x <> ")"
    V4uoppre u x -> "(" <> u <> show x <> ")"
    V4bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V4boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"

instance HasX Vec4
instance HasY Vec4
instance HasZ Vec4
instance HasW Vec4

instance Num Vec4 where
  (+) = V4bop "+"
  (*) = V4bop "*"
  negate = V4uoppre "-"
  abs = V4uop "abs"
  signum = V4uop "sign"
  fromInteger = (\x -> Vec4 (x, x, x, x)) . fromInteger


instance Fractional Vec4 where
  (/) = V4bop "/"
  recip = V4bop "/" 1
  fromRational = (\x -> Vec4 (x, x, x, x)) . fromRational

instance Floating Vec4 where
  pi = V4u "pi"
  exp = V4uop "exp"
  log = V4uop "log"
  sqrt = V4uop "sqrt"
  (**) = V4boppre "pow"
  sin = V4uop "sin"
  cos = V4uop "cos"
  tan = V4uop "tan"
  asin = V4uop "asin"
  acos = V4uop "acos"
  atan = V4uop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))
