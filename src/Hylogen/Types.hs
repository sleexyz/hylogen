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

data Vec1 where
  V1 :: Float -> Vec1
  V1u :: String -> Vec1
  V1uop :: (ToVec1 a) => String -> a -> Vec1
  X :: (HasX a) => a -> Vec1
  Y :: (HasY a) => a -> Vec1
  Z :: (HasZ a) => a -> Vec1
  W :: (HasW a) => a -> Vec1

instance Show Vec1 where
  show expr = case expr of
    V1 x -> show x
    V1u x -> x
    V1uop u x -> u <> "(" <> show x <> ")"
    X x ->  show x <> ".x"
    Y x ->  show x <> ".y"
    Z x ->  show x <> ".z"
    W x ->  show x <> ".w"


class Show a => ToVec1 a where toV1 :: a -> Vec1
instance {-# INCOHERENT #-} (a ~ Float) => ToVec1 a where toV1 = V1
instance ToVec1 Vec1     where toV1 = id




-- | Vec2:

data Vec2 where
  V2 :: (ToVec1 a, ToVec1 b) => a -> b -> Vec2
  V2u :: String -> Vec2

instance Show Vec2 where
  show expr = case expr of
    V2 x y -> "vec2(" <> show x <> ", " <> show y <> ")"
    V2u x -> x

instance HasX Vec2
instance HasY Vec2


-- | Vec3:

data Vec3 where
  V3 :: (ToVec1 a, ToVec1 b, ToVec1 c) => a -> b -> c -> Vec3
  V3u :: String -> Vec3

instance Show Vec3 where
  show expr = case expr of
    V3 x y z -> "vec3(" <> show x <> ", " <> show y <> ", " <> show z <> ")"
    V3u x -> x

instance HasX Vec3
instance HasY Vec3
instance HasZ Vec3


-- | Vec4:

data Vec4 where
  V4 :: (ToVec1 a, ToVec1 b, ToVec1 c, ToVec1 d) => a -> b -> c -> d -> Vec4
  V4u :: String -> Vec4

instance Show Vec4 where
  show expr = case expr of
    V4 x y z w -> "vec4(" <> show x <> ", " <> show y <> ", " <> show z <> ", " <> show w <> ")"
    V4u x -> x

instance HasX Vec4
instance HasY Vec4
instance HasZ Vec4
instance HasW Vec4


-- TODO: Swizzle!
