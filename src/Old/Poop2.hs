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

module Hylogen where

import GHC.TypeLits
import Free (Free(In, Pure), liftF)
import Data.Monoid


cata :: forall f a. (Functor f) => (f a -> a) -> Free f a -> a
cata _ (Pure x) = x
cata alg (In x) = alg $ (cata alg) <$>  x

class IsExpr f where
  showAlg :: f String -> String


data V1UOp = Sin
instance Show V1UOp where
  show Sin = "sin"


data Vec1 a where
  V1 :: Float -> Vec1 a
  V1u :: String -> Vec1 a
  V1uop :: V1UOp -> Vec1 a
deriving instance Functor Vec1

instance IsExpr Vec1 where
  showAlg  expr = case expr of
    V1 x -> show x
    V1u x -> x
    V1uop b x -> show b <> "(" <> x <> ")"


data Vec2 a where
  V2 :: Vec1 a -> Vec1 a -> Vec2 a
  V2u :: String -> Vec2 a
deriving instance Functor Vec2

instance IsExpr Vec2 where
  showAlg  expr = case expr of
    V2 x y -> "vec2(" <> showAlg x <> ", " <> showAlg y <> ")"
    V2u x-> x

testExpr = In $ V2 (V1 0.1) (V1 0.5)
