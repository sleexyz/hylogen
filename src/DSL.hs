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


-- class HasShape (n :: Nat) a
-- instance HasShape 2 Vec2

data Vec = V1 | V2 | V3


data DSL a where
  Get :: String -> (Vec -> a) -> DSL a
  Set :: String -> Vec -> a -> DSL a

instance Functor DSL where
  fmap f (Get name k) = Get name (f . k)
  fmap f (Set name value next) = Set name value (f next)
