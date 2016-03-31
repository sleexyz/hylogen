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

module Hylogen
       ( module Hylogen
       , module Hylogen.Types
       , module Hylogen.Uniforms
       )
       where

import Data.Monoid
import Data.Function

import Hylogen.Types
import Hylogen.Uniforms





testExpr :: Vec1
testExpr = V2 0.1 (sin_ 0.1) & X

testExpr2 :: Vec1
testExpr2 = sin_ 0.1
