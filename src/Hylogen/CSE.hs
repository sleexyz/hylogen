{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Hylogen.CSE where

import           Data.Hashable
import qualified Data.Map      as Map
import           Data.Monoid
import           GHC.Generics

import           Hylogen.Types


-- Recursive solution
hashTreeToCount :: HashTree -> Map.Map Hash Int
hashTreeToCount (Leaf h) = Map.singleton h 1
hashTreeToCount (Branch h subTrees) = Map.unionsWith (+)
  $ (Map.singleton h 1) : (hashTreeToCount <$> subTrees)




