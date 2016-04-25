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
hashTreeToCount :: HashTree -> Map.Map Hash (ExprForm, Int)
hashTreeToCount (Leaf h expr) = Map.singleton h (expr, 1)
hashTreeToCount (Branch h expr subTrees) = Map.unionsWith fn
  $ (Map.singleton h (expr, 1)) : (hashTreeToCount <$> subTrees)
  where
    fn :: (ExprForm, Int) -> (ExprForm, Int) -> (ExprForm, Int)
    fn (exprA, a) (exprB, b)
      | exprA == exprB = (exprA, a + b)
      | otherwise      = error $ (show exprA) <> "\ndoesn't equal\n" <> (show exprB)
