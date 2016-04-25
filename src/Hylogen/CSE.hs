{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Hylogen.CSE where

import qualified Data.Map      as Map
import           Data.Monoid

import           Hylogen.Types
-- import           Control.Monad.State.Lazy


-- Recursive solution
hashTreeToCount :: HashTree -> Map.Map Hash (Expr, Int)
hashTreeToCount (Leaf h expr) = Map.singleton h (expr, 1)
hashTreeToCount (Branch h expr subTrees) = Map.unionsWith fn
  $ (Map.singleton h (expr, 1)) : (hashTreeToCount <$> subTrees)
  where
    fn :: (Expr, Int) -> (Expr, Int) -> (Expr, Int)
    fn (exprA, a) (exprB, b)
      | exprA == exprB = (exprA, a + b)
      | otherwise      = error $ (show exprA) <> "\ndoesn't equal\n" <> (show exprB)


-- State monad?
newtype GLSL = GLSL ([(Hash, Expr)], Expr)

