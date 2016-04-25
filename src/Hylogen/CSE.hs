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

-- Now do depth-first traversal of the tree
data GLSL = GLSL [(Hash, Expr)] Expr
instance Show GLSL where
  show (GLSL context fragColor) = fn context ++ "\ngl_FragColor = " ++ show fragColor ++ ";"
    where
      fn xs = mconcat $ showStatement <$> xs
      showStatement (hash, expr) = show (getType expr) <> " var" <> show hash <> " = " <> show expr <> ";\n"
