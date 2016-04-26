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


-- TODO: actually get working..?

variablize :: Expr -> ([(Hash, Expr)], Expr)
variablize expr = variablize' (toHashTree expr) ([], expr)
  where
    variablize' :: HashTree -> ([(Hash, Expr)], Expr) -> ([(Hash, Expr)], Expr)
    variablize' (Leaf h expr) (ctx, last) = ((h, expr):ctx, last)
    variablize' (Branch h expr subTrees) (ctx, last) = (newctx ++ (h, expr):ctx, last)
      where
        newctx = mconcat $ fn <$> subTrees
        fn subTree = fst $ variablize' subTree (ctx, last)

-- toGLSL :: Vec4 -> GLSL
-- toGLSL v = fn top m
--   where
--     top = toExpr v
--     ht = toHashTree top
--     m = hashTreeToCount ht

--     fn :: Expr -> Map.Map Hash (Expr, Int)


-- Now do depth-first traversal of the tree
data GLSL = GLSL [(Hash, Expr)] Expr
instance Show GLSL where
  show (GLSL context fragColor) = fn context ++ "\ngl_FragColor = " ++ show fragColor ++ ";"
    where
      fn xs = mconcat $ showStatement <$> xs
      showStatement (hash, expr) = show (getType expr) <> " var" <> show hash <> " = " <> show expr <> ";\n"

-- State monad?
