{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Hylogen.CSE where

import qualified Data.Map      as Map
import           Data.Monoid

import           Hylogen.Types
import           Control.Monad.State.Lazy

type Id = Int
type Count = Int

-- | Add if in first, variabalize!
type Context = (Map.Map Id Expr, Map.Map Hash (Id, Count))

type GLSL = State Context

addNode:: Hash -> Expr -> GLSL ()
addNode hashish expr = do
  (id2expr, hash2id) <- get
  let newid = case Map.maxViewWithKey id2expr of
                Nothing -> 0
                Just ((k, _), _) -> k + 1

  if Map.member hashish hash2id
    then modify (\(foo, bar) -> ( foo
                                , Map.adjust (\(a, b) -> (a, b+1)) hashish bar
                                ))
    else modify (\(foo, bar) -> ( Map.insert newid expr foo
                                , Map.insert hashish (newid, 1) bar
                                ))

addTree :: HashTree -> GLSL ()
addTree ht = case ht of
  Leaf h e -> addNode h e
  Branch h e subTrees -> do
    addNode h e
    forM_ subTrees addTree

initialContext :: Context
initialContext = (Map.empty, Map.empty)

foobar :: (Expressible a) => a -> Context
foobar x = execState (addTree . toHashTree . toExpr $ x ) initialContext

printFoobar :: (Expressible a) => a -> IO ()
printFoobar x = do
  let (id2expr, hash2id) = foobar x
  forM_ (Map.toList id2expr) $ print


-- hashTreeToCount :: HashTree -> Map.Map Hash (Expr, Int)
-- hashTreeToCount (Leaf h expr) = Map.singleton h (expr, 1)
-- hashTreeToCount (Branch h expr subTrees) = Map.unionsWith fn
--   $ (Map.singleton h (expr, 1)) : (hashTreeToCount <$> subTrees)
--   where
--     fn :: (Expr, Int) -> (Expr, Int) -> (Expr, Int)
--     fn (exprA, a) (exprB, b)
--       | exprA == exprB = (exprA, a + b)
--       | otherwise      = error $ (show exprA) <> "\ndoesn't equal\n" <> (show exprB)


type Name = String
getName :: HashTree -> String
getName (Leaf h _) = show h
getName (Branch h _ _) = show h


variablize:: HashTree -> Expr
variablize a@(Leaf h expr) =  expr
variablize (Branch h expr subTrees) = expr'
  where
    expr' = case expr of
      Uniform ty st -> Uniform ty st
      UnaryOp ty st x -> UnaryOp ty st (f x $ subTrees !! 0)
      UnaryOpPre ty st x -> UnaryOpPre ty st (f x $ subTrees !! 0)
      BinaryOp ty st x y -> BinaryOp ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1)
      BinaryOpPre ty st x y -> BinaryOp ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1)
      TernaryOpPre ty st x y z -> TernaryOpPre ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2)
      QuaternaryOpPre ty st x y z w -> QuaternaryOpPre ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2) (f w $ subTrees !! 3)
      Select ty x y z -> Select ty (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2)
      Access ty st x -> Access ty st (f x $ subTrees !! 0)
      where
        f x h = Uniform (getType x) (getName h)

-- showWithVariable :: HashTree -> String
-- showWithVariable (Leaf h expr) = show h
-- showWithVariable (Branch h expr subTrees) = show $ case expr of
--     Uniform ty st -> Uniform ty st
--     UnaryOp ty st x -> UnaryOp ty st (f x $ subTrees !! 0)
--     UnaryOpPre ty st x -> UnaryOpPre ty st (f x $ subTrees !! 0)
--     BinaryOp ty st x y -> BinaryOp ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1)
--     BinaryOpPre ty st x y -> BinaryOp ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1)
--     TernaryOpPre ty st x y z -> TernaryOpPre ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2)
--     QuaternaryOpPre ty st x y z w -> QuaternaryOpPre ty st (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2) (f w $ subTrees !! 3)
--     Select ty x y z -> Select ty (f x $ subTrees !! 0) (f y $ subTrees !! 1) (f z $ subTrees !! 2)
--     Access ty st x -> Access ty st (f x $ subTrees !! 0)
--   where
--     f x h = Uniform (getType x) (getName h)



-- type Context = (Map.Map Hash (Expr, Int, Bool), [(Hash, Expr)])

-- TODO: actually get working..?

-- variablize :: Expr -> ([(Hash, Expr)], Expr)
-- variablize input= variablize' (toHashTree input) ([], input)
--   where
--     variablize' :: HashTree -> ([(Hash, Expr)], Expr) -> ([(Hash, Expr)], Expr)
--     variablize' (Leaf h expr) (ctx, lastExpr) = ((h, expr):ctx, lastExpr)
--     variablize' (Branch h expr subTrees) (ctx, lastExpr) = (newctx ++ (h, expr):ctx, lastExpr)
--       where
--         newctx = mconcat $ fn <$> subTrees
--         fn subTree = fst $ variablize' subTree (ctx, lastExpr)

-- toGLSL :: Vec4 -> GLSL
-- toGLSL v = fn top m
--   where
--     top = toExpr v
--     ht = toHashTree top
--     m = hashTreeToCount ht

--     fn :: Expr -> Map.Map Hash (Expr, Int)


-- Now do depth-first traversal of the tree

-- data GLSL = GLSL [(Hash, Expr)] Expr
-- instance Show GLSL where
--   show (GLSL context fragColor) = fn context ++ "\ngl_FragColor = " ++ show fragColor ++ ";"
--     where
--       fn xs = mconcat $ showStatement <$> xs
--       showStatement (hash, expr) = show (getType expr) <> " var" <> show hash <> " = " <> show expr <> ";\n"

-- State monad?
