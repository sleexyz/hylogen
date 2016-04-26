{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Hylogen.CSE where

import qualified Data.Map      as Map
import           Data.Monoid

import           Hylogen.Types
import           Control.Monad.State.Lazy
import           Data.List

type Id = Int
type Count = Int

-- | Add if in first, variabalize!
newtype GLSL = GLSL (Map.Map Id Expr, Map.Map Hash (Id, Count))

getTopLevel :: GLSL -> Expr
getTopLevel (GLSL (id2expr, _)) = case Map.maxViewWithKey id2expr of
  Nothing -> error "must have top level?"
  Just ((k, e), _) -> Uniform (getType e) ("_" <> show k)



type GLSLState = State GLSL

addNode:: Hash -> Expr -> GLSLState Id
addNode hashish expr = do
  GLSL (id2expr, hash2id) <- get
  let newid = case Map.maxViewWithKey id2expr of
                Nothing -> 0
                Just ((k, _), _) -> k + 1

  if Map.member hashish hash2id
    then do
         modify (\(GLSL (foo, bar)) -> GLSL ( foo
                                                  , Map.adjust (\(a, b) -> (a, b+1)) hashish bar
                                                  ))
         return $ fst $ hash2id Map.! hashish
    else do
         modify (\(GLSL (foo, bar)) -> GLSL ( Map.insert newid expr foo
                                                , Map.insert hashish (newid, 1) bar
                                                ))
         return $ newid

addTree :: HashTree -> GLSLState ()
addTree ht = case ht of
  Leaf h e -> do
    _ <- addNode h e
    return ()
  Branch h e subTrees -> do
    -- | post-order traversal guarantees topological ordering!
    forM_ subTrees addTree
    i <- addNode h e
    newExpr <- variablize e subTrees
    modify (\(GLSL (foo, bar)) -> GLSL ( Map.adjust (const newExpr) i foo
                                             , bar ))

genGLSL :: (Expressible a) => a -> GLSL
genGLSL x = execState (addTree . toHashTree . toExpr $ x ) initialGLSL
  where
    initialGLSL :: GLSL
    initialGLSL = GLSL (Map.empty, Map.empty)

glslToAssignments:: GLSL -> [String]
glslToAssignments glsl = do
  let (GLSL (id2expr, _)) = glsl
  fmap assign $ Map.toList id2expr
  where
    assign :: (Id, Expr) -> String
    assign (i, e) = show (getType e) <> " " <> "_" <> show i <> " = " <> show e <> ";"


getName :: HashTree -> GLSLState String
getName ht = do
  let h = case ht of
            Leaf h _ -> h
            Branch h _ _ -> h
  GLSL (_, hash2id) <- get
  return $ "_" <> show (fst $ hash2id Map.! h)

variablize :: Expr -> [HashTree] -> GLSLState Expr
variablize expr subTrees = case expr of
  Uniform ty st
    -> return $ Uniform ty st
  UnaryOp ty st x
    -> UnaryOp ty st
    <$> f x (subTrees !! 0)
  UnaryOpPre ty st x
    -> UnaryOpPre ty st
    <$> f x (subTrees !! 0)
  BinaryOp ty st x y
    -> BinaryOp ty st
    <$> f x (subTrees !! 0)
    <*> f y (subTrees !! 1)
  BinaryOpPre ty st x y
    -> BinaryOpPre ty st
    <$> f x (subTrees !! 0)
    <*> f y (subTrees !! 1)
  TernaryOpPre ty st x y z
    -> TernaryOpPre ty st
    <$> f x (subTrees !! 0)
    <*> f y (subTrees !! 1)
    <*> f z (subTrees !! 2)
  QuaternaryOpPre ty st x y z w
    -> QuaternaryOpPre ty st
    <$> f x (subTrees !! 0)
    <*> f y (subTrees !! 1)
    <*> f z (subTrees !! 2)
    <*> f w (subTrees !! 3)
  Select ty x y z
    -> Select ty
    <$> f x (subTrees !! 0)
    <*> f y (subTrees !! 1)
    <*> f z (subTrees !! 2)
  Access ty st x
    -> Access ty st
    <$> f x (subTrees !! 0)
  where
    f x h = Uniform (getType x) <$> (getName h)
