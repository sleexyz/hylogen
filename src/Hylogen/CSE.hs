{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.CSE where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap      as IntMap
import           Data.Monoid

import           Hylogen.Types
import           Control.Arrow


type Id = Int
-- | Add if in first, variabalize!
newtype GLSL = GLSL (IntMap (Expr, [Hash]), IntMap Id)
               deriving (Show)

-- TODO:
-- newtype GLSL = GLSL ([(Id, (Expr, [Hash]))], IntMap.Map Hash Id)
--                deriving (Show)


initialGLSL :: GLSL
initialGLSL = GLSL (IntMap.empty, IntMap.empty)



-- genContext :: HashTree -> GLSL
-- genContext = foldr fn initialGLSL
--   where
--     fn :: (Hash, Expr, [Hash]) -> GLSL -> GLSL
--     fn (h, e, children) glsl =
--       case e of
--         Uniform _ _ -> glsl
--         _ -> snd $ addNode' h e children glsl


-- TODO: slow

genContext :: HashTree -> GLSL
genContext ht = genContext' ht initialGLSL
  where
    genContext' :: HashTree -> GLSL -> GLSL
    genContext' (Leaf _) glsl = glsl
    genContext' (Branch (h, e, hs) subTrees) glsl = fn (h, e, hs) (foldr genContext' glsl subTrees)
      where
        fn :: (Hash, Expr, [Hash]) -> GLSL -> GLSL
        fn (h, e, children) glslAfterChildren@(GLSL (_, hash2id))
          = if IntMap.member h hash2id
            then glsl -- short circuit the branch
            else snd $ addNode' h e children glslAfterChildren


addNode' :: Hash -> Expr -> [Hash] -> GLSL -> (Id, GLSL)
addNode' hashish expr children glsl =
  let (GLSL (id2expr, hash2id)) = glsl
      newid = case IntMap.maxViewWithKey id2expr of
                Nothing -> 0
                Just ((k, _), _) -> k + 1
  in ( newid
     , GLSL ( IntMap.insert newid (expr, children) id2expr
            , IntMap.insert hashish newid hash2id
            )
     )

getName :: Hash -> GLSL -> String
getName h (GLSL (_, hash2id)) = "_" <> show (hash2id IntMap.! h)

variablize :: Expr -> [Hash] -> GLSL -> Expr
variablize expr hashes glsl =
  let
    GLSL (_, hash2id) = glsl

    f :: Expr -> Hash -> Expr
    f x h = if IntMap.member h hash2id
            then Uniform (getType x) (getName h glsl)
            else x
  in case expr of
  Uniform ty st
    -> Uniform ty st
  UnaryOp ty st x
    -> UnaryOp ty st
    ( f x (hashes !! 0))
  UnaryOpPre ty st x
    -> UnaryOpPre ty st
    ( f x (hashes !! 0))
  BinaryOp ty st x y
    -> BinaryOp ty st
    ( f x (hashes !! 0))
    ( f y (hashes !! 1))
  BinaryOpPre ty st x y
    -> BinaryOpPre ty st
    ( f x (hashes !! 0))
    ( f y (hashes !! 1))
  TernaryOpPre ty st x y z
    -> TernaryOpPre ty st
    ( f x (hashes !! 0))
    ( f y (hashes !! 1))
    ( f z (hashes !! 2))
  QuaternaryOpPre ty st x y z w
    -> QuaternaryOpPre ty st
    ( f x (hashes !! 0))
    ( f y (hashes !! 1))
    ( f z (hashes !! 2))
    ( f w (hashes !! 3))
  Select ty x y z
    -> Select ty
    ( f x (hashes !! 0))
    ( f y (hashes !! 1))
    ( f z (hashes !! 2))
  Access ty st x
    -> Access ty st
    ( f x (hashes !! 0))

type GLSL' = [(Id, Expr)]

replaceWithVariables :: GLSL -> GLSL'
replaceWithVariables glsl@(GLSL (id2expr, _))
  = foldr fn [] (IntMap.toList id2expr)
  where
    fn :: (Id, (Expr, [Hash])) -> GLSL' -> GLSL'
    fn (i, (e, hashes)) xs = (i, (variablize e hashes glsl)) : xs

foo :: GLSL -> String
foo (GLSL (id2expr, _))= foldl (\b a ->b <>"\n" <> show a) "" id2expr

genGLSL :: (Expressible a) => a -> GLSL'
genGLSL = toExpr
  >>> toHashTree
  >>> genContext
  >>> replaceWithVariables


getTopLevel :: GLSL' -> Expr
getTopLevel id2expr = fn (last id2expr)
  where
    fn (i, e) = Uniform (getType e) ("_" <> show i)

glslToAssignments:: GLSL' -> [String]
glslToAssignments glsl' = assign <$> glsl'
  where
    assign :: (Id, Expr) -> String
    assign (i, e) = show (getType e) <> " " <> "_" <> show i <> " = " <> show e <> ";"
