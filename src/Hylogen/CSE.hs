{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.CSE where

import qualified Data.Map      as Map
import           Data.Monoid

import           Hylogen.Types
import           Control.Arrow

type Id = Int
type Count = Int

-- | Add if in first, variabalize!
newtype GLSL = GLSL (Map.Map Id (Expr, [Hash]), Map.Map Hash (Id, Count))
               deriving (Show)


initialGLSL :: GLSL
initialGLSL = GLSL (Map.empty, Map.empty)



genContext :: HashTree -> GLSL
genContext = foldr fn initialGLSL
  where
    fn :: (Hash, Expr, [Hash]) -> GLSL -> GLSL
    fn (h, e, children) glsl =
      case e of
        Uniform _ _ -> glsl
        _ -> snd $ addNode' h e children glsl


addNode' :: Hash -> Expr -> [Hash] -> GLSL -> (Id, GLSL)
addNode' hashish expr children glsl =
  let (GLSL (id2expr, hash2id)) = glsl
      newid = case Map.maxViewWithKey id2expr of
                Nothing -> 0
                Just ((k, _), _) -> k + 1
  in
    if Map.member hashish hash2id
    then ( fst $ hash2id Map.! hashish
         , GLSL ( id2expr
                , Map.adjust (\(a, b) -> (a, b+1)) hashish hash2id
                )
         )
    else ( newid
         , GLSL ( Map.insert newid (expr, children) id2expr
                , Map.insert hashish (newid, 1) hash2id
                )
         )

getName :: Hash -> GLSL -> String
getName h (GLSL (_, hash2id)) = "_" <> show (fst $ hash2id Map.! h)

variablize :: Expr -> [Hash] -> GLSL -> Expr
variablize expr hashes glsl =
  let
    GLSL (_, hash2id) = glsl

    f :: Expr -> Hash -> Expr
    f x h = if Map.member h hash2id
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
  = foldr fn [] (Map.toList id2expr)
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
