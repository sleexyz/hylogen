{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase#-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Hylogen.CSE where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap      as IntMap
import           Data.Monoid
import Data.Hashable
import GHC.Generics

import           Hylogen.Types
import           Control.Arrow

type Hash = Int

-- data HashTree a = Leaf Hash a | Branch Hash a [HashTree a]
--   deriving (Generic, Hashable, Show, Eq, Ord, Foldable)

type Tags = (ExprForm, GLSLType, String, Hash, [Either Expr Hash])

type HashTree = Tree (ExprForm, GLSLType, String, Hash, [Either Expr Hash])

getHash :: HashTree -> Hash
getHash (Tree (_, _, _, h, _) _) = h

getExprForm :: HashTree -> ExprForm
getExprForm (Tree (ef, _, _, _, _) _) = ef


toHashTree :: Tree (ExprForm, GLSLType, String) -> Tree (ExprForm, GLSLType, String, Hash, [Either Expr Hash])
toHashTree  (Tree (ef, ty, str)  subtrees) = let
  subHashTrees :: [Tree (ExprForm, GLSLType, String, Hash, [Either Expr Hash])]
  subHashTrees = toHashTree <$> subtrees

  subHashes :: [Hash]
  subHashes = getHash <$> subHashTrees

  parentHash :: Hash
  parentHash = hash (ef, ty, str, subHashes)

  subHashes' :: [Either Expr Hash]
  subHashes' = zipWith fn subHashes subtrees
    where
      fn :: Hash -> Expr -> Either Expr Hash
      fn h expr@(Tree (ef, _, _) _)  = case ef of
        Uniform -> Left expr
        _       -> Right h
      
  in Tree (ef, ty, str, parentHash, subHashes') subHashTrees

-- variablize :: [Hash] -> HashTree -> [Hash] -> HashTree
-- variablize subHashes tree@(Tree (ef, ty, str, h) _) = case ef of
--   Uniform -> tree
--   _       -> tree





type Id = Int
-- | Add if in first, variabalize!
type GLSL = ( IntMap (ExprForm, GLSLType, String, [Either Expr Hash])
            , [(ExprForm, GLSLType, String, Hash, [Either Expr Hash])]
            )

-- TODO:
-- newtype GLSL = GLSL ([(Id, (Expr, [Hash]))], IntMap.Map Hash Id)
--                deriving (Show)


initialGLSL :: GLSL
initialGLSL = (IntMap.empty, [])



-- genContext :: HashTree -> GLSL
-- genContext = foldr fn initialGLSL
--   where
--     fn :: (Hash, Expr, [Hash]) -> GLSL -> GLSL
--     fn (h, e, children) glsl =
--       case e of
--         Uniform _ _ -> glsl
--         _ -> snd $ addNode' h e children glsl


-- TODO: slow

-- HashTree = Tree (ExprForm, GLSLType, String, Hash, [Hash])
toContext :: HashTree -> GLSL
toContext ht = genContext' ht initialGLSL
  where
    genContext' :: HashTree -> GLSL -> GLSL
    genContext' (Tree foo subTrees) glsl = fn foo (foldr genContext' glsl subTrees)
      where
        fn :: (ExprForm, GLSLType, String, Hash, [Either Expr Hash]) -> GLSL -> GLSL
        fn orig@(ef, ty, str, h, hs) (hashmap, output)
          = if IntMap.member h hashmap
            then ( hashmap
                 , output
                 )
            else ( IntMap.insert h (ef, ty, str, hs) hashmap
                 , orig:output
                 )

genContext :: (Expressible a) => a -> GLSL
genContext = toExpr
  >>> toHashTree
  >>> toContext

hash2Name :: Hash -> String
hash2Name h
  | h < 0     = "_n" <> tail shown
  | otherwise = "_" <> shown
    where
      shown = show h




getTopLevel :: GLSL -> Expr
getTopLevel (_, output) = tagsToExpr $ head output

contextToAssignments :: GLSL -> [String]
contextToAssignments (_, output) = foldl fn [] output
  where
    fn bs tags@(ef, _, _, _, _) = case ef of
      Uniform -> bs
      _       -> assign tags : bs
-- contextToAssignments :: GLSL -> [String]
-- contextToAssignments (_, output) = assign <$> reverse output

assign :: (ExprForm, GLSLType, String, Hash, [Either Expr Hash]) -> String
assign tags@(ef, ty, str, h, hs)
  = show ty <> " "
  <> hash2Name h <> " = "
  <> show expr <> ";"
  where
    expr = tagsToExpr tags

-- type Tags = (ExprForm, GLSLType, String, Hash, [Hash])
tagsToExpr :: Tags -> Expr
tagsToExpr (ef, ty, str, h, hs) = case ef of
  _ -> Tree (ef, ty, str) $ fn <$> hs
  where
    fn :: Either Expr Hash -> Expr
    fn (Left e) = e
    fn (Right h) = Tree (Variable, GLSLFloat, hash2Name h) []

