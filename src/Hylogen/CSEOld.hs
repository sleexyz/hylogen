
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase#-}

module Hylogen.CSE where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap      as IntMap
import           Data.Monoid
import Data.Hashable

import           Hylogen.Types
import           Control.Arrow

type Hash = Int
type Tags = ( ExprForm,               -- AST node form (eg. BinaryOp)
              GLSLType,               -- GLSL type (eg. vec3)
              String,                 -- String tag (eg. "sin")
              Hash,                   -- Hash of the node
              [Either Expr Hash]      -- Hash of children iff they are to be variablized
            )
type HashTree = Tree Tags


getHash :: HashTree -> Hash
getHash (Tree (_, _, _, h, _) _) = h

getExprForm :: HashTree -> ExprForm
getExprForm (Tree (ef, _, _, _, _) _) = ef


toHashTree :: Tree (ExprForm, GLSLType, String) -> HashTree
toHashTree  (Tree (ef, ty, str)  subtrees) = let
  subHashTrees :: [HashTree]
  subHashTrees = toHashTree <$> subtrees

  subHashes :: [Hash]
  subHashes = getHash <$> subHashTrees

  parentHash :: Hash
  parentHash = hash (ef, ty, str, subHashes)

  subHashes' :: [Either Expr Hash]
  subHashes' = zipWith fn subHashes subtrees
    where
      fn :: Hash -> Expr -> Either Expr Hash
      fn h expr@(Tree (ef', _, _) _)  = case ef' of
        Uniform -> Left expr
        _       -> Right h
  in Tree (ef, ty, str, parentHash, subHashes') subHashTrees






type Id = Int
-- | Add if in first, variabalize!
type GLSL = ( IntMap (ExprForm, GLSLType, String, [Either Expr Hash])
            , [(ExprForm, GLSLType, String, Hash, [Either Expr Hash])]
            )


initialGLSL :: GLSL
initialGLSL = (IntMap.empty, [])

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

assign :: (ExprForm, GLSLType, String, Hash, [Either Expr Hash]) -> String
assign tags@(_, ty, _, h, _)
  = show ty <> " "
  <> hash2Name h <> " = "
  <> show expr <> ";"
  where
    expr = tagsToExpr tags

-- type Tags = (ExprForm, GLSLType, String, Hash, [Hash])
tagsToExpr :: Tags -> Expr
tagsToExpr (ef, ty, str, _, hs) = Tree (ef, ty, str) $ fn <$> hs
  where
    fn :: Either Expr Hash -> Expr
    fn (Left expr) = expr
    fn (Right h') = Tree (Variable, GLSLFloat, hash2Name h') []
