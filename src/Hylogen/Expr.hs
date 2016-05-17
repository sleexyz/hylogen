{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


module Hylogen.Expr where

import Data.Hashable
import GHC.Generics
import GHC.TypeLits
import Data.Reify
import Data.VectorSpace

data GLSLType = GLSLFloat
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLBool
              | GLSLTexture
              deriving (Generic, Hashable, Eq, Ord)

data ExprForm = Uniform
              | Variable
              | Op1
              | Op1Pre
              | Op2
              | Op2Pre
              | Op3Pre
              | Op4Pre
              | Select
              | Access
                deriving (Show, Generic, Hashable)

data Tree a  = Tree { getElem     :: a
                    , getChildren :: [Tree a]
                    }

type ExprMono = Tree (ExprForm, GLSLType, String)

instance Show ExprMono where
  show (Tree (form, _, str) xs) = case form of
    Uniform  -> str
    Variable -> str
    Op1      -> mconcat ["(", str, show (xs!!0), ")"]
    Op1Pre   -> mconcat [ str, "(", show (xs!!0), ")"]
    Op2      -> mconcat ["(", show (xs !! 0), " ", str, " ", show (xs !! 1), ")"]
    Op2Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ")"]
    Op3Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ")"]
    Op4Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ", ", show (xs!!3), ")"]
    Select   -> mconcat ["( ", show (xs!!0), " ? ", show (xs!!1), " : ", show (xs!!2), ")"]
    Access   -> mconcat [show (xs!!0), ".", str]

-- The GLSLType needs to be manually dependent

-- light typed wrapper
data Expr ty = Expr { getTypeTag :: ty
                    , toMono ::  Tree (ExprForm, GLSLType, String)
                    }

instance ToGLSLType ty => Show (Expr ty) where
  show = show . toMono

class ToGLSLType  ty where
  toGLSLType :: ty -> GLSLType
