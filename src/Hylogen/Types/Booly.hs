{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hylogen.Types.Booly where

import Hylogen.Expr

-- | Booly singleton type tag
data BoolyType = BoolyType
instance ToGLSLType BoolyType where
  toGLSLType _ = GLSLBool
  tag = BoolyType

type Booly = Expr BoolyType

-- | We use Num operators for Boolean arithmetic:
instance Num Booly where
  -- | Or
  (+) = op2 "||"
  -- | And
  (*) = op2 "&&"
  negate = op1 "!"
  abs = id
  signum = id
  fromInteger x
    | x > 0 = uniform "true"
    | otherwise = uniform "false"
