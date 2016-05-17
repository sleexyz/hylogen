{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hylogen.Booly where

import Hylogen.Expr
import Hylogen.Vec

data BoolyType = BoolyType
instance ToGLSLType BoolyType where
  toGLSLType _ = GLSLBool
  tag = BoolyType

type Booly = Expr BoolyType

instance Num Booly where
  (+) = op2 "||"
  (*) = op2 "&&"
  negate = op1 "!"
  abs = id
  signum = id
  fromInteger x
    | x > 0 = uniform "true"
    | otherwise = uniform "false"
