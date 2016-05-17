{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hylogen.Booly where

import Hylogen.Expr
import Hylogen.Vec

data BoolyType = BoolyType
instance ToGLSLType BoolyType where
  toGLSLType _ = GLSLBool

type Booly = Expr BoolyType

bu :: String -> Booly
bu str = Expr BoolyType (Tree (Uniform, toGLSLType BoolyType, str) [])

bop1 :: String -> Booly -> Booly
bop1 str x = Expr BoolyType (Tree (Op1, toGLSLType BoolyType, str) [toMono x])

bop1pre :: String -> Booly -> Booly
bop1pre str x = Expr BoolyType (Tree (Op1Pre, toGLSLType BoolyType, str) [toMono x])

bop2 :: String -> Booly -> Booly -> Booly
bop2 str x y = Expr BoolyType (Tree (Op2, toGLSLType BoolyType, str) [toMono x, toMono y])

bcomp :: (Veccable n) => String -> Vec n -> Vec n-> Booly
bcomp str x y = product $ zipWith (bcomp_ str) (toList x) (toList y)

bcomp_ :: String -> Vec1 -> Vec1 -> Booly
bcomp_ str x y = Expr BoolyType (Tree (Op2, toGLSLType BoolyType, str) [toMono x, toMono y])

instance Num Booly where
  (+) = bop2 "||"
  (*) = bop2 "&&"
  negate = bop1 "!"
  abs = id
  signum = id
  fromInteger x
    | x > 0 = bu "true"
    | otherwise = bu "false"
