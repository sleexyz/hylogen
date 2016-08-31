{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hylogen.AST.Program where

import Hylogen.AST.Expr
import Hylogen.AST.Statement

newtype FunctionId = FunctionId Int
  deriving Num

instance Show FunctionId where
  show x = "__" ++ show x

newtype Program = Program [Function]
  deriving (Monoid, Show)


newtype Function = Function CodeBlock
  deriving (Monoid, Show)


