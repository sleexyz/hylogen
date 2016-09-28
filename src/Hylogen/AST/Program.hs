{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hylogen.AST.Program where

import Hylogen.AST.Expr
import Hylogen.AST.Statement

newtype FunctionId = FunctionId Int
  deriving Num

instance Show FunctionId where
  show (FunctionId x) = "__" ++ show x

newtype Program = Program [TopLevel]
  deriving (Monoid, Show)

data TopLevel =
  TLFunction Function
  | TLConstant () -- fixme: implement
  deriving Show

data Function = Function
  { _name :: String
  , _inputs :: [GLSLType]
  , _output :: GLSLType
  , _code :: CodeBlock
  }
  deriving (Show)
