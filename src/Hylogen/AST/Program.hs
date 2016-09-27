{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hylogen.AST.Program where

import Hylogen.AST.Expr
import Hylogen.AST.Statement

newtype FunctionId = FunctionId Int
  deriving Num

instance Show FunctionId where
  show (FunctionId x) = "__" ++ show x

newtype Program = Program [Function]
  deriving (Monoid, Show)

data Function = Function
  { _name :: String
  , _inputs :: [GLSLType]
  , _output :: GLSLType
  , _code :: CodeBlock
  }
  deriving (Show)
