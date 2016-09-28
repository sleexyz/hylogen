{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Hylogen.AST.Program where

import Hylogen.AST.Expr
import Hylogen.AST.Statement
import Data.List
import Data.Monoid
import Data.Function

newtype FunctionId = FunctionId Int
  deriving Num

instance Show FunctionId where
  show (FunctionId x) = "__" ++ show x

newtype Program = Program [TopLevel]
  deriving (Monoid)
instance Show Program where
  show (Program topLevels) =
    (show <$> topLevels)
    & intersperse "\n"
    & mconcat

data TopLevel =
  TLFunction Function
  | TLConstant () -- fixme: implement
instance Show TopLevel where
  show = \case
    TLFunction f -> show f
    TLConstant x -> show x

data Function = Function
  { _name :: String
  , _inputs :: [ExprMono]
  , _output :: GLSLType
  , _code :: CodeBlock
  }
instance Show Function where
  show Function{..} =
    show _output
    <> " "
    <> _name
    <> "("
    <> showInputs _inputs
    <> ")"
    <> "\n{\n"
    <> show _code
    <> "\n}"


showInputs :: [ExprMono] -> String
showInputs =
  mconcat
  . intersperse ", "
  . fmap (\i -> show (getTypeTagMono i) <> " " <> show i)
