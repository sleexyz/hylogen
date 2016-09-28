{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hylogen.AST.Statement where

import Hylogen.AST.Expr
import Hylogen.Types.Booly
import Data.Monoid
import Data.List
import Data.Function

newtype CodeBlock =  CodeBlock [(Int, Statement)]
  deriving (Monoid)

instance Show CodeBlock where
  show (CodeBlock xs) =
    ((\(n, statement) -> indent n ++ show statement) <$> xs)
    & intersperse "\n"
    & mconcat
    where
      indent n = mconcat $ replicate (n*2) " "

data Statement =
  NewRef Id ExprMono
  | Mutate Id ExprMono
  | IfThenElse ExprMono ExprMono ExprMono
  | ForBegin Int String
  | ForEnd
  | Return ExprMono
  | Break
  | Comment String

newtype Id = Id Int
  deriving (Num)

instance Show Id where
  show (Id x) = "_" <> show x

instance Show Statement where
  show (Mutate i expr) = mconcat
    [ show i
    , " = "
    , show expr
    , ";"
    ]
  show (NewRef i expr) = mconcat
    [ show . getTypeTagMono $ expr
    , " "
    , show i
    , " = "
    , show expr
    , ";"
    ]
  show (Comment str) = "// " <> str
  show (ForBegin i str ) = mconcat
    [ "for (int "
    , str
    , " = 0; i < "
    , show i
    , "; i++) {"
    ]
  show (ForEnd) = "}"
  show (Return expr) = "return " <> show expr <> ";"
  show (Break) = "break;"
