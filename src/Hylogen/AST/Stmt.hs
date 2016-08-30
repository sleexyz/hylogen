{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hylogen.AST.Stmt where

import Hylogen.AST.Expr
import Hylogen.Types.Booly
import Data.Reify
import Data.Monoid

newtype CodeBlock =  CodeBlock [(Int, Stmt)]
  deriving (Monoid)

instance Show CodeBlock where
  show (CodeBlock xs) =
    unlines $ (\(n, statement) -> indent n ++ show statement) <$> xs
    where
      indent n = mconcat $ replicate (n*2) " "

data Stmt = Assign Id ExprMono
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

instance Show Stmt where
  show (Assign i expr) = mconcat [ show . getTypeTagMono $ expr
                                    , " "
                                    , show i
                                    , " = "
                                    , show expr
                                    , ";"
                                    ]
  show (Comment str) = "// " <> str
  show (ForBegin i str ) = mconcat [ "for (int "
                                  , str
                                  , " = 0; i < "
                                  , show i
                                  , "; i++) {"
                                  ]
  show (ForEnd) = "}"
  show (Return expr) = "return " <> show expr <> ";"
  show (Break) = "break;"


