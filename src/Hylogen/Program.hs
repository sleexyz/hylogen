module Hylogen.Program where

import Data.Reify
import Data.Monoid
import System.IO.Unsafe

import Hylogen.Expr

-- Just for printing!
newtype Id = Id Int
instance Show Id where
  show (Id h) = "_" <> show h

data Statement = NewAssign (Unique, ExprMonoF Unique)
               -- | MutAssign (Unique, ExprMonoF Unique)

getExpr :: Statement -> ExprMonoF Unique
getExpr (NewAssign (_, expr)) = expr



instance Show Statement where
  show (NewAssign (i, expr@(TreeF (_, ty, _, _) _)))
    = mconcat [ show ty, " ", show . Id $ i, " = ", show . (Id<$>) $  expr, ";"]

newtype Function = Function [Statement]
instance Show Function where
  show (Function xs) = unlines [ "void main() {"
                              , assignments
                              , "    gl_FragColor = _1;"
                              , "}"
                              ]
    where
      assignments = mconcat $  (<> "\n") . ("    "<>) . show <$> reverse xs


toProgram :: ExprMono -> Function
toProgram v = unsafePerformIO $ do
  Graph nodes _ <- reifyGraph v
  return . Function $ NewAssign <$> nodes
