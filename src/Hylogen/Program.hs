{- |
Internal shader program representation.
-}

module Hylogen.Program where


import Data.Reify
import Data.Monoid
import System.IO.Unsafe

import Hylogen.Expr
import Hylogen.Types.Vec (Vec4)

newtype Id = Id Int
instance Show Id where
  show (Id h) = "_" <> show h

-- | Statement internal representation
--
-- We tag a Statement with a Unique ID and its corresponding untyped expression
data Statement = NewAssign (Unique, ExprMonoF Unique)
               -- MutAssign (Unique, ExprMonoF Unique)

getExpr :: Statement -> ExprMonoF Unique
getExpr (NewAssign (_, expr)) = expr



instance Show Statement where
  show (NewAssign (i, expr@(TreeF (_, ty, _, _) _)))
    = mconcat [ show ty, " ", show . Id $ i, " = ", show . (Id<$>) $  expr, ";"]

-- | GLSL Function internal representation
--
-- A Function is composed of Statements.
newtype Function = Function [Statement]
instance Show Function where
  show (Function xs) = unlines [ "void main() {"
                              , assignments
                              , "    gl_FragColor = _1;"
                              , "}"
                              ]
    where
      assignments = mconcat $  (<> "\n") . ("    "<>) . show <$> reverse xs


-- | Returns a program given an expression in closed untyped form
monoToProgram :: ExprMono -> Function
monoToProgram v = unsafePerformIO $ do
  Graph nodes _ <- reifyGraph v
  return . Function $ NewAssign <$> nodes

-- | A GLSL program. Currently synonym for Function.
type Program = Function

-- | Helper function from a Vec4 to A GLSL Program, with sharing.
toProgram :: Vec4 -> Program
toProgram = monoToProgram . toMono
