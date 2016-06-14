{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Identity

import qualified Hylogen as Hy
import qualified Hylogen.Expr as AST

-- TODO: Also implement as a RoseTree
-- import qualified Hylogen.Program as AST -- DEPRECATE

data Stmt = Assign (Id, AST.ExprMono)
               | Comment String
instance Show Stmt where
  show (Assign (i, expr)) = mconcat [ show . AST.getTypeTagMono $ expr
                                    , " "
                                    , showId i
                                    , " = "
                                    , show expr
                                    , ";"
                                    ]
  show (Comment str) = "// " ++ str

type Id = Int

showId :: Id -> String
showId x = "_" <> show x


newtype CodeBlock =  CodeBlock [Stmt]
  deriving (Monoid)

instance Show CodeBlock where
  show (CodeBlock xs) = unlines $ show <$> xs


newtype Hylogen a = Hylogen {unHylogen :: WriterT CodeBlock (StateT Id Identity) a}
  deriving (Functor, Applicative, Monad, MonadState Id, MonadWriter CodeBlock)




-- | Writes a statement to the context
emit :: Stmt -> Hylogen ()
emit = tell . CodeBlock . return

freshVar :: (AST.ToGLSLType a) => a -> Hylogen (AST.Expr a)
freshVar x = do
  s <-  get
  put $! s + 1
  return . AST.uniform . show $ s

assign :: (AST.ToGLSLType a) => AST.Expr a -> Hylogen (AST.Expr a)
assign expr = do
  let ty = AST.getTypeTag expr
  s <- get
  emit (Assign (s, AST.toMono expr))
  freshVar ty

runHylogen :: Hylogen a -> CodeBlock
runHylogen x = snd . fst . runIdentity $ runStateT (runWriterT . unHylogen $ x) 0

test :: Hylogen Hy.Vec4
test = do
  replicateM_ 10 (assign $ Hy.vec4 (1, 1, 1, 1))
  x <- assign $ Hy.vec4 (1, 1, 1, 1)
  return x

test2 :: Hylogen ()
test2 = do
  emit $ Comment "hello"
  void test
  emit $ Comment "world"



