{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Hylogen.Monad where

import qualified Control.Monad.State.Lazy as M
import qualified Control.Monad.Writer.Lazy as M
import qualified Control.Monad.Identity as M

import Hylogen
import Hylogen.Types.Vec (FloatVec)
import qualified Hylogen.AST.Expr as AST
import qualified Hylogen.AST.Stmt as AST

-- import qualified Hylogen.Program as AST -- DEPRECATE

class Definable a where
  define :: a -> Hylogen' ctx ()


instance Definable (Hylogen' ctx ()) where
  define = undefined

instance (AST.ToGLSLType a, Definable b) => Definable (a -> b) where
  define = undefined




data Effects = Eff CanBreak
data CanBreak = YesBreak | NoBreak

type family AllowBreak x where
  AllowBreak (Ctx (Eff b) i o) = Ctx (Eff YesBreak) i o

data Context i o = Ctx Effects [i] o

newtype Hylogen' (ctx :: Context * *) a
  = Hylogen' {unHylogen' :: M.WriterT AST.CodeBlock (M.StateT AST.Id M.Identity) a}
  deriving (Functor, Applicative, Monad, M.MonadState AST.Id, M.MonadWriter AST.CodeBlock)


instance (Show a) => Show (Hylogen' ctx a) where
  show = show . runHylogen


type Hylogen i o = Hylogen' (Ctx (Eff NoBreak) i  o)

type Main = Hylogen '[] Vec4 Vec4





-- | Writes a statement to the context
emit :: AST.Stmt -> Hylogen' ctx ()
emit = M.tell . AST.CodeBlock . return

freshVar :: (AST.ToGLSLType a) => a -> Hylogen' ctx (AST.Expr a)
freshVar x = do
  s <- M.get
  M.put $! s + 1
  return . AST.uniform . show $ s

-- public
assign :: (AST.ToGLSLType a) => AST.Expr a -> Hylogen' ctx (AST.Expr a)
assign expr = do
  let ty = AST.getTypeTag expr
  s <- M.get
  emit (AST.Assign s (AST.toMono expr))
  freshVar ty

-- public
comment :: String -> Hylogen' ctx ()
comment str = emit (AST.Comment str)

-- instance IsString (Hylogen)


-- TODO: allow break

for :: Int -> (Vec1 -> Hylogen' (AllowBreak ctx) ()) -> Hylogen' ctx ()
for i fn = do
  itercount <- freshVar (FloatVec :: FloatVec 1)
  emit (AST.ForBegin i (AST.getStringMono . AST.toMono $ itercount))
  Hylogen' . unHylogen' $ fn itercount
  emit AST.ForEnd
  return ()

-- public
returnWith :: AST.Expr o -> Hylogen' (Ctx eff i (AST.Expr o)) ()
returnWith = emit . AST.Return  . AST.toMono 

-- public
breakFor :: Hylogen' (Ctx (Eff YesBreak) i o) ()
breakFor = emit AST.Break

runHylogen :: Hylogen' ctx a -> AST.CodeBlock
runHylogen h = snd . fst . M.runIdentity $ M.runStateT (M.runWriterT . unHylogen' $ h) 0

test0 :: Main
test0 = do
  x <- assign $ vec4 (sin 1, 2, 2, 3)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  pure x

test1 :: Main
test1 = do
  for 10 (\i -> do
             x <- assign $ vec4 (i, 1, 1, 1)
             x <- assign $ vec4 (i, 1, 1, 1)
             x <- assign $ vec4 (i, 1, 1, 1)
             returnWith 1
             return ()
         )
  for 100 (\i -> do
             comment "lol"
             return ()
         )
  return 1

test2 :: Main
test2 = do
  comment "lol"
  _ <- test0
  _ <- test1
  comment "lol"
  return 1


-- test2 :: Hylogen [Vec1, Vec2] Vec2  ()
--   [x, y] <- getVars
--   blah
--   return ()


