{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Hylogen.Monad where

import qualified Control.Monad.State.Lazy as M
import qualified Control.Monad.Writer.Lazy as M
import qualified Control.Monad.Identity as M

import Hylogen.Types.Vec
import qualified Hylogen.AST.Expr as AST
import qualified Hylogen.AST.Stmt as AST

class Definable a where
  define :: a -> _Hylogen ctx ()


instance Definable (_Hylogen ctx ()) where
  define = undefined

instance (AST.ToGLSLType a, Definable b) => Definable (a -> b) where
  define = undefined




data Effects = Eff CanBreak
data CanBreak = YesBreak | NoBreak

type family AllowBreak x where
  AllowBreak (Ctx (Eff b) i o) = Ctx (Eff YesBreak) i o

data Context i o = Ctx Effects [i] o


data State = State { indentation :: Int
                   , idNumber :: AST.Id
                   }

_defaultState :: State
_defaultState = State { indentation = 0
                      , idNumber = 0
                      }


newtype HylogenInternal (ctx :: Context * *) a
  = HylogenInternal {unHylogenInternal :: M.WriterT AST.CodeBlock (M.StateT State M.Identity) a}
  deriving (Functor, Applicative, Monad, M.MonadState State, M.MonadWriter AST.CodeBlock)


instance (Show a) => Show (HylogenInternal ctx a) where
  show = show . runHylogen


type Hylogen i o = HylogenInternal (Ctx (Eff NoBreak) i  o)

type Main = Hylogen '[] Vec4 Vec4



_emit :: AST.Stmt -> HylogenInternal ctx ()
_emit statement = do
  State{..} <- M.get
  M.tell $ AST.CodeBlock [(indentation, statement)]

_freshVar :: (AST.ToGLSLType a) => a -> HylogenInternal ctx (AST.Expr a)
_freshVar x = do
  s@State {..} <- M.get
  M.put $! s {idNumber=idNumber + 1}
  return . AST.uniform . show $ idNumber

_indentIn :: HylogenInternal ctx ()
_indentIn = M.modify (\s@State{..} -> s {indentation=indentation + 1})

_indentOut :: HylogenInternal ctx ()
_indentOut = M.modify (\s@State{..} -> s {indentation=indentation - 1})


assign :: (AST.ToGLSLType a) => AST.Expr a -> HylogenInternal ctx (AST.Expr a)
assign expr = do
  let ty = AST.getTypeTag expr
  State {..} <- M.get
  _emit $ AST.Assign idNumber (AST.toMono expr)
  _freshVar ty

comment :: String -> HylogenInternal ctx ()
comment = _emit . AST.Comment


for :: Int -> (Vec1 -> HylogenInternal (AllowBreak ctx) ()) -> HylogenInternal ctx ()
for i fn = do
  _branch $ do
    itercount <- _freshVar (FloatVec :: FloatVec 1)
    _emit (AST.ForBegin i (AST.getStringMono . AST.toMono $ itercount))
    _indentIn
    fn itercount
  _emit AST.ForEnd
  return ()

returnWith :: AST.Expr o -> HylogenInternal (Ctx eff i (AST.Expr o)) ()
returnWith = _emit . AST.Return  . AST.toMono

breakFor :: HylogenInternal (Ctx (Eff YesBreak) i o) ()
breakFor = _emit AST.Break

runHylogen :: HylogenInternal ctx a -> AST.CodeBlock
runHylogen = _runHylogen' _defaultState

_runHylogen' :: State -> HylogenInternal ctx a -> AST.CodeBlock
_runHylogen' state h = snd . fst . M.runIdentity $ M.runStateT (M.runWriterT . unHylogenInternal $ h) state


_branch :: HylogenInternal ctx' a -> HylogenInternal ctx ()
_branch toBranch = do
  s <- M.get
  M.tell $ _runHylogen' s toBranch

_test0 :: Main
_test0 = do
  x <- assign $ vec4 (sin 1, 2, 2, 3)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  x <- assign $ x + vec4 (x_ x, 1, 1, 1)
  pure x


_test1 :: Main
_test1 = do
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

_test2 :: Main
_test2 = do
  comment "lol"
  _ <- _test0
  _ <- _test1
  comment "lol"
  return 1


-- test2 :: Hylogen [Vec1, Vec2] Vec2  ()
--   [x, y] <- getVars
--   blah
--   return ()


