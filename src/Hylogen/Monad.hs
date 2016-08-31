{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Hylogen.Monad ( Hylogen
                     , Main
                     , Ref , deref , ref
                     , mut
                     , for
                     -- , returnWith
                     , breakFor
                     ) where

import qualified Control.Monad.State.Lazy as M
import qualified Control.Monad.Writer.Lazy as M
import qualified Control.Monad.Identity as M

import Hylogen.Types.Vec
import qualified Hylogen.AST.Expr as AST
import qualified Hylogen.AST.Statement as AST
import qualified Hylogen.AST.Program as AST

import Data.Monoid

class Definable a where
  define :: Hylogen a -> Hylogen a


instance (AST.ToGLSLType a) => Definable (AST.Expr a) where
  define x = do
    let (program, codeblock)= _runHylogen x
    M.tell ( program  <> AST.Program [AST.Function codeblock]
           , AST.CodeBlock []
           )
    return $ AST.funApp "hello"

    -- TODO: return actual function call

-- instance (AST.ToGLSLType a, Definable b) => Definable (a -> b) where
--   define = undefined




data Effects = Eff CanBreak
data CanBreak = YesBreak | NoBreak

type family AllowBreak x where
  AllowBreak (Ctx (Eff b)) = Ctx (Eff YesBreak)

data Context = Ctx Effects


data State = State { _indent :: Int
                   , _id :: AST.Id
                   , _fid :: AST.FunctionId
                   }

_defaultState :: State
_defaultState = State { _indent = 0
                      , _id = 0
                      , _fid = 0
                      }


type Output = (AST.Program, AST.CodeBlock)

newtype HylogenInternal (ctx :: Context) a
  = HylogenInternal { unHylogenInternal :: M.WriterT Output (M.StateT State M.Identity) a}
  deriving (Functor, Applicative, Monad, M.MonadState State, M.MonadWriter Output)


instance (Show a) => Show (HylogenInternal ctx a) where
  show = show . _runHylogen


type Hylogen = HylogenInternal (Ctx (Eff NoBreak))




_emit :: AST.Statement -> HylogenInternal ctx ()
_emit statement = do
  State{..} <- M.get
  M.tell $ (mempty , AST.CodeBlock [(_indent , statement)])

_freshVar :: (AST.ToGLSLType a) => a -> HylogenInternal ctx (AST.Expr a)
_freshVar x = do
  s@State {..} <- M.get
  M.put $! s {_id=_id + 1}
  let var = AST.variable $ show _id
  return var

_freshFuncRef:: (AST.ToGLSLType a) => a -> HylogenInternal ctx (AST.Expr a)
_freshFuncRef x = do
  s@State {..} <- M.get
  M.put $! s {_fid=_fid+ 1}
  let var = AST.funApp $ show _fid
  return var

_indentIn :: HylogenInternal ctx ()
_indentIn = M.modify (\s@State{..} -> s {_indent=_indent+ 1})

_indentOut :: HylogenInternal ctx ()
_indentOut = M.modify (\s@State{..} -> s {_indent=_indent- 1})

data Ref a = Ref AST.Id (AST.Expr a)
deref :: (AST.ToGLSLType a) => Ref a -> AST.Expr a
deref (Ref _ expr) = expr

instance (AST.ToGLSLType a) => Show (Ref a) where
  show  = show . deref

ref :: (AST.ToGLSLType a) => AST.Expr a -> HylogenInternal ctx (Ref a)
ref expr = do
  State {..} <- M.get
  _freshVar (AST.getTypeTag expr)
  _emit $ AST.NewRef _id (AST.toMono expr)
  return $ Ref _id (AST.variable (show _id))

mut :: (AST.ToGLSLType a) => Ref a -> AST.Expr a -> HylogenInternal ctx ()
mut (Ref idNumber _) expr = do
  _emit $ AST.Mutate idNumber (AST.toMono expr)

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

-- returnWith :: AST.Expr o -> HylogenInternal (Ctx eff i (AST.Expr o)) ()
-- returnWith = _emit . AST.Return  . AST.toMono

breakFor :: HylogenInternal (Ctx (Eff YesBreak)) ()
breakFor = _emit AST.Break


-- | Runs the state monad with the default state
_runHylogen :: HylogenInternal ctx a -> Output
_runHylogen = _runHylogen' _defaultState

-- | Given a state, runs the state monad
_runHylogen' :: State -> HylogenInternal ctx a -> Output
_runHylogen' state h = snd . fst . M.runIdentity $ M.runStateT (M.runWriterT . unHylogenInternal $ h) state


_branch :: HylogenInternal ctx' a -> HylogenInternal ctx ()
_branch toBranch = do
  s <- M.get
  M.tell $ _runHylogen' s toBranch


newtype Main = Main (Hylogen Vec4)

instance Show Main where
  show (Main hylo) = unlines [ show program
                             , "void main() {"
                             , show codeblock
                             , "}"
                             ]
    where
      (program, codeblock) = _runHylogen hylo


test = Main $ do

  y <- define $ do
    x <- ref (100 :: Vec1)
    return (deref x) :: Hylogen Vec1
  x <- ref $ y
  x <- ref $ y

  z <- ref (0 :: Vec1)
  return 10

