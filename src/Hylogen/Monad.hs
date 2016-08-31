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
    let (p, cb)= _runHylogen x
    M.tell (p  <> AST.Program [AST.Function cb], AST.CodeBlock [])

    -- TODO: return function call
    x

-- instance (AST.ToGLSLType a, Definable b) => Definable (a -> b) where
--   define = undefined




data Effects = Eff CanBreak
data CanBreak = YesBreak | NoBreak

type family AllowBreak x where
  AllowBreak (Ctx (Eff b)) = Ctx (Eff YesBreak)

data Context = Ctx Effects


data State = State { indentation :: Int
                   , idNumber :: AST.Id
                   }

_defaultState :: State
_defaultState = State { indentation = 0
                      , idNumber = 0
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
  M.tell $ (mempty , AST.CodeBlock [(indentation, statement)])

_freshVar :: (AST.ToGLSLType a) => a -> HylogenInternal ctx (AST.Expr a)
_freshVar x = do
  s@State {..} <- M.get
  M.put $! s {idNumber=idNumber + 1}
  let var = AST.variable $ show idNumber
  return var

_indentIn :: HylogenInternal ctx ()
_indentIn = M.modify (\s@State{..} -> s {indentation=indentation + 1})

_indentOut :: HylogenInternal ctx ()
_indentOut = M.modify (\s@State{..} -> s {indentation=indentation - 1})

data Ref a = Ref AST.Id (AST.Expr a)
deref :: (AST.ToGLSLType a) => Ref a -> AST.Expr a
deref (Ref _ expr) = expr

instance (AST.ToGLSLType a) => Show (Ref a) where
  show  = show . deref

ref :: (AST.ToGLSLType a) => AST.Expr a -> HylogenInternal ctx (Ref a)
ref expr = do
  let ty = AST.getTypeTag expr
  State {..} <- M.get
  _emit $ AST.NewRef idNumber (AST.toMono expr)
  return $ Ref idNumber (AST.variable (show idNumber))

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

  define $ do
    x <- ref (100 :: Vec1)
    return (deref x) :: Hylogen Vec1

  x <- ref (0 :: Vec1)
  return 10

