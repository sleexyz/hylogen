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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hylogen.Monad (
  Hylogen,
  Main,
  Ref, deref, ref,
  mut,
  for,
  define,
  spec,
  breakFor,
  ) where

import qualified Control.Monad.State.Lazy as M
import qualified Control.Monad.Writer.Lazy as M
import qualified Control.Monad.Identity as M

import Hylogen.Types.Vec
import Hylogen.AST.Expr
import Hylogen.AST.Statement
import Hylogen.AST.Program

import Data.Monoid
import Data.Proxy

import Test.Hspec

-- | Define a function.
-- This writes a GLSL function (via eta-abstraction)
-- and constructs a function to be used in Hylogen
define :: (Definable a) => Hylogen a -> Hylogen a
define hyFun = do
  s@State {_fid} <- M.get
  M.put $! s {_fid=_fid + 1}

  let initializeState :: [GLSLType] -> State
      initializeState inputs = defaultState {_id = Id $ length inputs}

      (inputTypes, outputType) = buildGLSLFunction hyFun
      incomingState = initializeState inputTypes

      inputs :: [ExprMono]
      inputs = map _ inputTypes

      fed = _ inputs hyFun

      ((out, (program, codeblock)), outState) = runHylogenWithState incomingState fed

      glslFunction = Function
        { _name = (show _fid)
        , _inputs = inputTypes
        , _output = outputType
        , _code = codeblock
        }

  M.tell ( program  <> Program [glslFunction]
         , CodeBlock []
         )
  return $ buildFun (show _fid) [] (Proxy :: Proxy a)


class Definable a where
  buildGLSLFunction :: Hylogen a -> ([GLSLType], GLSLType)
  buildFun :: String -> [ExprMono]-> Proxy a -> a

instance (ToGLSLType a) => Definable (Expr a) where
  buildGLSLFunction _ = ([], toGLSLType (tag :: a))
  buildFun name args _ =
    Expr t (Tree (FunApp, toGLSLType t, name) args)
    where
      t = tag :: a

instance (ToGLSLType a, Definable b) => Definable (Expr a -> b) where
  buildGLSLFunction f = (t:inputs, output)
    where
      t = toGLSLType (tag :: a)
      (inputs, output) = buildGLSLFunction (f <*> undefined)

  buildFun name args _ =
    \a -> buildFun name (_mono a : args) (Proxy :: Proxy b)

data Effects = Eff CanBreak
data CanBreak = YesBreak | NoBreak

type family AllowBreak x where
  AllowBreak (Ctx (Eff b)) = Ctx (Eff YesBreak)

data Context = Ctx Effects


data State = State
  { _indent :: Int
  , _id :: Id
  , _fid :: FunctionId
  }

defaultState :: State
defaultState = State
  { _indent = 0
  , _id = 0
  , _fid = 0
  }


type Output = (Program, CodeBlock)

newtype HylogenInternal (ctx :: Context) a
  = HylogenInternal { unHylogenInternal :: M.WriterT Output (M.StateT State M.Identity) a}
  deriving (Functor, Applicative, Monad, M.MonadState State, M.MonadWriter Output)

instance (ToGLSLType a) => Show (HylogenInternal ctx (Expr a)) where
  show = show . snd . fst . runHylogenDefault . returnLast

type Hylogen = HylogenInternal (Ctx (Eff NoBreak))


emit :: Statement -> HylogenInternal ctx ()
emit statement = do
  State{..} <- M.get
  M.tell $ (mempty , CodeBlock [(_indent , statement)])

freshVar :: (ToGLSLType a) => a -> HylogenInternal ctx (Expr a)
freshVar x = do
  s@State {_id} <- M.get
  M.put $! s {_id=_id + 1}
  let var = variable $ show _id
  return var

indentIn :: HylogenInternal ctx ()
indentIn = M.modify (\s@State{..} -> s {_indent=_indent+ 1})

indentOut :: HylogenInternal ctx ()
indentOut = M.modify (\s@State{..} -> s {_indent=_indent- 1})

data Ref a = Ref Id (Expr a)
deref :: (ToGLSLType a) => Ref a -> Expr a
deref (Ref _ expr) = expr

instance (ToGLSLType a) => Show (Ref a) where
  show  = show . deref

ref :: (ToGLSLType a) => Expr a -> HylogenInternal ctx (Ref a)
ref expr = do
  State {..} <- M.get
  freshVar (_tag expr)
  emit $ NewRef _id (_mono expr)
  return $ Ref _id (variable (show _id))

mut :: (ToGLSLType a) => Ref a -> Expr a -> HylogenInternal ctx ()
mut (Ref idNumber _) expr = do
  emit $ Mutate idNumber (_mono expr)

comment :: String -> HylogenInternal ctx ()
comment = emit . Comment


for :: Int -> (Vec1 -> HylogenInternal (AllowBreak ctx) ()) -> HylogenInternal ctx ()
for i fn = do
  branch_ $ do
    itercount <- freshVar (FloatVec :: FloatVec 1)
    emit (ForBegin i (getStringMono . _mono $ itercount))
    indentIn
    fn itercount
  emit ForEnd
  return ()

ret :: ToGLSLType o => Expr o -> HylogenInternal ctx ()
ret = emit . Return  . toMono

returnLast :: ToGLSLType o => HylogenInternal ctx (Expr o) -> HylogenInternal ctx ()
returnLast =  (>>=ret)

breakFor :: HylogenInternal (Ctx (Eff YesBreak)) ()
breakFor = emit Break


-- | Runs the state monad with the default state
runHylogenDefault :: HylogenInternal ctx a -> ((a, Output), State)
runHylogenDefault = runHylogenWithState defaultState

-- | Given a state, runs the state monad
runHylogenWithState :: State -> HylogenInternal ctx a -> ((a, Output), State)
runHylogenWithState state h = M.runIdentity $ M.runStateT (M.runWriterT . unHylogenInternal $ h) state

branch_ :: HylogenInternal ctx' a -> HylogenInternal ctx ()
branch_ toBranch = do
  s <- M.get
  M.tell . snd . fst $ runHylogenWithState s toBranch


newtype Main = Main (Hylogen Vec4)

instance Show Main where
  show (Main hylo) = unlines
    [ show program
    , "void main() {"
    , show codeblock
    , "}"
    ]
    where
      ((program, codeblock), _) = runHylogenDefault hylo

test = Main $ do
  y <- ref (100 :: Vec2)
  foo <- define $ do
    x <- ref (100 :: Vec1)
    return (deref x) :: Hylogen Vec1
  x <- ref . deref $  y
  x <- ref . deref $  y

  z <- ref (0 :: Vec1)
  return 10


spec  = do
  describe "works" $ do
    it "works" $ do
      print $ do
        fun <- define (return (2 :: Vec2))
        x <- ref $fun + fun
        return (deref x)
