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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

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
-- This emits a GLSL function definition (via eta-abstraction)
-- and constructs a function to be used in Hylogen
define :: (Definable a) => Hylogen a -> Hylogen a
define hyFun = do
  s@State {_fid} <- M.get
  M.put $! s { _fid=_fid + 1}
  let (program, glslFunction) = makeGLSLFunction hyFun _fid
  M.tell $
    Emitted
    (program  <> Program [TLFunction glslFunction])
    (CodeBlock [])
  return $ buildHyFunOut (show _fid) [] (Proxy :: Proxy a)

makeGLSLFunction :: forall a. (Definable a) => Hylogen a -> FunctionId -> (Program, Function)
makeGLSLFunction hyFun fid =
  let
    inputTypes :: [GLSLType]
    outputType :: GLSLType
    (inputTypes, outputType) = buildGLSLFunction hyFun

    inputs :: [ExprMono]
    inputs = zipWith f inputTypes [0..]
      where
        f ty i = Tree (Variable, ty, show (Id i)) []

    hyFunApplied :: Hylogen (DefineOutput a)
    hyFunApplied = applyHyFunIn hyFun inputs

    hyFunReturned :: Hylogen ()
    hyFunReturned = returnLast hyFunApplied


    initializeState :: [GLSLType] -> State
    initializeState inputs = defaultState
      { _id = Id $ length inputs
      , _indent = 1
      }

    outState :: State
    out :: ()
    program :: Program
    codeblock :: CodeBlock
    ((out, Emitted program codeblock), outState) =
      runHylogen hyFunReturned (initializeState inputTypes)
  in
    ( program
    , Function
      { _name = (show fid)
      , _inputs = inputs
      , _output = outputType
      , _code = codeblock
      }
    )


class (IsExpr (DefineOutput a)) => Definable a where
  type DefineOutput a
  buildGLSLFunction :: Hylogen a -> ([GLSLType], GLSLType)
  applyHyFunIn :: Hylogen a -> [ExprMono] -> Hylogen (DefineOutput a)
  buildHyFunOut :: String -> [ExprMono]-> Proxy a -> a

instance (ToGLSLType a) => Definable (Expr a) where
  type DefineOutput (Expr a) = Expr a
  buildGLSLFunction _ = ([], toGLSLType (tag :: a))
  applyHyFunIn hyFun = \case
    _:_ -> error "unexpected"
    [] -> hyFun
  buildHyFunOut name args _ =
    Expr t (Tree (FunApp, toGLSLType t, name) args)
    where
      t = tag :: a

instance (ToGLSLType a, Definable b) => Definable (Expr a -> b) where
  type DefineOutput (Expr a -> b) = DefineOutput b
  buildGLSLFunction f = (t:inputs, output)
    where
      t = toGLSLType (tag :: a)
      (inputs, output) = buildGLSLFunction (f <*> undefined)
  applyHyFunIn hyFun = \case
    [] -> error "unexpected"
    x:xs ->
      applyHyFunIn (hyFun <*> return (Expr (tag :: a) x)) xs
  buildHyFunOut name args _ = \a ->
    buildHyFunOut name (_mono a : args) (Proxy :: Proxy b)

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


data Emitted = Emitted
  { _emittedProgram :: Program
  , _emittedCodeBlock :: CodeBlock
  }
instance Monoid Emitted where
  mempty = Emitted mempty mempty
  mappend (Emitted p c) (Emitted p' c') =
    Emitted (mappend p p') (mappend c c')

newtype HylogenInternal (ctx :: Context) a
  = HylogenInternal { unHylogenInternal :: M.WriterT Emitted (M.StateT State M.Identity) a}
  deriving (Functor, Applicative, Monad, M.MonadState State, M.MonadWriter Emitted)

instance (ToGLSLType a) => Show (HylogenInternal ctx (Expr a)) where
  show hylo = show program
    where
      state = defaultState
      (((), Emitted program codeblock), _) =
        runHylogen (returnLast hylo) state

type Hylogen = HylogenInternal (Ctx (Eff NoBreak))


emit :: Statement -> HylogenInternal ctx ()
emit statement = do
  State{..} <- M.get
  M.tell $ Emitted mempty (CodeBlock [(_indent , statement)])

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

ret :: IsExpr a => a -> HylogenInternal ctx ()
ret = emit . Return  . toMono

returnLast :: IsExpr a => HylogenInternal ctx a -> HylogenInternal ctx ()
returnLast =  (>>=ret)

breakFor :: HylogenInternal (Ctx (Eff YesBreak)) ()
breakFor = emit Break


-- | Given a state, runs the state monad
runHylogen :: HylogenInternal ctx a -> State -> ((a, Emitted), State)
runHylogen h state = M.runIdentity $ M.runStateT (M.runWriterT . unHylogenInternal $ h) state

branch_ :: HylogenInternal ctx' a -> HylogenInternal ctx ()
branch_ toBranch = do
  s <- M.get
  M.tell . snd . fst $ runHylogen toBranch s


newtype Main = Main (Hylogen Vec4)

instance Show Main where
  show (Main hylo) = unlines
    [ show program
    , ""
    , "void main()"
    , "{"
    , show codeblock
    , "  gl_FragColor = _0;" -- fixme: this doesn't work
    , "}"
    ]
    where
      state = defaultState {_indent = 1 }
      ((a, Emitted program codeblock), _) = runHylogen hylo state

test = Main $ do
  y <- ref (100 :: Vec2)
  foo <- define $ do
    x <- ref (100 :: Vec1)
    return (deref x) :: Hylogen Vec1
  x <- ref . deref $  y
  x <- ref . deref $  y

  z <- ref (0 :: Vec1)
  return 10


-- fixme: define should work on klieslis arrows, not mapped arrows
spec  = do
  describe "ref/deref" $ do
    it "works" $ do
      pending
  describe "define" $ do
    it "works for nullary functions" $ do
      print $ Main $ do
        fun <- define $ do
          return (2 :: Vec4)
        return fun

    it "works for unary functions" $ do
      print $ Main $ do
        fun <- define $ do
          return ((\x -> vec4(-x, x + x)) :: Vec2 -> Vec4)
        x <- ref (fun 1)
        return (deref x)
    it "works for nested defines" $ do
      pending
