module Hylogen where

import Prelude
import Free
import Control.Bind



cata :: forall f a. (Functor f) => (f a -> a) -> Free f a -> a
cata alg (Pure x) = x
cata alg (Free fx) = alg $ map (cata alg) $ fx

data BOp = Mul | Div | Plus | Minus

instance showBOp :: Show (BOp) where
  show (Mul) = "*"
  show (Div) = "/"
  show (Plus) = "+"
  show (Minus) = "-"

data Expr a = Float a
          | BinOp BOp a a
          | Vec2 a a
          | Vec3 a a a

instance functorExpr :: Functor Expr where
  map g (Float a) = Float (g a)

instance showExpr :: (Show a) => Show (Expr a) where
  show (Float x) = show x
  show (BinOp b x y) = show x <> " " <> show b <> " " <> show y
  show (Vec2 x y) = "vec2(" <> show x <> ", " <> show y <> ")"
  show (Vec3 x y z) = "vec3(" <> show x <> ", " <> show y <> ", " <> show z <> ")"
