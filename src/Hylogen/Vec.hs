{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


module Hylogen.Vec where

import GHC.TypeLits
import Data.VectorSpace
import Data.Proxy

import Hylogen.Expr


data FloatVec (n :: Nat) = FloatVec

type Vec n = Expr (FloatVec n)
type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4

instance ToGLSLType (FloatVec 1) where
  toGLSLType _ = GLSLFloat
  tag = FloatVec
instance ToGLSLType (FloatVec 2) where
  toGLSLType _ = GLSLVec2
  tag = FloatVec
instance ToGLSLType (FloatVec 3) where
  toGLSLType _ = GLSLVec3
  tag = FloatVec
instance ToGLSLType (FloatVec 4) where
  toGLSLType _ = GLSLVec4
  tag = FloatVec



class (ToGLSLType (FloatVec n), KnownNat n) => Veccable n where
  copy :: Vec1 -> Vec n
  toList :: Vec n -> [Vec1]


instance Veccable 1 where
  copy = id
  toList x = [x]
instance Veccable 2 where
  copy x = op2pre' "vec2" x x
  toList x = [x ! X, x ! Y]
instance Veccable 3 where
  copy x = op3pre' "vec3" x x x
  toList x = [x ! X, x ! Y, x ! Z]
instance Veccable 4 where
  copy x = op4pre' "vec4" x x x x
  toList x = [x ! X, x ! Y, x ! Z, x ! W]



instance (Veccable n) => Num (Vec n) where
  (+) = op2' "+"
  (-) = op2' "-"
  (*) = op2' "*"
  abs = op1pre "abs"
  signum = op1pre "sign"
  negate = op1 "-"
  fromInteger x = copy . uniform . show $ (fromInteger x :: Float)


instance (Veccable n) => Fractional (Vec n) where
  (/) = op2' "/"
  fromRational x = copy . uniform . show $ (fromRational x :: Float)

instance (Veccable n) => Floating (Vec n) where
  pi = copy $ uniform "pi"
  exp = op1pre "exp"
  log = op1pre "log"
  sqrt = op1pre "sqrt"
  (**) = op2pre' "pow"
  sin = op1pre "sin"
  cos = op1pre "cos"
  tan = op1pre "tan"
  asin = op1pre "asin"
  acos = op1pre "acos"
  atan = op1pre "atan"
  sinh x = (exp x - exp (negate x)) / 2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance Veccable n => AdditiveGroup (Vec n) where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance Veccable n => VectorSpace (Vec n) where
  type Scalar (Vec n) = Vec 1
  a *^ b = copy a * b

instance Veccable n => InnerSpace (Vec n) where
  a <.> b = Expr fv (Tree (Op2Pre, GLSLFloat, "dot") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec 1


  
type (>=) x y = (x + 1 <=? y) ~ 'False

class Swizzle a where
  type InputMin a :: Nat
  type OutputDim a :: Nat
  swizzShow :: a -> String

  (!) :: forall n. (Veccable (OutputDim a), n >= InputMin a) => Vec n -> a -> Vec (OutputDim a)
  x ! sw = Expr fv (Tree (Access, toGLSLType fv, swizzShow sw) (fmap toMono [x]))
    where fv = FloatVec :: FloatVec (OutputDim a)

data X = X
instance Swizzle X where
  type InputMin X = 2
  type OutputDim X = 1
  swizzShow _ = "x"

data Y = Y
instance Swizzle Y where
  type InputMin Y = 2
  type OutputDim Y = 1
  swizzShow _ = "y"

data Z = Z
instance Swizzle Z where
  type InputMin Z = 3
  type OutputDim Z = 1
  swizzShow _ = "z"

data W = W
instance Swizzle W where
  type InputMin W = 4
  type OutputDim W = 1
  swizzShow _ = "w"

-- TODO: finish swizzling!


vec2 :: (Vec1, Vec1) -> Vec2
vec2 (x, y) = op2pre' "vec2" x y


class ToVec3 tuple where vec3 :: tuple -> Vec3

instance (a ~ Vec m, b ~ Vec (3 - m)) => ToVec3 (a, b) where
  vec3 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec3") [toMono x, toMono y])
      where fv = FloatVec :: FloatVec 3

instance (a ~ Vec1, b ~ Vec1, c ~ Vec1) => ToVec3 (a, b, c) where
  vec3 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec3") (fmap toMono [x, y, z]))
      where fv = FloatVec :: FloatVec 3


class ToVec4 tuple where vec4 :: tuple -> Vec4

instance (a ~ Vec m, b ~ Vec (4 - m)) => ToVec4 (a, b) where
  vec4 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec4") [toMono x,toMono y])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (b ~ Vec1, c ~ Vec1) => ToVec4 (Vec2, b, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (a ~ Vec1, c ~ Vec1) => ToVec4 (a, Vec2, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-} (a ~ Vec1, b ~ Vec1) => ToVec4 (a, b, Vec2) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") [toMono x,toMono y,toMono z])
      where fv = FloatVec :: FloatVec 4


instance (a ~ Vec1, b ~ Vec1, c ~ Vec1, d ~ Vec1) => ToVec4 (a, b, c, d) where
  vec4 (x, y, z, w) = Expr fv (Tree (Op4Pre, toGLSLType fv, "vec4") (fmap toMono [x, y, z, w]))
      where fv = FloatVec :: FloatVec 4
