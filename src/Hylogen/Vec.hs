{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}


module Hylogen.Vec where

import Data.Hashable
import GHC.Generics
import GHC.TypeLits
import Data.Reify
import Data.VectorSpace

import Hylogen.Expr


data FloatVec (n :: Nat) = FloatVec

type Vec n = Expr (FloatVec n)
type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4

instance ToGLSLType (FloatVec 1) where
  toGLSLType _ = GLSLFloat
instance ToGLSLType (FloatVec 2) where
  toGLSLType _ = GLSLVec2
instance ToGLSLType (FloatVec 3) where
  toGLSLType _ = GLSLVec3
instance ToGLSLType (FloatVec 4) where
  toGLSLType _ = GLSLVec4

vu :: forall (n :: Nat). (Veccable n)
      => String -> Vec n
vu str = Expr fv (Tree (Uniform, toGLSLType fv, str) [])
    where fv = FloatVec :: FloatVec n

vop1 :: forall (m :: Nat) (n ::Nat). (Veccable n)
        => String -> Vec m -> Vec n
vop1 str a = Expr fv (Tree (Op1, toGLSLType fv, str) (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n

vop1'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n
vop1'' = vop1


vop1pre :: forall (m :: Nat) (n :: Nat). (Veccable n)
           => String -> Vec m -> Vec n
vop1pre str a = Expr fv (Tree (Op1Pre, toGLSLType fv, str) (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n

vop1pre'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n
vop1pre'' = vop1





vop2 :: forall (m1 :: Nat) (m2 :: Nat) (n :: Nat). (Veccable n)
        => String -> Vec m1 -> Vec m2 -> Vec n
vop2 str a b = Expr fv (Tree (Op2, toGLSLType fv, str) [toMono a, toMono b])
    where fv = FloatVec :: FloatVec n

vop2' :: forall (m :: Nat) (n :: Nat). (Veccable n)
        => String -> Vec m -> Vec m -> Vec n
vop2' str a b = Expr fv (Tree (Op2, toGLSLType fv, str) (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n


vop2'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n -> Vec n
vop2'' = vop2''



vop2pre :: forall (m1 :: Nat) (m2 :: Nat) (n :: Nat). (Veccable n)
           => String -> Vec m1 -> Vec m2 -> Vec n
vop2pre str a b = Expr fv (Tree (Op2Pre, toGLSLType fv, str) [toMono a, toMono b])
    where fv = FloatVec :: FloatVec n

vop2pre' :: forall (m :: Nat) (n :: Nat). (Veccable n)
        => String -> Vec m -> Vec m -> Vec n
vop2pre' str a b = Expr fv (Tree (Op2Pre, toGLSLType fv, str) (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n

vop2pre'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n -> Vec n
vop2pre'' = vop2''





vop3pre :: forall (m1 :: Nat) (m2 :: Nat) (m3 :: Nat) (n :: Nat). (Veccable n)
           => String -> Vec m1 -> Vec m2 -> Vec m3 -> Vec n
vop3pre str a b c = Expr fv (Tree (Op3Pre, toGLSLType fv, str) [toMono a, toMono b, toMono c])
    where fv = FloatVec :: FloatVec n

vop3pre' :: forall (m :: Nat) (n :: Nat). (Veccable n)
        => String -> Vec m -> Vec m -> Vec m -> Vec n
vop3pre' str a b c = Expr fv (Tree (Op3Pre, toGLSLType fv, str) (fmap toMono [a, b, c]))
    where fv = FloatVec :: FloatVec n

vop3pre'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n -> Vec n -> Vec n
vop3pre'' = vop3pre''





vop4pre :: forall (m1 :: Nat) (m2 :: Nat) (m3 :: Nat) (m4 :: Nat) (n :: Nat). (Veccable n)
           => String -> Vec m1 -> Vec m2 -> Vec m3 -> Vec m4 -> Vec n
vop4pre str a b c d = Expr fv (Tree (Op4Pre, toGLSLType fv, str) [toMono a,toMono b,toMono c, toMono d])
    where fv = FloatVec :: FloatVec n

vop4pre' :: forall (m :: Nat) (n :: Nat). (Veccable n)
        => String -> Vec m -> Vec m -> Vec m -> Vec m -> Vec n
vop4pre' str a b c d = Expr fv (Tree (Op3Pre, toGLSLType fv, str) (fmap toMono [a, b, c, d]))
    where fv = FloatVec :: FloatVec n

vop4pre'' :: forall (n :: Nat). (Veccable n) => String -> Vec n -> Vec n -> Vec n -> Vec n -> Vec n
vop4pre'' = vop4pre''

class (ToGLSLType (FloatVec n), KnownNat n) => Veccable n where
  copy :: Vec1 -> Vec n
  toList :: Vec n -> [Vec1]


instance Veccable 1 where
  copy = id
  toList x = [x]
instance Veccable 2 where
  copy x = vop2pre' "vec2" x x
  toList x = [x ! X, x ! Y]
instance Veccable 3 where
  copy x = vop3pre' "vec3" x x x
  toList x = [x ! X, x ! Y, x ! Z]
instance Veccable 4 where
  copy x = vop4pre' "vec4" x x x x
  toList x = [x ! X, x ! Y, x ! Z, x ! W]



instance (Veccable n) => Num (Vec n) where
  (+) = vop2' "+"
  (-) = vop2' "-"
  (*) = vop2' "*"
  abs = vop1pre "abs"
  signum = vop1pre "sign"
  negate = vop1 "-"
  fromInteger x = vu . show $ (fromInteger x :: Float)


instance (Veccable n) => Fractional (Vec n) where
  (/) = vop2' "/"
  fromRational x = vu . show $ (fromRational x :: Float)

instance (Veccable n) => Floating (Vec n) where
  pi = copy $ vu "pi"
  exp = vop1pre "exp"
  log = vop1pre "log"
  sqrt = vop1pre "sqrt"
  (**) = vop2pre' "pow"
  sin = vop1pre "sin"
  cos = vop1pre "cos"
  tan = vop1pre "tan"
  asin = vop1pre "asin"
  acos = vop1pre "acos"
  atan = vop1pre "atan"
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
vec2 (x, y) = vop2pre' "vec2" x y


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

-- Implement Boolean
-- Implement  
-- I need a typessafe programmer-facing thing
-- I also need a monomorphic implementation
-- Another solution is to introduce the notion of scope.
-- so I'll move to data.reify
