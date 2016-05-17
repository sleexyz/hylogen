{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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

-- {-# LANGUAGE UndecidableInstances #-}

module Hylogen.TypesNew where

import Data.Hashable
import GHC.Generics
import GHC.TypeLits
import Data.Reify
import Data.VectorSpace

data GLSLType = GLSLFloat
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLBool
              | GLSLTexture
              deriving (Generic, Hashable, Eq, Ord)

data ExprForm = Uniform
              | Variable
              | Op1
              | Op1Pre
              | Op2
              | Op2Pre
              | Op3Pre
              | Op4Pre
              | Select
              | Access
                deriving (Show, Generic, Hashable)

data Tree a  = Tree { getElem     :: a
                    , getChildren :: [Tree a]
                    }

type ExprMono = Tree (ExprForm, GLSLType, String)

instance Show ExprMono where
  show (Tree (form, _, str) xs) = case form of
    Uniform  -> str
    Variable -> str
    Op1      -> mconcat ["(", str, show (xs!!0), ")"]
    Op1Pre   -> mconcat [ str, "(", show (xs!!0), ")"]
    Op2      -> mconcat ["(", show (xs !! 0), " ", str, " ", show (xs !! 1), ")"]
    Op2Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ")"]
    Op3Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ")"]
    Op4Pre   -> mconcat [str, "(", show (xs!!0), ", ", show (xs!!1), ", ", show (xs!!2), ", ", show (xs!!3), ")"]
    Select   -> mconcat ["( ", show (xs!!0), " ? ", show (xs!!1), " : ", show (xs!!2), ")"]
    Access   -> mconcat [show (xs!!0), ".", str]

-- The GLSLType needs to be manually dependent

-- light typed wrapper
data Expr ty = Expr { getTypeTag :: ty
                    , toMono ::  Tree (ExprForm, GLSLType, String)
                    }

instance ToGLSLType ty => Show (Expr ty) where
  show = show . toMono

class ToGLSLType  ty where
  toGLSLType :: ty -> GLSLType

  -- | The problem is that monomorphize traverses the whole tree!
  -- | What if this was parmapped?


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

class (ToGLSLType (FloatVec n), KnownNat n) => Veccable n where
  copy :: Vec1 -> Vec n
  

instance Veccable 1 where copy = id
instance Veccable 2 where copy x = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec2") (fmap toMono [x, x]))
                            where fv = FloatVec :: FloatVec 2
instance Veccable 3 where copy x = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec3") (fmap toMono [x, x, x]))
                            where fv = FloatVec :: FloatVec 3
instance Veccable 4 where copy x = Expr fv (Tree (Op4Pre, toGLSLType fv, "vec4") (fmap toMono [x, x, x, x]))
                            where fv = FloatVec :: FloatVec 4
  
  


instance (Veccable n) => Num (Vec n) where
  a+b = Expr fv (Tree (Op2, toGLSLType fv, "+") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n

  a-b = Expr fv (Tree (Op2, toGLSLType fv, "-") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n

  a*b = Expr fv (Tree (Op2, toGLSLType fv, "*") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n

  abs a = Expr fv (Tree (Op1Pre, toGLSLType fv, "abs") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n

  signum a = Expr fv (Tree (Op1Pre, toGLSLType fv, "sign") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n

  negate a = Expr fv (Tree (Op1, toGLSLType fv, "-") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n

  fromInteger a = Expr fv (Tree (Uniform , toGLSLType fv, show (fromInteger a :: Float)) [])
    where fv = FloatVec :: FloatVec n


instance (Veccable n) => Fractional (Vec n) where
  a/b = Expr fv (Tree (Op2, toGLSLType fv, "/") (fmap toMono [a, b]))
    where fv = FloatVec :: FloatVec n
  fromRational x = Expr fv (Tree (Uniform, toGLSLType fv, show (fromRational x :: Float)) [])
    where fv = FloatVec :: FloatVec n

instance (Veccable n) => Floating (Vec n) where
  pi = copy $ Expr fv (Tree (Uniform, toGLSLType fv, "pi") [])
    where fv = FloatVec :: FloatVec 1
  exp a = Expr fv (Tree (Op1Pre, toGLSLType fv, "exp") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  log a = Expr fv (Tree (Op1Pre, toGLSLType fv, "log") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  sqrt a = Expr fv (Tree (Op1Pre, toGLSLType fv, "sqrt") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  a ** b = Expr fv (Tree (Op2Pre, toGLSLType fv, "pow") (fmap toMono [a, b])) 
    where fv = FloatVec :: FloatVec n
  sin a = Expr fv (Tree (Op1Pre, toGLSLType fv, "sin") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  cos a = Expr fv (Tree (Op1Pre, toGLSLType fv, "cos") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  tan a = Expr fv (Tree (Op1Pre, toGLSLType fv, "tan") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  asin a = Expr fv (Tree (Op1Pre, toGLSLType fv, "asin") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  acos a = Expr fv (Tree (Op1Pre, toGLSLType fv, "acos") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
  atan a = Expr fv (Tree (Op1Pre, toGLSLType fv, "atan") (fmap toMono [a]))
    where fv = FloatVec :: FloatVec n
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


  
type (>=) x y = (x + 1 <=? y) ~ False

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
vec2 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec2") (fmap toMono [x, y]))
    where fv = FloatVec :: FloatVec 2


class ToVec3 tuple where vec3 :: tuple -> Vec3

instance (a ~ Vec m, b ~ Vec (3 - m)) => ToVec3 (a, b) where
  vec3 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec3") ([toMono x, toMono y]))
      where fv = FloatVec :: FloatVec 3

instance (a ~ Vec1, b ~ Vec1, c ~ Vec1) => ToVec3 (a, b, c) where
  vec3 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec3") (fmap toMono [x, y, z]))
      where fv = FloatVec :: FloatVec 3


class ToVec4 tuple where vec4 :: tuple -> Vec4

instance (a ~ Vec m, b ~ Vec (4 - m)) => ToVec4 (a, b) where
  vec4 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec4") ([toMono x,toMono y]))
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-}(b ~ Vec1, c ~ Vec1) => ToVec4 (Vec2, b, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") ([toMono x,toMono y,toMono z]))
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-}(a ~ Vec1, c ~ Vec1) => ToVec4 (a, Vec2, c) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") ([toMono x,toMono y,toMono z]))
      where fv = FloatVec :: FloatVec 4

instance {-#INCOHERENT#-}(a ~ Vec1, b ~ Vec1) => ToVec4 (a, b, Vec2) where
  vec4 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec4") ([toMono x,toMono y,toMono z]))
      where fv = FloatVec :: FloatVec 4


instance (a ~ Vec1, b ~ Vec1, c ~ Vec1, d ~ Vec1) => ToVec4 (a, b, c, d) where
  vec4 (x, y, z, w) = Expr fv (Tree (Op4Pre, toGLSLType fv, "vec4") (fmap toMono [x, y, z, w]))
      where fv = FloatVec :: FloatVec 4



foo :: Vec2
foo = Expr (FloatVec :: FloatVec 2) (Tree (Uniform, GLSLVec2, "foo") [])

-- Play with Typeable and Reify
-- I need a typessafe programmer-facing thing
-- I also need a monomorphic implementation
-- Another solution is to introduce the notion of scope.
-- so I'll move to data.reify
