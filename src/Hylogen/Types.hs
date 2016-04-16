{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Hylogen.Types where


import Data.Monoid
import Data.VectorSpace
import GHC.Exts (Constraint)

class (ConstructFrom' tuple hprim, Show tuple, Vec hprim) => ConstructFrom tuple hprim
instance ConstructFrom Float Vec1
instance ConstructFrom (Vec1, Vec1) Vec2
instance ConstructFrom (Vec1, Vec1, Vec1) Vec3
instance ConstructFrom (Vec2, Vec1) Vec3
instance ConstructFrom (Vec1, Vec2) Vec3
instance ConstructFrom (Vec1, Vec1, Vec1, Vec1) Vec4
instance ConstructFrom (Vec2, Vec1, Vec1) Vec4
instance ConstructFrom (Vec1, Vec2, Vec1) Vec4
instance ConstructFrom (Vec1, Vec1, Vec2) Vec4
instance ConstructFrom (Vec3, Vec1) Vec4
instance ConstructFrom (Vec1, Vec3) Vec4
instance ConstructFrom (Vec2, Vec2) Vec4

type family (ConstructFrom' tuple hprim) :: Constraint where
  ConstructFrom' a Vec1 = a ~ Float
  ConstructFrom' (a, b) Vec2 = (a ~ Vec1, b ~ Vec1)

  ConstructFrom' (a, b, c) Vec3 = (a ~ Vec1, b ~ Vec1, c ~ Vec1)
  ConstructFrom' (Vec2, b) Vec3 = (b ~ Vec1)
  ConstructFrom' (a, Vec2) Vec3 = (a ~ Vec1)

  ConstructFrom' (a, b, c, d) Vec4 = (a ~ Vec1, b ~ Vec1, c ~ Vec1, d ~ Vec1)
  ConstructFrom' (Vec3, b) Vec4 = (b ~ Vec1)
  ConstructFrom' (Vec2, b) Vec4 = (b ~ Vec2)
  ConstructFrom' (a, Vec3) Vec4 = (a ~ Vec1)
  ConstructFrom' (a, Vec2, c) Vec4 = (a ~ Vec1, c ~ Vec1)
  ConstructFrom' (Vec2, b, c) Vec4 = (b ~ Vec1, c ~ Vec1)
  ConstructFrom' (a, b, Vec2) Vec4 = (a ~ Vec1, b ~ Vec1)



class (Show v) => Vec v where
  vec :: (ConstructFrom tuple v) => tuple -> v
  vu :: String -> v
  vuop :: String -> v -> v
  vuoppre :: String -> v -> v
  vbop :: String -> v -> v -> v
  vboppre :: String -> v -> v -> v
  select :: Booly -> v -> v -> v
  fromVec1 :: Vec1 -> v
  toList :: v -> [Vec1]



class Show a => HasX a
class HasX a => HasY a
class HasY a => HasZ a
class HasZ a => HasW a


data Vec1 where
  Vec1 :: Float -> Vec1
  V1u :: String -> Vec1
  V1uop :: String -> Vec1 -> Vec1
  V1uoppre :: String -> Vec1 -> Vec1
  V1bop :: String -> Vec1 -> Vec1 -> Vec1
  V1boppre :: String -> Vec1 -> Vec1 -> Vec1
  V1select :: Booly -> Vec1 -> Vec1 -> Vec1
  Dot :: (Vec a) => a -> a -> Vec1
  X :: (HasX a) => a -> Vec1
  Y :: (HasY a) => a -> Vec1
  Z :: (HasZ a) => a -> Vec1
  W :: (HasW a) => a -> Vec1



instance Show Vec1 where
  show expr = case expr of
    Vec1 x -> show x
    V1u x -> x
    V1uop u x -> u <> "(" <> show x <> ")"
    V1uoppre u x -> "(" <> u <> show x <> ")"
    V1bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V1boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    V1select b x y -> "( " <> show b <> " ? " <> show x <> " : " <> show y <> ")"
    Dot x y -> "dot(" <> show x <> ", " <> show y <> ")"
    X x ->  show x <> ".x"
    Y x ->  show x <> ".y"
    Z x ->  show x <> ".z"
    W x ->  show x <> ".w"


instance Vec Vec1 where
  vec = Vec1
  vu = V1u
  vuop = V1uop
  vuoppre = V1uoppre
  vbop = V1bop
  vboppre = V1boppre
  select = V1select
  fromVec1 = id
  toList x = [x]

instance Num Vec1 where
  (+) = vbop "+"
  (*) = vbop "*"
  negate = vuoppre "-"
  abs = vuop "abs"
  signum = vuop "sign"
  fromInteger = Vec1 . fromInteger

instance Fractional Vec1 where
  (/) = vbop "/"
  recip = vbop "/" 1
  fromRational = Vec1 . fromRational


instance Floating Vec1 where
  pi = vu "pi"
  exp = vuop "exp"
  log = vuop "log"
  sqrt = vuop "sqrt"
  (**) = vboppre "pow"
  sin = vuop "sin"
  cos = vuop "cos"
  tan = vuop "tan"
  asin = vuop "asin"
  acos = vuop "acos"
  atan = vuop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance AdditiveGroup Vec1 where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance VectorSpace Vec1 where
  type Scalar Vec1 = Vec1
  a *^ b = a * b

instance InnerSpace Vec1 where
  (<.>) = Dot

-- | Vec2:

data Vec2 where
  Vec2 :: (ConstructFrom tuple Vec2) => tuple -> Vec2
  V2u :: String -> Vec2
  V2uop :: String -> Vec2 -> Vec2
  V2uoppre :: String -> Vec2 -> Vec2
  V2bop :: String -> Vec2 -> Vec2 -> Vec2
  V2boppre :: String -> Vec2 -> Vec2 -> Vec2
  V2bops :: String -> Vec1 -> Vec2 -> Vec2
  V2select :: Booly -> Vec2 -> Vec2 -> Vec2

instance Vec Vec2 where
  vec = Vec2
  vu = V2u
  vuop = V2uop
  vuoppre = V2uoppre
  vbop = V2bop
  vboppre = V2boppre
  select = V2select
  fromVec1 x = Vec2 (x, x)
  toList x = [X x, Y x]


instance Show Vec2 where
  show expr = case expr of
    Vec2 tuple -> "vec2" <> show tuple
    V2u x -> x
    V2uop u x -> u <> "(" <> show x <> ")"
    V2uoppre u x -> "(" <> u <> show x <> ")"
    V2bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V2boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    V2bops b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V2select b x y -> "( " <> show b <> " ? " <> show x <> " : " <> show y <> ")"

instance Num Vec2 where
  (+) = vbop "+"
  (*) = vbop "*"
  negate = vuoppre "-"
  abs = vuop "abs"
  signum = vuop "sign"
  fromInteger = fromVec1 . fromInteger

instance Fractional Vec2 where
  (/) = vbop "/"
  recip = vbop "/" 1
  fromRational = fromVec1 . fromRational


instance Floating Vec2 where
  pi = vu "pi"
  exp = vuop "exp"
  log = vuop "log"
  sqrt = vuop "sqrt"
  (**) = vboppre "pow"
  sin = vuop "sin"
  cos = vuop "cos"
  tan = vuop "tan"
  asin = vuop "asin"
  acos = vuop "acos"
  atan = vuop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance AdditiveGroup Vec2 where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance VectorSpace Vec2 where
  type Scalar Vec2 = Vec1
  a *^ b = V2bops "*" a b

instance InnerSpace Vec2 where
  (<.>) = Dot

instance HasX Vec2
instance HasY Vec2


-- | Vec3:

data Vec3 where
  Vec3 :: (ConstructFrom tuple Vec3) => tuple -> Vec3
  V3u :: String -> Vec3
  V3uop :: String -> Vec3 -> Vec3
  V3uoppre :: String -> Vec3 -> Vec3
  V3bop :: String -> Vec3 -> Vec3 -> Vec3
  V3boppre :: String -> Vec3 -> Vec3 -> Vec3
  V3bops :: String -> Vec1 -> Vec3 -> Vec3
  V3select :: Booly -> Vec3 -> Vec3 -> Vec3

instance Vec Vec3 where
  vec = Vec3
  vu = V3u
  vuop = V3uop
  vuoppre = V3uoppre
  vbop = V3bop
  vboppre = V3boppre
  select = V3select
  fromVec1 x = Vec3 (x, x, x)
  toList x = [X x, Y x, Z x]

instance Show Vec3 where
  show expr = case expr of
    Vec3 tuple -> "vec3" <> show tuple
    V3u x -> x
    V3uop u x -> u <> "(" <> show x <> ")"
    V3uoppre u x -> "(" <> u <> show x <> ")"
    V3bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V3boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    V3bops b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V3select b x y -> "( " <> show b <> " ? " <> show x <> " : " <> show y <> ")"

instance Num Vec3 where
  (+) = vbop "+"
  (*) = vbop "*"
  negate = vuoppre "-"
  abs = vuop "abs"
  signum = vuop "sign"
  fromInteger = fromVec1 . fromInteger

instance Fractional Vec3 where
  (/) = vbop "/"
  recip = vbop "/" 1
  fromRational = fromVec1 . fromRational


instance Floating Vec3 where
  pi = vu "pi"
  exp = vuop "exp"
  log = vuop "log"
  sqrt = vuop "sqrt"
  (**) = vboppre "pow"
  sin = vuop "sin"
  cos = vuop "cos"
  tan = vuop "tan"
  asin = vuop "asin"
  acos = vuop "acos"
  atan = vuop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance AdditiveGroup Vec3 where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance VectorSpace Vec3 where
  type Scalar Vec3 = Vec1
  a *^ b = V3bops "*" a b

instance InnerSpace Vec3 where
  (<.>) = Dot

instance HasX Vec3
instance HasY Vec3
instance HasZ Vec3



-- | Vec4:

data Vec4 where
  Vec4 :: (ConstructFrom tuple Vec4) => tuple -> Vec4
  V4u :: String -> Vec4
  V4uop :: String -> Vec4 -> Vec4
  V4uoppre :: String -> Vec4 -> Vec4
  V4bop :: String -> Vec4 -> Vec4 -> Vec4
  V4boppre :: String -> Vec4 -> Vec4 -> Vec4
  V4bops :: String -> Vec1 -> Vec4 -> Vec4
  V4select :: Booly -> Vec4 -> Vec4 -> Vec4
  Texture2D :: Texture -> Vec2 -> Vec4


instance Vec Vec4 where
  vec = Vec4
  vu = V4u
  vuop = V4uop
  vuoppre = V4uoppre
  vbop = V4bop
  vboppre = V4boppre
  select = V4select
  fromVec1 x = Vec4 (x, x, x, x)
  toList x = [X x, Y x, Z x, W x]

instance Show Vec4 where
  show expr = case expr of
    Vec4 tuple -> "vec4" <> show tuple
    V4u x -> x
    V4uop u x -> u <> "(" <> show x <> ")"
    V4uoppre u x -> "(" <> u <> show x <> ")"
    V4bop b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V4boppre b x y -> b <> "(" <> show x <> ", " <> show y <> ")"
    V4bops b x y -> "(" <> show x <> " " <> b <> " " <> show y <> ")"
    V4select b x y -> "( " <> show b <> " ? " <> show x <> " : " <> show y <> ")"
    Texture2D t v -> "texture2D(" <> show t <> ", " <> show v <> ")"

instance Num Vec4 where
  (+) = vbop "+"
  (*) = vbop "*"
  negate = vuoppre "-"
  abs = vuop "abs"
  signum = vuop "sign"
  fromInteger = fromVec1 . fromInteger

instance Fractional Vec4 where
  (/) = vbop "/"
  recip = vbop "/" 1
  fromRational = fromVec1 . fromRational


instance Floating Vec4 where
  pi = vu "pi"
  exp = vuop "exp"
  log = vuop "log"
  sqrt = vuop "sqrt"
  (**) = vboppre "pow"
  sin = vuop "sin"
  cos = vuop "cos"
  tan = vuop "tan"
  asin = vuop "asin"
  acos = vuop "acos"
  atan = vuop "atan"
  sinh x = (exp x - exp (negate x))/2
  cosh x = (exp x + exp (negate x))/2
  tanh x = sinh x / cosh x
  asinh x = log $ x + sqrt(x**2 + 1)
  acosh x = log $ x + sqrt(x**2 - 1)
  atanh x = 0.5 * log ((1 + x)/(1 - x))

instance AdditiveGroup Vec4 where
  zeroV = 0
  (^+^) = (+)
  negateV = negate
  (^-^) = (-)

instance VectorSpace Vec4 where
  type Scalar Vec4 = Vec1
  a *^ b = V4bops "*" a b

instance InnerSpace Vec4 where
  (<.>) = Dot

instance HasX Vec4
instance HasY Vec4
instance HasZ Vec4
instance HasW Vec4


data Texture where
  Tu :: String -> Texture

instance Show Texture where
  show (Tu xs) = xs

-- | We implement Bool as a Num

data Booly where
  Bu:: String -> Booly
  Buop :: String -> Booly -> Booly
  Buoppre :: String -> Booly -> Booly
  Bbop :: String -> Booly -> Booly -> Booly
  Bcomp :: (Vec a) => String -> a -> a -> Booly
  Bcomp_ :: String -> Vec1 -> Vec1 -> Booly

instance Show Booly where
  show expr = case expr of
    Bu x -> x
    Buop u x -> u <> "(" <> show x <> ")"
    Buoppre u x -> "(" <> u <> show x <> ")"
    Bbop u x y -> "(" <> show x <> " " <> u <> " " <>  show y <> ")"
    Bcomp u x y -> show . product $ zipWith (Bcomp_ u) (toList x) (toList y)
    Bcomp_ u x y -> "(" <> show x <> " " <> u <> " " <>  show y <> ")"

instance Num Booly where
  (+) = Bbop "||"
  (*) = Bbop "&&"
  negate = Buoppre "!"
  abs = id
  signum = id
  fromInteger x
    | x > 0 = Bu "true"
    | otherwise = Bu "false"


-- data Expr a where
--   Node a :: ID -> a
