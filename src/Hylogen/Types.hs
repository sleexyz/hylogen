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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Hylogen.Types where


import Data.Monoid
import Data.VectorSpace
import GHC.Exts (Constraint)
import Data.Hashable
import GHC.Generics

class (ConstructFrom' tuple hprim, Show tuple, Vec hprim) => ConstructFrom tuple hprim where
  hashConstructor :: tuple -> hprim -> HashTree

instance ConstructFrom Float Vec1 where
  hashConstructor x _ = Leaf . Hash . hash $ x
instance ConstructFrom (Vec1, Vec1) Vec2 where
  hashConstructor (x, y) _  = mkBranch2 "vec2" x y
instance ConstructFrom (Vec1, Vec1, Vec1) Vec3 where
  hashConstructor (x, y, z) _  = mkBranch3 "vec3" x y z
instance ConstructFrom (Vec2, Vec1) Vec3 where
  hashConstructor (x, y) _  = mkBranch2 "vec3" x y
instance ConstructFrom (Vec1, Vec2) Vec3 where
  hashConstructor (x, y) _  = mkBranch2 "vec3" x y
instance ConstructFrom (Vec1, Vec1, Vec1, Vec1) Vec4 where
  hashConstructor (x, y, z, w) _  = mkBranch4 "vec4" x y z w
instance ConstructFrom (Vec2, Vec1, Vec1) Vec4 where
  hashConstructor (x, y, z) _  = mkBranch3 "vec4" x y z
instance ConstructFrom (Vec1, Vec2, Vec1) Vec4 where
  hashConstructor (x, y, z) _  = mkBranch3 "vec4" x y z
instance (a ~ Vec1, b ~ Vec1) => ConstructFrom (a, b, Vec2) Vec4 where
  hashConstructor (x, y, z) _  = mkBranch3 "vec4" x y z
instance ConstructFrom (Vec3, Vec1) Vec4 where
  hashConstructor (x, y) _  = mkBranch2 "vec4" x y
instance (a ~ Vec1) => ConstructFrom (a, Vec3) Vec4 where
  hashConstructor (x, y) _  = mkBranch2 "vec4" x y
instance (a ~ Vec2) => ConstructFrom (a, Vec2) Vec4 where
  hashConstructor (x, y) _  = mkBranch2 "vec4" x y

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


  -- ConstructFrom' (Vec1, Vec1, Vec2) Vec4 = ()
  -- ConstructFrom' (Vec1, Vec2, Vec1) Vec4 = ()
  -- ConstructFrom' (Vec2, Vec1, Vec1) Vec4 = ()

  ConstructFrom' (Vec2, b, c) Vec4 = (b ~ Vec1, c ~ Vec1)
  ConstructFrom' (a, Vec2, c) Vec4 = (a ~ Vec1, c ~ Vec1) -- works
  ConstructFrom' (a, b, Vec2) Vec4 = (a ~ Vec1, b ~ Vec1)



class (Expressible v, Show v) => Vec v where
  vec :: (ConstructFrom tuple v) => tuple -> v
  vu :: String -> v
  vuop :: String -> v -> v
  vuoppre :: String -> v -> v
  vbop :: String -> v -> v -> v
  vboppre :: String -> v -> v -> v
  select :: Booly -> v -> v -> v
  fromVec1 :: Vec1 -> v
  toList :: v -> [Vec1]



class (Expressible a, Show a) => HasX a
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


-- data Variable where
--   VVec1 :: Vec1 -> Variable
--   VVec2 :: Vec2 -> Variable
--   VVec3 :: Vec3 -> Variable
--   VBooly :: Booly -> Variable

-- instance Num (Context Vec1) where

-- data Expr a where
--   Node a :: ID -> a



instance Hashable Vec1 where
  hashWithSalt salt x = hashWithSalt salt $ ("vec1", show x)
instance Hashable Vec2 where
  hashWithSalt salt x = hashWithSalt salt $ ("vec2", show x)
instance Hashable Vec3 where
  hashWithSalt salt x = hashWithSalt salt $ ("vec3", show x)
instance Hashable Vec4 where
  hashWithSalt salt x = hashWithSalt salt $ ("vec4", show x)
instance Hashable Booly where
  hashWithSalt salt x = hashWithSalt salt $ ("booly", show x)

instance Hashable Texture where
  hashWithSalt salt x = hashWithSalt salt $ ("texture2D", show x)

-- TODO: textures cannot be saved as variable!



data GLSLType = GLSLFloat
              | GLSLVec1
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLBool
              deriving Eq

instance Show GLSLType where
  show x = case x of
    GLSLFloat -> "float"
    GLSLVec1 -> "vec1"
    GLSLVec2 -> "vec2"
    GLSLVec3 -> "vec3"
    GLSLVec4 -> "vec4"
    GLSLBool -> "bool"

newtype Hash = Hash Int
  deriving (Generic, Hashable, Show, Eq, Ord)

data HashTree = Leaf Hash | Branch Hash [HashTree]
  deriving (Generic, Hashable, Show, Eq, Ord)

class (Show a) => Expressible a where
  getType ::  a -> GLSLType
  toHashTree :: a -> HashTree

mkBranch1 :: ( Hashable t
             , Expressible a
             ) => t -> a -> HashTree
mkBranch1 str x = Branch (Hash $ hash (str, subTrees)) subTrees
  where
    subTrees = [toHashTree x]

mkBranch2 :: ( Hashable t
             , Expressible a
             , Expressible b
             ) => t -> a -> b-> HashTree
mkBranch2 str x y = Branch (Hash $ hash (str, subTrees)) subTrees
  where
    subTrees = [toHashTree x, toHashTree y]

mkBranch3 :: ( Hashable t
             , Expressible a
             , Expressible b
             , Expressible c
             ) => t -> a -> b -> c -> HashTree
mkBranch3 str x y z = Branch (Hash $ hash (str, subTrees)) subTrees
  where
    subTrees = [toHashTree x, toHashTree y, toHashTree z]

mkBranch4 :: ( Hashable t
             , Expressible a
             , Expressible b
             , Expressible c
             , Expressible d
             ) => t -> a -> b -> c -> d -> HashTree
mkBranch4 str x y z w = Branch (Hash $ hash (str, subTrees)) subTrees
  where
    subTrees = [toHashTree x, toHashTree y, toHashTree z, toHashTree w]

-- TODO: tag strings so hash is correct

instance Expressible Vec1 where
  getType _ = GLSLVec1
  toHashTree a@(Vec1 x)  = hashConstructor x a
  toHashTree a@(V1u _) = Leaf $ Hash $ hash a
  toHashTree (V1uop str x) = mkBranch1 str x
  toHashTree (V1uoppre str x) = mkBranch1 str x
  toHashTree (V1bop str x y) = mkBranch2 str x y
  toHashTree (V1boppre str x y) = mkBranch2 str x y
  toHashTree (V1select str x y) = mkBranch2 str x y
  toHashTree (Dot x y) = mkBranch2 "dot" x y
  toHashTree (X x) = mkBranch1 "X" x
  toHashTree (Y x) = mkBranch1 "Y" x
  toHashTree (Z x) = mkBranch1 "Z" x
  toHashTree (W x) = mkBranch1 "W" x

instance Expressible Vec2 where
  getType _ = GLSLVec2
  toHashTree a@(Vec2 x)  = hashConstructor x a
  toHashTree a@(V2u _) = Leaf $ Hash $ hash a
  toHashTree (V2uop str x) = mkBranch1 str x
  toHashTree (V2uoppre str x) = mkBranch1 str x
  toHashTree (V2bop str x y) = mkBranch2 str x y
  toHashTree (V2boppre str x y) = mkBranch2 str x y
  toHashTree (V2bops str x y) = mkBranch2 str x y
  toHashTree (V2select str x y) = mkBranch2 str x y

instance Expressible Vec3 where
  getType _ = GLSLVec3
  toHashTree a@(Vec3 x)  = hashConstructor x a
  toHashTree a@(V3u _) = Leaf $ Hash $ hash a
  toHashTree (V3uop str x) = mkBranch1 str x
  toHashTree (V3uoppre str x) = mkBranch1 str x
  toHashTree (V3bop str x y) = mkBranch2 str x y
  toHashTree (V3boppre str x y) = mkBranch2 str x y
  toHashTree (V3bops str x y) = mkBranch2 str x y
  toHashTree (V3select str x y) = mkBranch2 str x y

instance Expressible Vec4 where
  getType _ = GLSLVec4
  toHashTree a@(Vec4 x)  = hashConstructor x a
  toHashTree a@(V4u _) = Leaf $ Hash $ hash a
  toHashTree (V4uop str x) = mkBranch1 str x
  toHashTree (V4uoppre str x) = mkBranch1 str x
  toHashTree (V4bop str x y) = mkBranch2 str x y
  toHashTree (V4boppre str x y) = mkBranch2 str x y
  toHashTree (V4bops str x y) = mkBranch2 str x y
  toHashTree (V4select str x y) = mkBranch2 str x y
  toHashTree (Texture2D t x) = mkBranch1 ("texture2D", show t) x

instance Expressible Booly where
  getType _ = GLSLBool
  toHashTree a@(Bu _) = Leaf $ Hash $ hash a
  toHashTree (Buop str x) = mkBranch1 str x
  toHashTree (Buoppre str x) = mkBranch1 str x
  toHashTree (Bbop str x y) = mkBranch2 str x y
  toHashTree (Bcomp str x y) = mkBranch2 str x y -- will this work with the weird show implementation?
  toHashTree (Bcomp_ str x y) = mkBranch2 str x y -- will this work with the weird show implementation?

-- | Existential
data Expr where
  ToExpr :: (Expressible a) => a -> Expr

deriving instance Show Expr

