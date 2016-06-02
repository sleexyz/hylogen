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


module Hylogen.Types.Vec where

import GHC.TypeLits
import Data.VectorSpace

import Hylogen.Expr


-- | Floating vector singleton type tag
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



-- | A Nat is veccable if it can be the dimension of a GLSL vector
class (ToGLSLType (FloatVec n), KnownNat n) => Veccable n where
  -- | Creates a Vec n from a Vec1
  copy :: Vec1 -> Vec n
  -- | Transforms a Vec n into a list of Vec1's
  toList :: Vec n -> [Vec1]


instance Veccable 1 where
  copy = id
  toList v = [v]
instance Veccable 2 where
  copy v = op2pre' "vec2" v v 
  toList v = [x_ v, y_ v]
instance Veccable 3 where
  copy v = op3pre' "vec3" v v v
  toList v = [x_ v, y_ v, z_ v]
instance Veccable 4 where
  copy v = op4pre' "vec4" v v v v
  toList v = [x_ v, y_ v, z_ v]



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
  -- pi = copy $ uniform "pi"
  pi = copy $ uniform "3.141592653589793238462643383"
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


  


-- | Exposed constructor for making vec2's
vec2 :: (Vec1, Vec1) -> Vec2
vec2 (x, y) = op2pre' "vec2" x y


class ToVec3 tuple where
  -- | Exposed constructor for making vec3's
  vec3 :: tuple -> Vec3

instance (a ~ Vec m, b ~ Vec (3 - m)) => ToVec3 (a, b) where
  vec3 (x, y) = Expr fv (Tree (Op2Pre, toGLSLType fv, "vec3") [toMono x, toMono y])
      where fv = FloatVec :: FloatVec 3

instance (a ~ Vec1, b ~ Vec1, c ~ Vec1) => ToVec3 (a, b, c) where
  vec3 (x, y, z) = Expr fv (Tree (Op3Pre, toGLSLType fv, "vec3") (fmap toMono [x, y, z]))
      where fv = FloatVec :: FloatVec 3


class ToVec4 tuple where
  -- | Exposed constructor for making vec4's
  vec4 :: tuple -> Vec4

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


type (>=) x y = (x + 1 <=? y) ~ 'False

-- | Makes swizzle functions. Uses GenSwizz.hs to generate the following 340 swizzle expressions.
mkSwizz :: forall n m. (Veccable n, Veccable m) => String -> Vec n -> Vec m
mkSwizz str v = Expr fv (Tree (Access, toGLSLType fv, str) [toMono v])
  where
    fv = FloatVec :: FloatVec m

xxxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xxxx_ = mkSwizz "xxxx"

yxxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yxxx_ = mkSwizz "yxxx"

zxxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxxx_ = mkSwizz "zxxx"

wxxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxxx_ = mkSwizz "wxxx"

xxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
xxx_ = mkSwizz "xxx"

xyxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xyxx_ = mkSwizz "xyxx"

yyxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yyxx_ = mkSwizz "yyxx"

zyxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyxx_ = mkSwizz "zyxx"

wyxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyxx_ = mkSwizz "wyxx"

yxx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
yxx_ = mkSwizz "yxx"

xzxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzxx_ = mkSwizz "xzxx"

yzxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzxx_ = mkSwizz "yzxx"

zzxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzxx_ = mkSwizz "zzxx"

wzxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzxx_ = mkSwizz "wzxx"

zxx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zxx_ = mkSwizz "zxx"

xwxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwxx_ = mkSwizz "xwxx"

ywxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywxx_ = mkSwizz "ywxx"

zwxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwxx_ = mkSwizz "zwxx"

wwxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwxx_ = mkSwizz "wwxx"

wxx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wxx_ = mkSwizz "wxx"

xx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 2
xx_ = mkSwizz "xx"

xxyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xxyx_ = mkSwizz "xxyx"

yxyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yxyx_ = mkSwizz "yxyx"

zxyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxyx_ = mkSwizz "zxyx"

wxyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxyx_ = mkSwizz "wxyx"

xyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
xyx_ = mkSwizz "xyx"

xyyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xyyx_ = mkSwizz "xyyx"

yyyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yyyx_ = mkSwizz "yyyx"

zyyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyyx_ = mkSwizz "zyyx"

wyyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyyx_ = mkSwizz "wyyx"

yyx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
yyx_ = mkSwizz "yyx"

xzyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzyx_ = mkSwizz "xzyx"

yzyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzyx_ = mkSwizz "yzyx"

zzyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzyx_ = mkSwizz "zzyx"

wzyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzyx_ = mkSwizz "wzyx"

zyx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zyx_ = mkSwizz "zyx"

xwyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwyx_ = mkSwizz "xwyx"

ywyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywyx_ = mkSwizz "ywyx"

zwyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwyx_ = mkSwizz "zwyx"

wwyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwyx_ = mkSwizz "wwyx"

wyx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wyx_ = mkSwizz "wyx"

yx_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 2
yx_ = mkSwizz "yx"

xxzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xxzx_ = mkSwizz "xxzx"

yxzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yxzx_ = mkSwizz "yxzx"

zxzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxzx_ = mkSwizz "zxzx"

wxzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxzx_ = mkSwizz "wxzx"

xzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
xzx_ = mkSwizz "xzx"

xyzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xyzx_ = mkSwizz "xyzx"

yyzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yyzx_ = mkSwizz "yyzx"

zyzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyzx_ = mkSwizz "zyzx"

wyzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyzx_ = mkSwizz "wyzx"

yzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
yzx_ = mkSwizz "yzx"

xzzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzzx_ = mkSwizz "xzzx"

yzzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzzx_ = mkSwizz "yzzx"

zzzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzzx_ = mkSwizz "zzzx"

wzzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzzx_ = mkSwizz "wzzx"

zzx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zzx_ = mkSwizz "zzx"

xwzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwzx_ = mkSwizz "xwzx"

ywzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywzx_ = mkSwizz "ywzx"

zwzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwzx_ = mkSwizz "zwzx"

wwzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwzx_ = mkSwizz "wwzx"

wzx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wzx_ = mkSwizz "wzx"

zx_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 2
zx_ = mkSwizz "zx"

xxwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxwx_ = mkSwizz "xxwx"

yxwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxwx_ = mkSwizz "yxwx"

zxwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxwx_ = mkSwizz "zxwx"

wxwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxwx_ = mkSwizz "wxwx"

xwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xwx_ = mkSwizz "xwx"

xywx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xywx_ = mkSwizz "xywx"

yywx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yywx_ = mkSwizz "yywx"

zywx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zywx_ = mkSwizz "zywx"

wywx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wywx_ = mkSwizz "wywx"

ywx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
ywx_ = mkSwizz "ywx"

xzwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzwx_ = mkSwizz "xzwx"

yzwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzwx_ = mkSwizz "yzwx"

zzwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzwx_ = mkSwizz "zzwx"

wzwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzwx_ = mkSwizz "wzwx"

zwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zwx_ = mkSwizz "zwx"

xwwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwwx_ = mkSwizz "xwwx"

ywwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywwx_ = mkSwizz "ywwx"

zwwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwwx_ = mkSwizz "zwwx"

wwwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwwx_ = mkSwizz "wwwx"

wwx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wwx_ = mkSwizz "wwx"

wx_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
wx_ = mkSwizz "wx"

x_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 1
x_ = mkSwizz "x"

xxxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xxxy_ = mkSwizz "xxxy"

yxxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yxxy_ = mkSwizz "yxxy"

zxxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxxy_ = mkSwizz "zxxy"

wxxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxxy_ = mkSwizz "wxxy"

xxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
xxy_ = mkSwizz "xxy"

xyxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xyxy_ = mkSwizz "xyxy"

yyxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yyxy_ = mkSwizz "yyxy"

zyxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyxy_ = mkSwizz "zyxy"

wyxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyxy_ = mkSwizz "wyxy"

yxy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
yxy_ = mkSwizz "yxy"

xzxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzxy_ = mkSwizz "xzxy"

yzxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzxy_ = mkSwizz "yzxy"

zzxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzxy_ = mkSwizz "zzxy"

wzxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzxy_ = mkSwizz "wzxy"

zxy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zxy_ = mkSwizz "zxy"

xwxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwxy_ = mkSwizz "xwxy"

ywxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywxy_ = mkSwizz "ywxy"

zwxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwxy_ = mkSwizz "zwxy"

wwxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwxy_ = mkSwizz "wwxy"

wxy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wxy_ = mkSwizz "wxy"

xy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 2
xy_ = mkSwizz "xy"

xxyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xxyy_ = mkSwizz "xxyy"

yxyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yxyy_ = mkSwizz "yxyy"

zxyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxyy_ = mkSwizz "zxyy"

wxyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxyy_ = mkSwizz "wxyy"

xyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
xyy_ = mkSwizz "xyy"

xyyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
xyyy_ = mkSwizz "xyyy"

yyyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 4
yyyy_ = mkSwizz "yyyy"

zyyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyyy_ = mkSwizz "zyyy"

wyyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyyy_ = mkSwizz "wyyy"

yyy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 3
yyy_ = mkSwizz "yyy"

xzyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzyy_ = mkSwizz "xzyy"

yzyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzyy_ = mkSwizz "yzyy"

zzyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzyy_ = mkSwizz "zzyy"

wzyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzyy_ = mkSwizz "wzyy"

zyy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zyy_ = mkSwizz "zyy"

xwyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwyy_ = mkSwizz "xwyy"

ywyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywyy_ = mkSwizz "ywyy"

zwyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwyy_ = mkSwizz "zwyy"

wwyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwyy_ = mkSwizz "wwyy"

wyy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wyy_ = mkSwizz "wyy"

yy_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 2
yy_ = mkSwizz "yy"

xxzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xxzy_ = mkSwizz "xxzy"

yxzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yxzy_ = mkSwizz "yxzy"

zxzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxzy_ = mkSwizz "zxzy"

wxzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxzy_ = mkSwizz "wxzy"

xzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
xzy_ = mkSwizz "xzy"

xyzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xyzy_ = mkSwizz "xyzy"

yyzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yyzy_ = mkSwizz "yyzy"

zyzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyzy_ = mkSwizz "zyzy"

wyzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyzy_ = mkSwizz "wyzy"

yzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
yzy_ = mkSwizz "yzy"

xzzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzzy_ = mkSwizz "xzzy"

yzzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzzy_ = mkSwizz "yzzy"

zzzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzzy_ = mkSwizz "zzzy"

wzzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzzy_ = mkSwizz "wzzy"

zzy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zzy_ = mkSwizz "zzy"

xwzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwzy_ = mkSwizz "xwzy"

ywzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywzy_ = mkSwizz "ywzy"

zwzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwzy_ = mkSwizz "zwzy"

wwzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwzy_ = mkSwizz "wwzy"

wzy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wzy_ = mkSwizz "wzy"

zy_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 2
zy_ = mkSwizz "zy"

xxwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxwy_ = mkSwizz "xxwy"

yxwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxwy_ = mkSwizz "yxwy"

zxwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxwy_ = mkSwizz "zxwy"

wxwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxwy_ = mkSwizz "wxwy"

xwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xwy_ = mkSwizz "xwy"

xywy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xywy_ = mkSwizz "xywy"

yywy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yywy_ = mkSwizz "yywy"

zywy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zywy_ = mkSwizz "zywy"

wywy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wywy_ = mkSwizz "wywy"

ywy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
ywy_ = mkSwizz "ywy"

xzwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzwy_ = mkSwizz "xzwy"

yzwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzwy_ = mkSwizz "yzwy"

zzwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzwy_ = mkSwizz "zzwy"

wzwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzwy_ = mkSwizz "wzwy"

zwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zwy_ = mkSwizz "zwy"

xwwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwwy_ = mkSwizz "xwwy"

ywwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywwy_ = mkSwizz "ywwy"

zwwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwwy_ = mkSwizz "zwwy"

wwwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwwy_ = mkSwizz "wwwy"

wwy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wwy_ = mkSwizz "wwy"

wy_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
wy_ = mkSwizz "wy"

y_ :: forall n. (Veccable n, n >= 2) => Vec n -> Vec 1
y_ = mkSwizz "y"

xxxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xxxz_ = mkSwizz "xxxz"

yxxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yxxz_ = mkSwizz "yxxz"

zxxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxxz_ = mkSwizz "zxxz"

wxxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxxz_ = mkSwizz "wxxz"

xxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
xxz_ = mkSwizz "xxz"

xyxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xyxz_ = mkSwizz "xyxz"

yyxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yyxz_ = mkSwizz "yyxz"

zyxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyxz_ = mkSwizz "zyxz"

wyxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyxz_ = mkSwizz "wyxz"

yxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
yxz_ = mkSwizz "yxz"

xzxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzxz_ = mkSwizz "xzxz"

yzxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzxz_ = mkSwizz "yzxz"

zzxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzxz_ = mkSwizz "zzxz"

wzxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzxz_ = mkSwizz "wzxz"

zxz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zxz_ = mkSwizz "zxz"

xwxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwxz_ = mkSwizz "xwxz"

ywxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywxz_ = mkSwizz "ywxz"

zwxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwxz_ = mkSwizz "zwxz"

wwxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwxz_ = mkSwizz "wwxz"

wxz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wxz_ = mkSwizz "wxz"

xz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 2
xz_ = mkSwizz "xz"

xxyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xxyz_ = mkSwizz "xxyz"

yxyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yxyz_ = mkSwizz "yxyz"

zxyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxyz_ = mkSwizz "zxyz"

wxyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxyz_ = mkSwizz "wxyz"

xyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
xyz_ = mkSwizz "xyz"

xyyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xyyz_ = mkSwizz "xyyz"

yyyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yyyz_ = mkSwizz "yyyz"

zyyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyyz_ = mkSwizz "zyyz"

wyyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyyz_ = mkSwizz "wyyz"

yyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
yyz_ = mkSwizz "yyz"

xzyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzyz_ = mkSwizz "xzyz"

yzyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzyz_ = mkSwizz "yzyz"

zzyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzyz_ = mkSwizz "zzyz"

wzyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzyz_ = mkSwizz "wzyz"

zyz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zyz_ = mkSwizz "zyz"

xwyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwyz_ = mkSwizz "xwyz"

ywyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywyz_ = mkSwizz "ywyz"

zwyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwyz_ = mkSwizz "zwyz"

wwyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwyz_ = mkSwizz "wwyz"

wyz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wyz_ = mkSwizz "wyz"

yz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 2
yz_ = mkSwizz "yz"

xxzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xxzz_ = mkSwizz "xxzz"

yxzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yxzz_ = mkSwizz "yxzz"

zxzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zxzz_ = mkSwizz "zxzz"

wxzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxzz_ = mkSwizz "wxzz"

xzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
xzz_ = mkSwizz "xzz"

xyzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xyzz_ = mkSwizz "xyzz"

yyzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yyzz_ = mkSwizz "yyzz"

zyzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zyzz_ = mkSwizz "zyzz"

wyzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyzz_ = mkSwizz "wyzz"

yzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
yzz_ = mkSwizz "yzz"

xzzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
xzzz_ = mkSwizz "xzzz"

yzzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
yzzz_ = mkSwizz "yzzz"

zzzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 4
zzzz_ = mkSwizz "zzzz"

wzzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzzz_ = mkSwizz "wzzz"

zzz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 3
zzz_ = mkSwizz "zzz"

xwzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwzz_ = mkSwizz "xwzz"

ywzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywzz_ = mkSwizz "ywzz"

zwzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwzz_ = mkSwizz "zwzz"

wwzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwzz_ = mkSwizz "wwzz"

wzz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wzz_ = mkSwizz "wzz"

zz_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 2
zz_ = mkSwizz "zz"

xxwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxwz_ = mkSwizz "xxwz"

yxwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxwz_ = mkSwizz "yxwz"

zxwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxwz_ = mkSwizz "zxwz"

wxwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxwz_ = mkSwizz "wxwz"

xwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xwz_ = mkSwizz "xwz"

xywz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xywz_ = mkSwizz "xywz"

yywz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yywz_ = mkSwizz "yywz"

zywz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zywz_ = mkSwizz "zywz"

wywz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wywz_ = mkSwizz "wywz"

ywz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
ywz_ = mkSwizz "ywz"

xzwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzwz_ = mkSwizz "xzwz"

yzwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzwz_ = mkSwizz "yzwz"

zzwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzwz_ = mkSwizz "zzwz"

wzwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzwz_ = mkSwizz "wzwz"

zwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zwz_ = mkSwizz "zwz"

xwwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwwz_ = mkSwizz "xwwz"

ywwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywwz_ = mkSwizz "ywwz"

zwwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwwz_ = mkSwizz "zwwz"

wwwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwwz_ = mkSwizz "wwwz"

wwz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wwz_ = mkSwizz "wwz"

wz_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
wz_ = mkSwizz "wz"

z_ :: forall n. (Veccable n, n >= 3) => Vec n -> Vec 1
z_ = mkSwizz "z"

xxxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxxw_ = mkSwizz "xxxw"

yxxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxxw_ = mkSwizz "yxxw"

zxxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxxw_ = mkSwizz "zxxw"

wxxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxxw_ = mkSwizz "wxxw"

xxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xxw_ = mkSwizz "xxw"

xyxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xyxw_ = mkSwizz "xyxw"

yyxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yyxw_ = mkSwizz "yyxw"

zyxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zyxw_ = mkSwizz "zyxw"

wyxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyxw_ = mkSwizz "wyxw"

yxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
yxw_ = mkSwizz "yxw"

xzxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzxw_ = mkSwizz "xzxw"

yzxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzxw_ = mkSwizz "yzxw"

zzxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzxw_ = mkSwizz "zzxw"

wzxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzxw_ = mkSwizz "wzxw"

zxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zxw_ = mkSwizz "zxw"

xwxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwxw_ = mkSwizz "xwxw"

ywxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywxw_ = mkSwizz "ywxw"

zwxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwxw_ = mkSwizz "zwxw"

wwxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwxw_ = mkSwizz "wwxw"

wxw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wxw_ = mkSwizz "wxw"

xw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
xw_ = mkSwizz "xw"

xxyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxyw_ = mkSwizz "xxyw"

yxyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxyw_ = mkSwizz "yxyw"

zxyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxyw_ = mkSwizz "zxyw"

wxyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxyw_ = mkSwizz "wxyw"

xyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xyw_ = mkSwizz "xyw"

xyyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xyyw_ = mkSwizz "xyyw"

yyyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yyyw_ = mkSwizz "yyyw"

zyyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zyyw_ = mkSwizz "zyyw"

wyyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyyw_ = mkSwizz "wyyw"

yyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
yyw_ = mkSwizz "yyw"

xzyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzyw_ = mkSwizz "xzyw"

yzyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzyw_ = mkSwizz "yzyw"

zzyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzyw_ = mkSwizz "zzyw"

wzyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzyw_ = mkSwizz "wzyw"

zyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zyw_ = mkSwizz "zyw"

xwyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwyw_ = mkSwizz "xwyw"

ywyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywyw_ = mkSwizz "ywyw"

zwyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwyw_ = mkSwizz "zwyw"

wwyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwyw_ = mkSwizz "wwyw"

wyw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wyw_ = mkSwizz "wyw"

yw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
yw_ = mkSwizz "yw"

xxzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxzw_ = mkSwizz "xxzw"

yxzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxzw_ = mkSwizz "yxzw"

zxzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxzw_ = mkSwizz "zxzw"

wxzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxzw_ = mkSwizz "wxzw"

xzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xzw_ = mkSwizz "xzw"

xyzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xyzw_ = mkSwizz "xyzw"

yyzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yyzw_ = mkSwizz "yyzw"

zyzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zyzw_ = mkSwizz "zyzw"

wyzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyzw_ = mkSwizz "wyzw"

yzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
yzw_ = mkSwizz "yzw"

xzzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzzw_ = mkSwizz "xzzw"

yzzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzzw_ = mkSwizz "yzzw"

zzzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzzw_ = mkSwizz "zzzw"

wzzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzzw_ = mkSwizz "wzzw"

zzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zzw_ = mkSwizz "zzw"

xwzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwzw_ = mkSwizz "xwzw"

ywzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywzw_ = mkSwizz "ywzw"

zwzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwzw_ = mkSwizz "zwzw"

wwzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwzw_ = mkSwizz "wwzw"

wzw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
wzw_ = mkSwizz "wzw"

zw_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
zw_ = mkSwizz "zw"

xxww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xxww_ = mkSwizz "xxww"

yxww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yxww_ = mkSwizz "yxww"

zxww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zxww_ = mkSwizz "zxww"

wxww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wxww_ = mkSwizz "wxww"

xww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
xww_ = mkSwizz "xww"

xyww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xyww_ = mkSwizz "xyww"

yyww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yyww_ = mkSwizz "yyww"

zyww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zyww_ = mkSwizz "zyww"

wyww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wyww_ = mkSwizz "wyww"

yww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
yww_ = mkSwizz "yww"

xzww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xzww_ = mkSwizz "xzww"

yzww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
yzww_ = mkSwizz "yzww"

zzww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zzww_ = mkSwizz "zzww"

wzww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wzww_ = mkSwizz "wzww"

zww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
zww_ = mkSwizz "zww"

xwww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
xwww_ = mkSwizz "xwww"

ywww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
ywww_ = mkSwizz "ywww"

zwww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
zwww_ = mkSwizz "zwww"

wwww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 4
wwww_ = mkSwizz "wwww"

www_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 3
www_ = mkSwizz "www"

ww_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 2
ww_ = mkSwizz "ww"

w_ :: forall n. (Veccable n, n >= 4) => Vec n -> Vec 1
w_ = mkSwizz "w"


