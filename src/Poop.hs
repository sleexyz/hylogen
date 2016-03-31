{-# LANGUAGE DeriveFunctor#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hylogen where

import GHC.TypeLits
import Free (Free(In, Pure), liftF)
import Data.Monoid


-- class HasShape (n :: Nat) a
-- instance HasShape 2 Vec2

data HType (n :: Nat) = HType


data HExpr a where
  HVec1  :: Float -> HExpr a
  HVec2  :: a -> a -> HExpr a

  -- HVec3  :: a -> a -> a -> HExpr 3 a
  -- HVec4  :: a -> a -> a -> a -> HExpr 4 a
deriving instance Functor HExpr

transform :: Free HExpr a -> Free HExpr String
transform (Pure _) = Pure ""
transform (In x) = In (fmap transform x)


type Body n = Free HExpr

showHExpr :: HExpr String -> String
showHExpr expr = case expr of
  HVec1 x -> show x
  HVec2 x y -> "vec2(" <> x <> ", " <> y <> ")"
  -- HVec3 x y z -> "vec3(" <> x <> ", " <> y <> ", " <> z <> ")"
  -- HVec4 x y z w -> "vec4(" <> x <> ", " <> y <> ", " <> z <> ", " <> w <> ")"



cata :: forall f a. (Functor f) => (f a -> a) -> Free f a -> a
cata _ (Pure x) = x
cata alg (In x) = alg $ (cata alg) <$>  x



class ToHExpr n t where
  toHExpr :: t -> Free HExpr ()

instance ToHExpr n (Free (HExpr) ()) where
  toHExpr (Pure _) =  Pure ()
  toHExpr (In x) =  In x

-- instance ToHExpr 1 Float where
--   toHExpr = In . HFloat


-- testHExpr1 :: Free Expr a
testHExpr1 = In $ HVec2 (In $ HVec1 0.1)  (In $ HVec1 0.1)

-- testHExpr2 :: Body a
testHExpr2 = In $ HVec2 (In $ HVec2 (In $ HVec1 0.1) (In $ HVec1 0.2))  (In $ HVec1 0.1)






class IsVec a where
  type VecVal a
  toVec :: a -> VecVal a

instance












-- instance ToHExpr (Double, Double) where
--   toHExpr (x, y) = In $ Vec2 (In $ Float x, In $ Float y)

-- vec2 :: (ToHExpr i, ToHExpr i' ) => i ->  i' -> Free HExpr ()
-- vec2 x y = In $ Vec2 (toHExpr x, toHExpr y)

-- vec3 = In . Vec3
-- vec4 = In . Vec4

-- class Transformable a where
--   def :: a

--   transform :: Free HExpr b -> Free HExpr a
--   transform (Pure _) = Pure def
--   transform (In x) = In (transform <$> x)

-- instance Transformable () where def = ()
-- instance Transformable String where def = ""



-- toGLSL :: Free HExpr a -> String
-- toGLSL x = "void main() {\n"
--   <> "gl_FragColor = "
--   <> cata showHExpr (transform x)
--   <> ";"
--   <> "\n}"

-- priint :: Free HExpr a -> IO ()
-- priint = putStrLn . toGLSL


-- testHExpr :: Body ()
-- testHExpr = In $ BOp Mul (In $ Uni $ FromString "rand()" ) (In $ Float 0.1)


-- testHExpr2 :: Body ()
-- testHExpr2 = toHExpr 0.1

-- testHExpr3 :: Free HExpr ()
-- testHExpr3 =  do
--   a <- vec2 0.2 0.2
--   return a
