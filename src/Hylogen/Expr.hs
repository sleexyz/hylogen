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
{-# LANGUAGE DeriveFunctor #-}


module Hylogen.Expr where

import Data.Reify

data GLSLType = GLSLFloat
              | GLSLVec2
              | GLSLVec3
              | GLSLVec4
              | GLSLBool
              | GLSLTexture
              deriving (Eq, Ord)

instance Show GLSLType where
  show x = case x of
    GLSLFloat -> "float"
    GLSLVec2 -> "vec2"
    GLSLVec3 -> "vec3"
    GLSLVec4 -> "vec4"
    GLSLBool -> "bool"
    GLSLTexture -> "sampler2D"

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
                deriving (Show)

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
  tag :: ty -- TODO: fill in!

uniform :: forall a
           . ToGLSLType a
           => String -> Expr a
uniform str = Expr t (Tree (Uniform, toGLSLType t, str) [])
  where t = tag :: a

op1 :: forall a b
       . (ToGLSLType a, ToGLSLType b)
       => String -> Expr a -> Expr b
op1 str a = Expr t (Tree (Op1, toGLSLType t, str) [toMono a])
  where t = tag :: b

op1'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a
op1'' str a = Expr t (Tree (Op1, toGLSLType t, str) [toMono a])
  where t = tag :: a

op1pre :: forall a b
          . (ToGLSLType a, ToGLSLType b)
          => String -> Expr a -> Expr b
op1pre str a = Expr t (Tree (Op1Pre, toGLSLType t, str) [toMono a])
  where t = tag :: b

op1pre'' :: forall a
          . (ToGLSLType a)
          => String -> Expr a -> Expr a
op1pre'' str a = Expr t (Tree (Op1Pre, toGLSLType t, str) [toMono a])
  where t = tag :: a

op2 :: forall a b c
       . (ToGLSLType a, ToGLSLType b, ToGLSLType c)
       => String -> Expr a -> Expr b -> Expr c
op2 str a b = Expr t (Tree (Op2, toGLSLType t, str) [toMono a, toMono b])
  where t = tag :: c

op2' :: forall a c
       . (ToGLSLType a, ToGLSLType c)
       => String -> Expr a -> Expr a -> Expr c
op2' str a b = Expr t (Tree (Op2, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: c

op2'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a -> Expr a
op2'' str a b = Expr t (Tree (Op2, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: a


op2pre :: forall a b c
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c)
          => String -> Expr a -> Expr b -> Expr c
op2pre str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) [toMono a, toMono b])
  where t = tag :: c

op2pre' :: forall a c
       . (ToGLSLType a, ToGLSLType c)
       => String -> Expr a -> Expr a -> Expr c
op2pre' str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: c

op2pre'' :: forall a
       . (ToGLSLType a)
       => String -> Expr a -> Expr a -> Expr a
op2pre'' str a b = Expr t (Tree (Op2Pre, toGLSLType t, str) (fmap toMono [a, b]))
  where t = tag :: a

op3pre :: forall a b c d
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c, ToGLSLType d)
          => String -> Expr a -> Expr b -> Expr c -> Expr d
op3pre str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) [toMono a, toMono b, toMono c])
  where t = tag :: d

op3pre' :: forall a d
          . (ToGLSLType a, ToGLSLType d)
          => String -> Expr a -> Expr a -> Expr a -> Expr d
op3pre' str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) (fmap toMono [a, b, c]))
  where t = tag :: d

op3pre'' :: forall a
          . (ToGLSLType a)
          => String -> Expr a -> Expr a -> Expr a -> Expr a
op3pre'' str a b c = Expr t (Tree (Op3Pre, toGLSLType t, str) (fmap toMono [a, b, c]))
  where t = tag :: a


op4pre :: forall a b c d e
          . (ToGLSLType a, ToGLSLType b, ToGLSLType c, ToGLSLType d, ToGLSLType e)
          => String -> Expr a -> Expr b -> Expr c -> Expr d -> Expr e
op4pre str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) [toMono a, toMono b, toMono c, toMono d])
  where t = tag :: e

op4pre' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a -> Expr a -> Expr e
op4pre' str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) (fmap toMono [a, b, c, d]))
  where t = tag :: e

op4pre'' :: forall a e
          . (ToGLSLType a, ToGLSLType e)
          => String -> Expr a -> Expr a -> Expr a -> Expr a -> Expr e
op4pre'' str a b c d = Expr t (Tree (Op4Pre, toGLSLType t, str) (fmap toMono [a, b, c, d]))
  where t = tag :: e




data TreeF a b = TreeF { getElemF     :: a
                       , getChildrenF   :: [Maybe b]
                       }
                 deriving (Functor)

type ExprMonoF = TreeF (ExprForm, GLSLType, String, [ExprMono])

emfStringAt :: (Show a) => ExprMonoF a -> Int -> String
emfStringAt (TreeF (_, _, _, xs) ys)  i = zipWith fn xs ys !! i
  where
    fn x Nothing = show x
    fn _ (Just y)= show y

instance (Show a) => Show (ExprMonoF a) where
  show expr@(TreeF (form, _, str, _) _) = case form of
    Uniform  -> str
    Variable -> str
    Op1      -> mconcat ["(", str, strAt 0, ")"]
    Op1Pre   -> mconcat [ str, "(", strAt 0, ")"]
    Op2      -> mconcat ["(", strAt 0, " ", str, " ", strAt 1, ")"]
    Op2Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ")"]
    Op3Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ")"]
    Op4Pre   -> mconcat [str, "(", strAt 0, ", ", strAt 1, ", ", strAt 2, ", ", strAt 3, ")"]
    Select   -> mconcat ["( ", strAt 0, " ? ", strAt 1, " : ", strAt 2, ")"]
    Access   -> mconcat [strAt 0, ".", str]
    where
      strAt = emfStringAt expr

-- instance MuRef ExprMono where
--   type DeRef ExprMono = ExprMonoF
--   mapDeRef f (Tree tup xs) = TreeF tup <$> traverse f xs

instance MuRef ExprMono where
  type DeRef ExprMono = ExprMonoF
  mapDeRef func (Tree (form, ty, str) xs) = TreeF (form, ty, str, xs) <$> g xs
    where
      g (x:xs) = (:) <$> (traverse func $ shouldShare x) <*> (g $  xs)
      g [] = pure []

      shouldShare :: ExprMono -> Maybe ExprMono
      shouldShare (Tree (Uniform, _, _) _) = Nothing
      shouldShare expr = Just expr
