{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hylogen.Texture where

import Hylogen.Expr
import Hylogen.Vec

data TextureType = TextureType
instance ToGLSLType TextureType where
  toGLSLType _ = GLSLBool

type Texture = Expr TextureType

tu :: String -> Texture
tu str = Expr TextureType (Tree (Uniform, toGLSLType TextureType, str) [])


texture2D :: Texture -> Vec2 -> Vec4
texture2D t uv = Expr (FloatVec :: FloatVec 4) (Tree (Op2Pre, GLSLVec4, "texture2D") [toMono t, toMono uv])
