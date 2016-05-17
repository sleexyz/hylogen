{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hylogen.Texture where

import Hylogen.Expr

data TextureType = TextureType
instance ToGLSLType TextureType where
  toGLSLType _ = GLSLBool
  tag = TextureType

type Texture = Expr TextureType
