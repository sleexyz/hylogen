{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Types


time = fromVec1 $ V1u "time"
fract = fromVec1 . V1uop "fract"




uv = V2u "uv"
mouse = V2u "mouse"


-- coord_ = V4u "gl_FragCoord"
audio = V4u "audio"

getPixel = V4FromTexture

backbuffer = TextureUniform "backbuffer"
