{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Types


-- | Vec1:

time :: Vec1
time = V1u "time"
-- rand_ = V1uop "rand"


fract :: Vec1 -> Vec1
fract = V1uop "fract"




-- | Vec2:

uv, mouse :: Vec2
uv = V2u "uv"
mouse = V2u "mouse"


-- coord_ = V4u "gl_FragCoord"
audio = V4u "audio"
