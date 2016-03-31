{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Types


-- | Vec1:

pi = V1u "pi"
time = V1u "time"
-- rand_ = V1uop "rand"


fract :: (ToVec1 a) => a -> Vec1
fract = V1uop "fract"




-- | Vec2:

uv = V2u "uv"


-- coord_ = V4u "gl_FragCoord"
