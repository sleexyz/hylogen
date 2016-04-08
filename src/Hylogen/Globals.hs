{-# LANGUAGE NoMonomorphismRestriction #-}

module Hylogen.Globals where

import Hylogen.Types


time = V1u "time"

inverseSqrt = vuop "inversesqrt"

fract = vuop "fract"
floor_ = vuop "floor"
ceil_ = vuop "ceil"
min_ = vbop "min"
max_ = vbop "max"

clamp x y z = (z `min_` y) `max_` x




uv = V2u "uv()"
uvN = V2u "uvN"
mouse = V2u "mouse"


-- coord_ = V4u "gl_FragCoord"
audio = V4u "audio"

backBuffer = Tu "backBuffer"
