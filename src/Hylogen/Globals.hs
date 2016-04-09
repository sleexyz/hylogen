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

clamp :: (HyloPrim a) => a -> a -> a -> a
clamp x y z = (z `min_` y) `max_` x


linexp :: (Vec1, Vec1, Vec1, Vec1) -> Vec1 -> Vec1
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

linlin :: (Vec1, Vec1, Vec1, Vec1) -> Vec1 -> Vec1
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))


uv = V2u "uv()"
uvN = V2u "uvN"
mouse = V2u "mouse"


-- coord_ = V4u "gl_FragCoord"
audio = V4u "audio"

backBuffer = Tu "backBuffer"

-- | Booly's

true = Bu "true"
false = Bu "false"

eq :: (HyloPrim v) => v -> v -> Booly
eq = Bcomp "=="

neq :: (HyloPrim v) => v -> v -> Booly
neq = Bcomp "!="

lt :: (HyloPrim v) => v -> v -> Booly
lt = Bcomp "<"

gt :: (HyloPrim v) => v -> v -> Booly
gt = Bcomp ">"

leq :: (HyloPrim v) => v -> v -> Booly
leq = Bcomp "<="

geq :: (HyloPrim v) => v -> v -> Booly
geq = Bcomp ">="
