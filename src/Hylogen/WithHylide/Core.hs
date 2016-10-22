-- | Primitives for use in Hylide
module Hylogen.WithHylide.Core where

import           Data.Monoid
import           Hylogen
import           Hylogen.Expr


-- Writes GLSL, without sharing
-- toGLSL' :: Vec4 -> String
-- toGLSL' v = unlines [ "void main() {"
--                     , "    gl_FragColor = " <> show v <> ";"
--                     , "}"
--                     ]



-- | Pixel coordinates
--
-- (0, 0) is the lower left corner
--
-- (1, 1) is the upper right corner
uv :: Vec2
uv = uniform "uv()"

-- | Pixel coordinates
--
-- (0, 0) is the center of the screen
--
-- (1, 1) is the upper right corner
uvN :: Vec2
uvN = uniform "uvN"

-- | Time in milliseconds since start of Hylide instance
time :: Vec1
time = uniform "time"

-- | Beat phase from 0 to 1
beat :: Vec1
beat = uniform "beat"


-- | Resolution of the screen
resolution :: Vec2
resolution = uniform "resolution"

-- | Mouse position
mouse :: Vec2
mouse = uniform "mouse"


-- | Intensity of audio input, split into 4 bands
--
-- (low, low-mid, mid, high)
audio :: Vec4
audio = uniform "audio"

-- | The last rendered frame, as a texture
backBuffer :: Texture
backBuffer = uniform "backBuffer"

channel1 :: Texture
channel1 = uniform "channel1"


osc0 :: Vec1
osc0 = uniform "osc0"
osc1 :: Vec1
osc1 = uniform "osc1"
osc2 :: Vec1
osc2 = uniform "osc2"
osc3 :: Vec1
osc3 = uniform "osc3"
osc4 :: Vec1
osc4 = uniform "osc4"
osc5 :: Vec1
osc5 = uniform "osc5"
osc6 :: Vec1
osc6 = uniform "osc6"
osc7 :: Vec1
osc7 = uniform "osc7"
osc8 :: Vec1
osc8 = uniform "osc8"
osc9 :: Vec1
osc9 = uniform "osc9"
osc10 :: Vec1
osc10 = uniform "osc10"
osc11 :: Vec1
osc11 = uniform "osc11"
osc12 :: Vec1
osc12 = uniform "osc12"
osc13 :: Vec1
osc13 = uniform "osc13"
osc14 :: Vec1
osc14 = uniform "osc14"
osc15 :: Vec1
osc15 = uniform "osc15"
