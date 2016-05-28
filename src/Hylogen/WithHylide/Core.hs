module Hylogen.WithHylide.Core where

import           Data.Monoid
import           Hylogen
import           Hylogen.Expr
import           Hylogen.Program  (toProgram)


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


-- TODO: flip these definitions! Normalized means ??
uv :: Vec2
uv = uniform "uv()"

uvN :: Vec2
uvN = uniform "uvN"

time :: Vec1
time = uniform "time"


resolution :: Vec2
resolution = uniform "resolution"

mouse :: Vec2
mouse = uniform "mouse"


audio :: Vec4
audio = uniform "audio"

backBuffer :: Texture
backBuffer = uniform "backBuffer"

channel1 :: Texture
channel1 = uniform "channel1"


-- | No sharing
toGLSL' :: Vec4 -> String
toGLSL' v = unlines [ "void main() {"
                    , "    gl_FragColor = " <> show v <> ";"
                    , "}"
                    ]


-- | sharing via Data.reify
toGLSL :: Vec4 -> String
toGLSL = show . toProgram . toMono
