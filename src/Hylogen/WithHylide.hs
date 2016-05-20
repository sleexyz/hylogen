module Hylogen.WithHylide ( module Hylogen.WithHylide
                          , module Hylogen
                          ) where

import           Data.Monoid
import           Hylogen
import           Hylogen.Expr
import           Hylogen.Program (toProgram)

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
