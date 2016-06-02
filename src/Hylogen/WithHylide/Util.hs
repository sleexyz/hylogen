module Hylogen.WithHylide.Util where

import Hylogen

black :: Vec3
black = vec3 (0, 0, 0)

white :: Vec3
white = vec3 (1, 1, 1)

-- | Given an alpha value, sets it for the alpha channel for a given color
--
-- @
-- setAlpha alpha color -- == n newColor
-- @
setAlpha :: Vec1 -> Vec4 -> Vec4
setAlpha alpha v = vec4 (xyz_ v, alpha)

-- TODO: hsl


-- | Linear to exponential map
--
-- @
-- linexp (a, b, c, d) a           -- == c
-- linexp (a, b, c, d) b           -- == d
-- linexp (a, b, c, d) ((a + b)\/2) -- == c * sqrt(d/c)
-- @
linexp :: (Floating a) => (a, a, a, a) -> a -> a
linexp (a, b, c, d) x = c * ((d / c) ** ((x - a) / (b - a)))

-- | Linear to linear map
--
-- @
-- linexp (a, b, c, d) a           -- == c
-- linexp (a, b, c, d) b           -- == d
-- linexp (a, b, c, d) ((a + b)\/2) -- == ((c + d)/2)
-- @
linlin :: (Floating a) => (a, a, a, a) -> a -> a
linlin (a, b, c, d) x = c + (d - c) * ((x - a) / (b - a))
