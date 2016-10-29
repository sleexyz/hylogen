module Hylogen.WithHylide.Util where

import Hylogen
import Hylogen.Expr

-- | Given an alpha value, sets it for the alpha channel for a given color
--
-- @
-- setAlpha alpha color -- == n newColor
-- @
setAlpha :: Vec1 -> Vec4 -> Vec4
setAlpha alpha v = vec4 (xyz_ v, alpha)

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

hsv2rgb :: Vec4 -> Vec4
hsv2rgb v = vec4(op1pre "hsv2rgb" (xyz_ v) :: Vec3, w_ v :: Vec1)

rgb2hsv :: Vec4 -> Vec4
rgb2hsv v = vec4 (op1pre "rgb2hsv" (xyz_ v) :: Vec3, w_ v :: Vec1)
