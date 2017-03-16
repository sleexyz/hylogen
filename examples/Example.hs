module Example where
import Hylogen.WithHylide

output :: Program
output = toProgram color

color :: Vec4
color = vec4 (a, a, a, 1)
  where
    k = 20
    f = (*k) . sin . (/k)
    a = sum [ cos (x_ uvN * f time + x_ mouse )
            , sin (y_ uvN * f time + y_ mouse )
            ]
