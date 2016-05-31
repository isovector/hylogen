module Hylogen.WithHylide.Util where

import Hylogen

-- Colors
black :: Vec3
black = vec3 (0, 0, 0)

white :: Vec3
white = vec3 (1, 1, 1)

setAlpha :: Vec1 -> Vec4 -> Vec4
setAlpha alpha v = vec4 (xyz_ v, alpha)

-- TODO: hsl
