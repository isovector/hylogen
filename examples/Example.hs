module Example where
import Hylogen.WithHylide
import Hylogen.Types.Vec

output :: Program
output = toProgram color



color :: Vec4
color = c4
  where
    uv' = uv + vec2(sin(y_ uv * 10 + time), 0)
    c1 = voronoi $ vec3 (uv' * vec2(4, 4) + vec2(time, time), 1)
    c2 = rgb2hsv $ vec4 (c1, 1)
    c3 = mkAssign "x" c2 time
    c4 = hsv2rgb c3
    -- bb = xyz_ $ texture2D backBuffer uv

