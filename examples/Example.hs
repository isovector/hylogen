module Example where
import Hylogen.WithHylide

output :: Program
output = toProgram color



color :: Vec4
color = vec4 (c1, 1)
  where
    uv' = uv + vec2(sin(y_ uv * 10 + time), 0)
    c1 = voronoi $ vec3 (uv' * vec2(4, 4) + vec2(time, time), 1)
    -- bb = xyz_ $ texture2D backBuffer uv

