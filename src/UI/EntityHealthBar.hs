module UI.EntityHealthBar
  ( entityHealthBarVerts
  ) where

-- | Generate HUD vertex data for an entity health bar.
--   screenX, screenY: position in NDC of the bar's bottom-left corner.
--   health: current health points (clamped to [0, maxHealth]).
--   maxHealth: maximum health points (must be > 0; if <= 0, returns []).
--   Returns a flat list of floats (vec2 pos + vec4 color per vertex).
--   Draws a red background bar at full width, then a green fill bar
--   proportional to health/maxHealth overlaid on top.
--   Each bar is two triangles (6 vertices, 36 floats).
--   Total output: 72 floats (background + fill), or empty when maxHealth <= 0.
entityHealthBarVerts :: Float -> Float -> Int -> Int -> [Float]
entityHealthBarVerts _screenX _screenY _health maxHealth
  | maxHealth <= 0 = []
entityHealthBarVerts screenX screenY health maxHealth =
  bgQuad ++ fillQuad
  where
    clampedHealth = max 0 (min health maxHealth)
    fraction      = fromIntegral clampedHealth / fromIntegral maxHealth
    fillW         = barWidth * fraction
    bgQuad        = coloredQuad screenX screenY barWidth barHeight bgR bgG bgB bgA
    fillQuad      = coloredQuad screenX screenY fillW   barHeight fgR fgG fgB fgA

-- | Width of the full health bar in NDC.
barWidth :: Float
barWidth = 0.12

-- | Height of the health bar in NDC.
barHeight :: Float
barHeight = 0.015

-- | Background color: dark red.
bgR, bgG, bgB, bgA :: Float
bgR = 0.6
bgG = 0.1
bgB = 0.1
bgA = 0.9

-- | Foreground (fill) color: bright green.
fgR, fgG, fgB, fgA :: Float
fgR = 0.2
fgG = 0.8
fgB = 0.2
fgA = 0.9

-- | Produce a quad as two triangles (6 vertices).
--   Each vertex is: x, y, r, g, b, a (6 floats).
coloredQuad :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> [Float]
coloredQuad x0 y0 w h r g b a =
  let x1 = x0 + w
      y1 = y0 + h
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]
