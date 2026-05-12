module UI.ArmorBar
  ( armorBarVerts
  ) where

-- | Generate HUD vertex data for the armor bar.
--   Takes armorPoints (0-20), x position, and y position in NDC.
--   Returns a flat list of floats (vec2 pos + vec4 color per vertex).
--   Each 2 armor points produces one full gray shield quad.
--   Odd armor points produce a half-width shield for the last icon.
--   Color is RGBA (0.7, 0.7, 0.7, 0.8). Hidden (empty list) when armor <= 0.
armorBarVerts :: Int -> Float -> Float -> [Float]
armorBarVerts armorPoints x y
  | armorPoints <= 0 = []
  | otherwise        = concatMap (shieldQuad x y) [0 .. fullIcons - 1]
                    ++ halfQuad
  where
    clamped   = min 20 (max 0 armorPoints)
    fullIcons = clamped `div` 2
    hasHalf   = odd clamped
    halfQuad  = if hasHalf
                then halfShieldQuad x y fullIcons
                else []

-- | Width of each full shield icon in NDC.
shieldWidth :: Float
shieldWidth = 0.025

-- | Height of each shield icon in NDC.
shieldHeight :: Float
shieldHeight = 0.025

-- | Gap between shield icons in NDC.
shieldGap :: Float
shieldGap = 0.003

-- | Shield color: gray with some transparency.
shieldColor :: (Float, Float, Float, Float)
shieldColor = (0.7, 0.7, 0.7, 0.8)

-- | Render a full shield quad at the given icon index.
shieldQuad :: Float -> Float -> Int -> [Float]
shieldQuad baseX baseY idx =
  let x0 = baseX + fromIntegral idx * (shieldWidth + shieldGap)
      x1 = x0 + shieldWidth
      y0 = baseY
      y1 = baseY + shieldHeight
      (r, g, b, a) = shieldColor
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]

-- | Render a half-width shield quad for odd armor values.
halfShieldQuad :: Float -> Float -> Int -> [Float]
halfShieldQuad baseX baseY idx =
  let x0 = baseX + fromIntegral idx * (shieldWidth + shieldGap)
      x1 = x0 + shieldWidth * 0.5
      y0 = baseY
      y1 = baseY + shieldHeight
      (r, g, b, a) = shieldColor
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]
