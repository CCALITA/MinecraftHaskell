module UI.SaturationBar
  ( saturationBarVerts
  ) where

-- | Generate HUD vertex data for the saturation bar overlay.
--   Takes saturation (0-20), x position, and y position in NDC.
--   Returns a flat list of floats (vec2 pos + vec4 color per vertex).
--   Each unit of saturation produces one small yellow quad.
--   Color is RGBA (1.0, 0.9, 0.2, 0.6). Hidden (empty list) when saturation <= 0.
saturationBarVerts :: Float -> Float -> Float -> [Float]
saturationBarVerts saturation x y
  | saturation <= 0 = []
  | otherwise       = concatMap (drumstickQuad x y) [0 .. units - 1]
  where
    units = floor (min 20 (max 0 saturation)) :: Int

-- | Width of each saturation unit quad in NDC.
unitWidth :: Float
unitWidth = 0.025

-- | Height of each saturation unit quad in NDC.
unitHeight :: Float
unitHeight = 0.025

-- | Gap between saturation unit quads in NDC.
unitGap :: Float
unitGap = 0.003

-- | Render a single saturation drumstick quad at the given index offset.
drumstickQuad :: Float -> Float -> Int -> [Float]
drumstickQuad baseX baseY idx =
  let x0 = baseX + fromIntegral idx * (unitWidth + unitGap)
      x1 = x0 + unitWidth
      y0 = baseY
      y1 = baseY + unitHeight
      (r, g, b, a) = (1.0 :: Float, 0.9, 0.2, 0.6)
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]
