module UI.MiningSpeed
  ( miningTimeText
  , miningProgressVerts
  ) where

import Game.Item (ToolMaterial, toolMiningSpeed, ToolType)
import Numeric (showFFloat)

-- | Format a mining duration in seconds as a human-readable string.
--   Always shows exactly one decimal place followed by "s".
--   Negative values are clamped to 0.0.
--
-- >>> miningTimeText 2.53
-- "2.5s"
-- >>> miningTimeText 0
-- "0.0s"
miningTimeText :: Float -> String
miningTimeText seconds =
  let clamped = max 0 seconds
  in showFFloat (Just 1) clamped "s"

-- | Generate HUD vertex data for a mining progress bar.
--   Takes progress (0-1, clamped), x position, and y position in NDC.
--   Returns a flat list of floats (vec2 pos + vec4 color per vertex).
--   The bar is drawn as a single filled quad whose width scales with progress.
--   Returns an empty list when progress <= 0.
--   Color transitions from red (0%) through yellow (50%) to green (100%).
miningProgressVerts :: Float -> Float -> Float -> [Float]
miningProgressVerts progress x y
  | clampedProgress <= 0 = []
  | otherwise            = barQuad clampedProgress x y
  where
    clampedProgress = min 1 (max 0 progress)

-- | Full width of the mining progress bar in NDC.
barWidth :: Float
barWidth = 0.3

-- | Height of the mining progress bar in NDC.
barHeight :: Float
barHeight = 0.02

-- | Compute the bar color based on progress (0-1).
--   0.0 = red (1,0,0), 0.5 = yellow (1,1,0), 1.0 = green (0,1,0).
barColor :: Float -> (Float, Float, Float, Float)
barColor p
  | p <= 0.5  = (1.0, p * 2.0, 0.0, 0.8)
  | otherwise = (1.0 - (p - 0.5) * 2.0, 1.0, 0.0, 0.8)

-- | Render a single progress bar quad.
barQuad :: Float -> Float -> Float -> [Float]
barQuad progress baseX baseY =
  let x0 = baseX
      x1 = baseX + barWidth * progress
      y0 = baseY
      y1 = baseY + barHeight
      (r, g, b, a) = barColor progress
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]
