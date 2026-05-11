module UI.BreathBar
  ( bubbleColor
  ) where

-- | Compute the RGBA color for a breath bar bubble.
--   airSupply: 0-15 (0 = drowning, 15 = full breath)
--   time: elapsed seconds (used for pulse animation)
--   index: bubble index (used for staggered animation)
--   Returns (R, G, B, A) tuple.
--   When airSupply < 3, bubbles pulse red; otherwise steady blue.
bubbleColor :: Float -> Float -> Int -> (Float, Float, Float, Float)
bubbleColor airSupply time index
  | airSupply < 3 = (1.0, 0.1, 0.1, alpha)
  | otherwise     = (0.2, 0.5, 1.0, 0.8)
  where
    -- Pulsing alpha for low-air warning, staggered per bubble
    phase = sin (time * 6.0 + fromIntegral index * 0.5)
    alpha = 0.5 + 0.4 * abs phase
