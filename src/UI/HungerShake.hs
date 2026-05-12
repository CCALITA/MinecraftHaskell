module UI.HungerShake
  ( hungerShakeOffset
  ) where

-- | Compute a screen-space jitter offset based on hunger level and time.
-- When hunger <= 3, produces a shaking effect. Otherwise returns (0, 0).
hungerShakeOffset :: Int -> Float -> (Float, Float)
hungerShakeOffset hunger time
  | hunger > 3  = (0.0, 0.0)
  | hunger < 0  = (0.0, 0.0)
  | otherwise   = (dx, dy)
  where
    intensity = fromIntegral (4 - hunger) / 4.0 :: Float
    dx = intensity * 2.0 * sin (time * 17.3)
    dy = intensity * 2.0 * sin (time * 23.7)
