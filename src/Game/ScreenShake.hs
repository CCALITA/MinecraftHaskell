module Game.ScreenShake
  ( shakeOffset
  , shakeFromFallDamage
  , shakeDuration
  ) where

-- | Duration of a screen shake effect in seconds.
shakeDuration :: Float
shakeDuration = 0.3

-- | Compute a camera offset (x, y) from shake intensity and elapsed time.
--   Uses sin-based oscillation that decays over 'shakeDuration'.
--   Returns (0, 0) when time exceeds duration or intensity is zero.
shakeOffset :: Float -> Float -> (Float, Float)
shakeOffset intensity time
  | time < 0 || time >= shakeDuration || intensity <= 0 = (0, 0)
  | otherwise =
      let decay = 1.0 - (time / shakeDuration)
          freq  = 30.0
          x = intensity * decay * sin (freq * time)
          y = intensity * decay * sin (freq * time * 1.3)
      in (x, y)

-- | Convert fall damage (integer hit points) to shake intensity.
--   Returns 0 for non-positive damage.
shakeFromFallDamage :: Int -> Float
shakeFromFallDamage dmg
  | dmg <= 0  = 0
  | otherwise = 0.01 * fromIntegral dmg
