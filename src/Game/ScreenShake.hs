module Game.ScreenShake
  ( ScreenShake(..)
  , noShake
  , triggerShake
  , tickShake
  , shakeOffset
  , shakeDuration
  , shakeIntensityScale
  ) where

import Linear (V3(..))

-- | Duration in seconds for the shake to fully decay.
shakeDuration :: Float
shakeDuration = 0.3

-- | Scale factor: intensity = damage * shakeIntensityScale.
shakeIntensityScale :: Float
shakeIntensityScale = 0.04

-- | Screen shake state.
--   @ssIntensity@ is the peak displacement in blocks.
--   @ssTimer@ counts down from 'shakeDuration' to 0.
--   @ssSeed@ provides deterministic variation per shake event.
data ScreenShake = ScreenShake
  { ssIntensity :: !Float
  , ssTimer     :: !Float
  , ssSeed      :: !Float
  } deriving stock (Show, Eq)

-- | No active screen shake.
noShake :: ScreenShake
noShake = ScreenShake 0 0 0

-- | Start a new shake whose intensity is proportional to the damage taken.
--   @damage@ should be the raw HP lost (positive integer cast to Float).
triggerShake :: Float -> ScreenShake
triggerShake damage =
  ScreenShake
    { ssIntensity = damage * shakeIntensityScale
    , ssTimer     = shakeDuration
    , ssSeed      = damage  -- deterministic seed derived from damage
    }

-- | Advance the shake timer by @dt@ seconds and return the updated state
--   together with the camera offset to apply this frame.
--   The offset decays linearly over 'shakeDuration'.
tickShake :: Float -> ScreenShake -> (ScreenShake, V3 Float)
tickShake dt shake
  | ssTimer shake <= 0 = (noShake, V3 0 0 0)
  | otherwise =
      let newTimer = max 0 (ssTimer shake - dt)
          updatedShake = shake { ssTimer = newTimer }
          offset = shakeOffset updatedShake
      in (updatedShake, offset)

-- | Compute the current camera offset from a 'ScreenShake' state.
--   Uses a high-frequency sine driven by the remaining timer and seed
--   to produce a jittery displacement that decays linearly to zero.
shakeOffset :: ScreenShake -> V3 Float
shakeOffset shake
  | ssTimer shake <= 0 = V3 0 0 0
  | otherwise =
      let t         = ssTimer shake
          decay     = t / shakeDuration  -- 1.0 at start, 0.0 at end
          freq      = 30.0               -- oscillation frequency
          seed      = ssSeed shake
          magnitude = ssIntensity shake * decay
          dx        = magnitude * sin (t * freq + seed)
          dy        = magnitude * sin (t * freq * 1.3 + seed + 1.0)
          dz        = magnitude * sin (t * freq * 0.7 + seed + 2.0)
      in V3 dx dy dz
