module Game.ViewBob
  ( bobOffset
  , bobSpeed
  , bobAmplitude
  , bobDecayRate
  , bobMovementThreshold
  ) where

-- | Speed multiplier for advancing bob time while walking.
bobSpeed :: Float
bobSpeed = 12.0

-- | Vertical amplitude of the view bob oscillation.
bobAmplitude :: Float
bobAmplitude = 0.05

-- | Rate at which bob time decays toward zero when the player stops moving.
bobDecayRate :: Float
bobDecayRate = 8.0

-- | Minimum horizontal velocity magnitude to trigger view bobbing.
bobMovementThreshold :: Float
bobMovementThreshold = 0.1

-- | Pure function: compute camera Y offset from the current bob time.
--   Returns @sin(bobTime) * bobAmplitude@.
bobOffset :: Float -> Float
bobOffset t = sin t * bobAmplitude
