module Game.Swimming
  ( swimmingSpeed
  , isSwimming
  , swimBobFrequency
  , swimBobAmplitude
  ) where

-- | Movement speed multiplier while swimming (slower than walking).
swimmingSpeed :: Float
swimmingSpeed = 1.5

-- | Determine if the player is swimming.
--   A player is swimming when submerged in water AND actively moving.
isSwimming :: Bool -> Bool -> Bool
isSwimming inWater moving = inWater && moving

-- | Oscillation frequency for swim bobbing (cycles per second).
swimBobFrequency :: Float
swimBobFrequency = 6.0

-- | Vertical amplitude of the swim bob effect.
swimBobAmplitude :: Float
swimBobAmplitude = 0.03
