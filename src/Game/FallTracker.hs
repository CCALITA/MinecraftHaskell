module Game.FallTracker
  ( wouldTakeFallDamage
  , fallDamageAmount
  , safeFallDistance
  ) where

-- | The maximum distance a player can fall without taking damage.
safeFallDistance :: Float
safeFallDistance = 3.0

-- | Returns True if falling the given distance would cause damage.
wouldTakeFallDamage :: Float -> Bool
wouldTakeFallDamage dist = dist > safeFallDistance

-- | Calculates the amount of fall damage for a given distance.
-- Returns 0 if the distance is within the safe range.
fallDamageAmount :: Float -> Int
fallDamageAmount dist
  | dist <= safeFallDistance = 0
  | otherwise = floor dist - 3
