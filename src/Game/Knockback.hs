module Game.Knockback
  ( knockbackVelocity
  , defaultKnockback
  , sprintKnockback
  ) where

import Linear (V3(..), normalize)

-- | Default knockback strength applied on regular hit.
defaultKnockback :: Float
defaultKnockback = 0.4

-- | Knockback strength applied when attacker is sprinting.
sprintKnockback :: Float
sprintKnockback = 0.6

-- | Compute the knockback velocity applied to a target.
-- The result is a horizontal push away from the attacker plus an upward
-- component of 0.4. If attacker and target share the same XZ position,
-- the push direction defaults to +Z.
knockbackVelocity :: V3 Float -> V3 Float -> Float -> V3 Float
knockbackVelocity (V3 ax _ az) (V3 tx _ tz) strength =
  let dx = tx - ax
      dz = tz - az
      horiz = V3 dx 0 dz
      dir = if dx == 0 && dz == 0
              then V3 0 0 1  -- fallback direction
              else let V3 nx _ nz = normalize horiz
                   in V3 nx 0 nz
      V3 dirX _ dirZ = dir
  in V3 (dirX * strength) 0.4 (dirZ * strength)
