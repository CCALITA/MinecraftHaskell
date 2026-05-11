module Game.InteractionCooldown
  ( canInteract
  , placeCooldown
  , doorCooldown
  ) where

-- | Check whether enough time has elapsed since the last interaction.
--   Returns 'True' when @now - lastTime >= cooldown@.
canInteract :: Float -> Float -> Float -> Bool
canInteract lastTime cooldown now = (now - lastTime) >= cooldown

-- | Cooldown in seconds for block placement.
placeCooldown :: Float
placeCooldown = 0.2

-- | Cooldown in seconds for door toggling.
doorCooldown :: Float
doorCooldown = 0.3
