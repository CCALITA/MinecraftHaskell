module UI.HotbarAnimation
  ( hotbarSelectorX
  , selectorLerpSpeed
  ) where

-- | Interpolation speed for the hotbar selector slide animation.
--   Higher values produce a snappier transition.
selectorLerpSpeed :: Float
selectorLerpSpeed = 15.0

-- | Compute the lerped X position of the hotbar selector highlight.
--   Uses exponential lerp: currentX moves toward the target slot's X
--   position at a rate governed by 'selectorLerpSpeed' and the frame
--   delta time.
--
--   @targetSlot@ — the slot index (0-8) the player has selected
--   @currentX@   — the current rendered X position of the selector
--   @dt@         — frame delta time in seconds
--
--   Returns the new X position after one frame of interpolation.
--   When dt <= 0, the position is unchanged (no negative-time movement).
hotbarSelectorX :: Int -> Float -> Float -> Float
hotbarSelectorX targetSlot currentX dt
  | dt <= 0   = currentX
  | otherwise = currentX + (targetX - currentX) * clampedAlpha
  where
    targetX      = slotToX targetSlot
    alpha        = 1.0 - exp (negate selectorLerpSpeed * dt)
    clampedAlpha = min 1.0 (max 0.0 alpha)

-- | Convert a hotbar slot index to its NDC X position.
--   Slot 0 starts at -0.45, each slot is 0.1 units wide.
slotToX :: Int -> Float
slotToX slot = -0.45 + fromIntegral (clamp 0 8 slot) * 0.1

-- | Clamp an Int to the inclusive range [lo, hi].
clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)
