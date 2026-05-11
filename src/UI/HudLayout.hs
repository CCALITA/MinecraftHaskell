module UI.HudLayout
  ( hotbarX0
  , hotbarY
  , slotSize
  , healthBarX
  , healthBarY
  , hungerBarX
  , xpBarY
  , crosshairSize
  ) where

-- | Left edge of the hotbar in NDC.
hotbarX0 :: Float
hotbarX0 = -0.45

-- | Y position of the hotbar in NDC.
hotbarY :: Float
hotbarY = 0.85

-- | Width/height of a single hotbar slot in NDC.
slotSize :: Float
slotSize = 0.09

-- | Left edge of the health bar in NDC.
healthBarX :: Float
healthBarX = -0.45

-- | Y position of the health bar in NDC.
healthBarY :: Float
healthBarY = 0.74

-- | Left edge of the hunger bar in NDC.
hungerBarX :: Float
hungerBarX = 0.05

-- | Y position of the XP bar in NDC.
xpBarY :: Float
xpBarY = 0.82

-- | Half-size of the crosshair in NDC.
crosshairSize :: Float
crosshairSize = 0.02
