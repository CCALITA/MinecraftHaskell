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

-- | Left edge of the hotbar in Vulkan NDC (range -1..1).
hotbarX0 :: Float
hotbarX0 = -0.45

-- | Y position of the hotbar in Vulkan NDC.
hotbarY :: Float
hotbarY = 0.85

-- | Width and height of a single hotbar slot in NDC units.
slotSize :: Float
slotSize = 0.09

-- | Left edge of the health bar in Vulkan NDC.
healthBarX :: Float
healthBarX = -0.45

-- | Y position of the health bar in Vulkan NDC.
healthBarY :: Float
healthBarY = 0.74

-- | Left edge of the hunger bar in Vulkan NDC.
hungerBarX :: Float
hungerBarX = 0.05

-- | Y position of the experience bar in Vulkan NDC.
xpBarY :: Float
xpBarY = 0.82

-- | Half-size of the crosshair in NDC units.
crosshairSize :: Float
crosshairSize = 0.02
