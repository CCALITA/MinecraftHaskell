module UI.EnchantGlow
  ( enchantGlowBorder
  , glowColor
  , glowThickness
  , isSlotEnchanted
  ) where

import qualified Data.Map.Strict as Map
import Game.Enchanting (Enchantment)

-- | Purple shimmer color (r, g, b, a) for enchanted item glow
glowColor :: (Float, Float, Float, Float)
glowColor = (0.7, 0.3, 1.0, 0.4)

-- | Thickness of the glow border in NDC units
glowThickness :: Float
glowThickness = 0.004

-- | Check whether a slot index has enchantments in the map
isSlotEnchanted :: Map.Map Int [Enchantment] -> Int -> Bool
isSlotEnchanted enchMap slot =
  case Map.lookup slot enchMap of
    Just (_:_) -> True
    _          -> False

-- | Generate 4 thin quads forming a purple shimmer border around a slot.
--   Takes slot top-left (x0, y0), width, height.
--   Returns a list of floats representing 4 quads (6 verts each, 6 floats per vert).
enchantGlowBorder :: Float -> Float -> Float -> Float -> [Float]
enchantGlowBorder x0 y0 w h =
  let t = glowThickness
      (r, g, b, a) = glowColor
      q lx ly rx ry =
        [ lx, ly, r, g, b, a,  rx, ly, r, g, b, a,  rx, ry, r, g, b, a
        , lx, ly, r, g, b, a,  rx, ry, r, g, b, a,  lx, ry, r, g, b, a ]
  in -- Top edge
     q x0 y0 (x0 + w) (y0 + t)
     -- Bottom edge
     ++ q x0 (y0 + h - t) (x0 + w) (y0 + h)
     -- Left edge
     ++ q x0 (y0 + t) (x0 + t) (y0 + h - t)
     -- Right edge
     ++ q (x0 + w - t) (y0 + t) (x0 + w) (y0 + h - t)
