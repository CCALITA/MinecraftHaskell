module UI.CrosshairColor
  ( crosshairColor
  ) where

-- | Determine crosshair RGBA based on what the player is looking at.
--   White when no entity, green for passive mobs, red for hostile mobs.
crosshairColor :: Bool -> Bool -> (Float, Float, Float, Float)
crosshairColor False _     = (1.0, 1.0, 1.0, 1.0)  -- white: no entity
crosshairColor True  True  = (1.0, 0.2, 0.2, 1.0)  -- red:   hostile
crosshairColor True  False = (0.2, 1.0, 0.2, 1.0)  -- green: passive
