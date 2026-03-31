module UI.HUD
  ( HUDState(..)
  , HUDElement(..)
  , CrosshairStyle(..)
  , newHUDState
  , updateHUD
  , hudElements
  , toggleDebugInfo
  ) where

import Game.Player (Player(..))
import Game.Inventory (Inventory(..), ItemStack(..), getHotbarSlot, hotbarSlots)
import Game.DayNight (DayNightCycle(..), getAmbientLight)
import Linear (V2(..), V3(..))

-- | Crosshair visual style
data CrosshairStyle = CrosshairDot | CrosshairCross | CrosshairNone
  deriving stock (Show, Eq)

-- | HUD element to render
data HUDElement
  = HECrosshair !CrosshairStyle !(V2 Float)          -- style, screen center
  | HEHotbar ![Maybe ItemStack] !Int                  -- slots, selected index
  | HEHealthBar !Float !Float                         -- current, max
  | HEHungerBar !Float !Float                         -- current, max (placeholder)
  | HEDebugText ![String]                             -- debug info lines
  | HEText !String !(V2 Float) !Float                 -- text, position, scale
  deriving stock (Show, Eq)

-- | HUD state
data HUDState = HUDState
  { hudShowDebug     :: !Bool
  , hudCrosshair     :: !CrosshairStyle
  , hudHealth        :: !Float
  , hudMaxHealth     :: !Float
  , hudHunger        :: !Float          -- placeholder for future
  , hudMaxHunger     :: !Float
  } deriving stock (Show, Eq)

-- | Default HUD state
newHUDState :: HUDState
newHUDState = HUDState
  { hudShowDebug  = False
  , hudCrosshair  = CrosshairCross
  , hudHealth     = 20.0
  , hudMaxHealth  = 20.0
  , hudHunger     = 20.0
  , hudMaxHunger  = 20.0
  }

-- | Toggle debug info display
toggleDebugInfo :: HUDState -> HUDState
toggleDebugInfo hud = hud { hudShowDebug = not (hudShowDebug hud) }

-- | Update HUD based on current game state
updateHUD :: Player -> Inventory -> DayNightCycle -> Int -> HUDState -> HUDState
updateHUD player _inv _dayNight _fps hud = hud
  { hudHealth = 20.0  -- placeholder: would come from player health component
  }

-- | Generate list of HUD elements to render
hudElements :: HUDState -> Player -> Inventory -> DayNightCycle -> Int -> V2 Int -> [HUDElement]
hudElements hud player inv dayNight fps (V2 screenW screenH) =
  let cx = fromIntegral screenW / 2.0
      cy = fromIntegral screenH / 2.0

      -- Crosshair at center
      crosshair = HECrosshair (hudCrosshair hud) (V2 cx cy)

      -- Hotbar: collect items from slots 0-8
      hotbarItems = [ getHotbarSlot inv i | i <- [0 .. hotbarSlots - 1] ]
      hotbar = HEHotbar hotbarItems (invSelected inv)

      -- Health bar
      health = HEHealthBar (hudHealth hud) (hudMaxHealth hud)

      -- Hunger bar (placeholder)
      hunger = HEHungerBar (hudHunger hud) (hudMaxHunger hud)

      -- Debug info (F3 screen)
      debugInfo = if hudShowDebug hud
        then [HEDebugText (debugLines player dayNight fps)]
        else []

  in [crosshair, hotbar, health, hunger] ++ debugInfo

-- | Generate debug info lines
debugLines :: Player -> DayNightCycle -> Int -> [String]
debugLines player dayNight fps =
  let V3 px py pz = plPos player
  in [ "Minecraft Haskell"
     , "FPS: " ++ show fps
     , "XYZ: " ++ showF2 px ++ " / " ++ showF2 py ++ " / " ++ showF2 pz
     , "Facing: yaw=" ++ showF1 (plYaw player) ++ " pitch=" ++ showF1 (plPitch player)
     , "Flying: " ++ show (plFlying player)
     , "Day: " ++ show (dncDayCount dayNight) ++ " Time: " ++ showF2 (dncTime dayNight)
     , "Light: " ++ showF2 (getAmbientLight dayNight)
     ]

-- | Format float with N decimal places
showF1 :: Float -> String
showF1 x = show (fromIntegral (round (x * 10)) / 10.0 :: Float)

showF2 :: Float -> String
showF2 x = show (fromIntegral (round (x * 100)) / 100.0 :: Float)
