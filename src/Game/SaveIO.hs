module Game.SaveIO
  ( buildSaveDataV3
  , playerFromSaveDataV3
  , restoreFromSaveV3
  ) where

import Data.IORef (IORef, writeIORef)
import Linear (V3(..))

import Game.DayNight (DayNightCycle(..), newDayNightCycle)
import Game.Inventory (Inventory, ItemStack(..))
import Game.Player (Player(..))
import Game.Save (inventoryToSlotList, slotListToInventory)
import Game.SaveV3 (SaveDataV3(..), savev3Version)
import World.Weather (WeatherState(..), WeatherType(..))

-- | Build a SaveDataV3 record from current game state
buildSaveDataV3 :: Player -> Inventory -> DayNightCycle -> WeatherState -> Int -> V3 Float -> SaveDataV3
buildSaveDataV3 player inv dayNight weather xp (V3 sx sy sz) =
  let V3 px py pz = plPos player
      weatherByte = case wsType weather of
        Clear -> 0
        Rain  -> 1
  in SaveDataV3
    { sv3Version      = savev3Version
    , sv3PlayerPos    = (px, py, pz)
    , sv3PlayerYaw    = plYaw player
    , sv3PlayerPitch  = plPitch player
    , sv3Flying       = plFlying player
    , sv3Health       = fromIntegral (plHealth player)
    , sv3Hunger       = plHunger player
    , sv3Saturation   = plSaturation player
    , sv3XP           = xp
    , sv3XPLevel      = 0
    , sv3FallDist     = plFallDist player
    , sv3AirSupply    = plAirSupply player
    , sv3DayTime      = dncTime dayNight
    , sv3DayCount     = dncDayCount dayNight
    , sv3WeatherType  = weatherByte
    , sv3WeatherTimer = wsTimer weather
    , sv3Inventory    = inventoryToSlotList inv
    , sv3ArmorSlots   = map (fmap (\(ItemStack i c) -> (i, c))) (plArmorSlots player)
    , sv3SpawnPoint   = (sx, sy, sz)
    , sv3WorldSeed    = 12345
    , sv3ChunkMetas   = []
    }

-- | Reconstruct a Player from SaveDataV3
playerFromSaveDataV3 :: SaveDataV3 -> Player
playerFromSaveDataV3 sd =
  let (px, py, pz) = sv3PlayerPos sd
  in Player
    { plPos       = V3 px py pz
    , plVelocity  = V3 0 0 0
    , plYaw       = sv3PlayerYaw sd
    , plPitch     = sv3PlayerPitch sd
    , plOnGround  = False
    , plFlying    = sv3Flying sd
    , plSprinting = False
    , plSneaking  = False
    , plHealth    = round (sv3Health sd)
    , plHunger    = sv3Hunger sd
    , plFallDist  = sv3FallDist sd
    , plEatingTimer = 0.0
    , plArmorSlots = map (fmap (\(i, c) -> ItemStack i c)) (sv3ArmorSlots sd)
    , plAirSupply = sv3AirSupply sd
    , plSaturation = sv3Saturation sd
    , plSprintToggled = False
    }

-- | Restore player, inventory, day/night, weather, XP, and spawn from SaveDataV3
restoreFromSaveV3 :: IORef Player -> IORef Inventory -> IORef DayNightCycle
                  -> IORef WeatherState -> IORef Int -> IORef (V3 Float)
                  -> SaveDataV3 -> IO ()
restoreFromSaveV3 playerRef inventoryRef dayNightRef weatherRef xpRef spawnRef sd = do
  writeIORef playerRef (playerFromSaveDataV3 sd)
  writeIORef inventoryRef (slotListToInventory (sv3Inventory sd) 0)
  writeIORef dayNightRef (newDayNightCycle { dncTime = sv3DayTime sd, dncDayCount = sv3DayCount sd })
  let wType = if sv3WeatherType sd == 0 then Clear else Rain
  writeIORef weatherRef (WeatherState wType (sv3WeatherTimer sd) (if wType == Rain then 1.0 else 0.0))
  writeIORef xpRef (sv3XP sd)
  let (sx, sy, sz) = sv3SpawnPoint sd
  writeIORef spawnRef (V3 sx sy sz)
