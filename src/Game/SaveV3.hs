module Game.SaveV3
  ( SaveDataV3(..)
  , ChunkMeta(..)
  , savev3Version
  , encodeSaveV3
  , decodeSaveV3
  , migrateV2toV3
  ) where

import Game.Item (Item)
import Game.Save (SaveData(..))
import Game.Player (maxAirSupply, defaultSaturation)

import Data.Binary (Binary(..), encode, decodeOrFail)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GHC.Generics (Generic)
import Linear (V2(..))

-- | Save format version for V3
savev3Version :: Word8
savev3Version = 3

-- | Lightweight metadata for a chunk (no block data)
data ChunkMeta = ChunkMeta
  { cmPos         :: !(V2 Int)
  , cmModified    :: !Bool
  , cmHasEntities :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Binary)

-- | Extended save data (version 3)
--
-- Adds: saturation, XP, air supply, weather state, spawn point,
-- armor slots, and chunk metadata compared to V2.
data SaveDataV3 = SaveDataV3
  { sv3Version      :: !Word8
  , sv3PlayerPos    :: !(Float, Float, Float)
  , sv3PlayerYaw    :: !Float
  , sv3PlayerPitch  :: !Float
  , sv3Flying       :: !Bool
  , sv3Health       :: !Float
  , sv3Hunger       :: !Int
  , sv3Saturation   :: !Float
  , sv3XP           :: !Int
  , sv3XPLevel      :: !Int
  , sv3FallDist     :: !Float
  , sv3AirSupply    :: !Float
  , sv3DayTime      :: !Float
  , sv3DayCount     :: !Int
  , sv3WeatherType  :: !Word8   -- 0=Clear, 1=Rain
  , sv3WeatherTimer :: !Float
  , sv3Inventory    :: ![(Int, Item, Int)]
  , sv3ArmorSlots   :: ![Maybe (Item, Int)]
  , sv3SpawnPoint   :: !(Float, Float, Float)
  , sv3WorldSeed    :: !Int
  , sv3ChunkMetas   :: ![ChunkMeta]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Binary)

-- | Encode a SaveDataV3 to lazy ByteString with a version prefix.
encodeSaveV3 :: SaveDataV3 -> BL.ByteString
encodeSaveV3 sd =
  encode savev3Version <> encode sd

-- | Decode a version-prefixed ByteString into SaveDataV3.
-- Returns Nothing on version mismatch or parse failure.
decodeSaveV3 :: BL.ByteString -> Maybe SaveDataV3
decodeSaveV3 bytes =
  case decodeOrFail bytes of
    Left _ -> Nothing
    Right (rest, _, ver)
      | ver /= savev3Version -> Nothing
      | otherwise ->
          case decodeOrFail rest of
            Left _          -> Nothing
            Right (_, _, sd) -> Just sd

-- | Migrate a V2 SaveData to V3 with sensible defaults.
migrateV2toV3 :: SaveData -> SaveDataV3
migrateV2toV3 v2 = SaveDataV3
  { sv3Version      = savev3Version
  , sv3PlayerPos    = sdPlayerPos v2
  , sv3PlayerYaw    = sdPlayerYaw v2
  , sv3PlayerPitch  = sdPlayerPitch v2
  , sv3Flying       = sdPlayerFlying v2
  , sv3Health       = fromIntegral (sdHealth v2)
  , sv3Hunger       = sdHunger v2
  , sv3Saturation   = defaultSaturation  -- from Game.Player
  , sv3XP           = 0
  , sv3XPLevel      = 0
  , sv3FallDist     = sdFallDist v2
  , sv3AirSupply    = maxAirSupply       -- from Game.Player
  , sv3DayTime      = sdDayTime v2
  , sv3DayCount     = sdDayCount v2
  , sv3WeatherType  = 0          -- Clear
  , sv3WeatherTimer = 600.0      -- 10 minutes until first weather change
  , sv3Inventory    = sdInventory v2
  , sv3ArmorSlots   = []
  , sv3SpawnPoint   = (0, 80, 0) -- default spawn
  , sv3WorldSeed    = sdWorldSeed v2
  , sv3ChunkMetas   = []
  }
