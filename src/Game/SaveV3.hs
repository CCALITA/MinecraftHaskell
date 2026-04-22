module Game.SaveV3
  ( SaveDataV3(..)
  , ChunkMeta(..)
  , savev3Version
  , encodeSaveV3
  , decodeSaveV3
  , migrateV2toV3
  , savePlayerV3
  , loadPlayerV3
  ) where

import Game.Item (Item)
import Game.Save (SaveData(..), playerSavePath)
import Game.Player (maxAirSupply, defaultSaturation)

import Data.Binary (Binary(..), encode, decodeOrFail)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GHC.Generics (Generic)
import Linear (V2(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)

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

-- | Save player state in V3 format with version prefix.
savePlayerV3 :: FilePath -> SaveDataV3 -> IO ()
savePlayerV3 saveDir sd = do
  createDirectoryIfMissing True saveDir
  BL.writeFile (playerSavePath saveDir) (encodeSaveV3 sd)

-- | Load player state, auto-detecting version.
-- V3 saves are loaded directly; V2 saves are migrated via 'migrateV2toV3'.
-- Returns 'Nothing' when no save file exists or parsing fails.
loadPlayerV3 :: FilePath -> IO (Maybe SaveDataV3)
loadPlayerV3 saveDir = do
  let path = playerSavePath saveDir
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile path
      case decodeOrFail bytes of
        Left _ -> Nothing <$ pure ()
        Right (rest, _, version) -> case (version :: Word8) of
          v | v == savev3Version ->
              -- V3 format: decode payload directly
              case decodeOrFail rest of
                Right (_, _, sd) -> pure (Just sd)
                Left _ -> pure Nothing
          2 ->
              -- V2 format: decode then migrate
              case decodeOrFail rest of
                Right (_, _, sd) -> pure (Just (migrateV2toV3 sd))
                Left _ -> pure Nothing
          _ ->
              -- Unknown version: not supported
              pure Nothing
