module Game.Save
  ( SaveData(..)
  , saveWorld
  , loadWorld
  , savePlayer
  , loadPlayer
  , worldSavePath
  , playerSavePath
  , listSaves
  , nextSaveName
  , inventoryToSlotList
  , slotListToInventory
  ) where

import World.Chunk (Chunk(..), ChunkPos, newChunk, freezeBlocks)
import World.World (World(..))
import Game.Item (Item(..))
import Game.Inventory (Inventory(..), ItemStack(..), emptyInventory, inventorySlots, setSlot, getSlot)

import Control.Monad (unless, filterM)

import Control.Concurrent.STM (readTVarIO, atomically, writeTVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.IORef
import Data.Word (Word8)
import Data.Binary (Binary(..), encode, decodeOrFail)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Data.List (sort, stripPrefix)
import Linear (V2(..))
import GHC.Generics (Generic)

-- | Current save format version
currentSaveVersion :: Word8
currentSaveVersion = 2

-- | Top-level save data (version 2: includes health, hunger, fall distance,
-- day/night cycle, and inventory)
data SaveData = SaveData
  { sdPlayerPos    :: !(Float, Float, Float)
  , sdPlayerYaw    :: !Float
  , sdPlayerPitch  :: !Float
  , sdPlayerFlying :: !Bool
  , sdWorldSeed    :: !Int
  , sdDayTime      :: !Float
  , sdDayCount     :: !Int
  , sdHealth       :: !Int
  , sdHunger       :: !Int
  , sdFallDist     :: !Float
  , sdInventory    :: ![(Int, Item, Int)]  -- (slot index, item, count)
  , sdSelectedSlot :: !Int
  } deriving stock (Show, Eq, Generic)

instance Binary SaveData

-- | Serialized chunk data
data ChunkSave = ChunkSave
  { csPos    :: !(Int, Int)        -- chunk position
  , csBlocks :: ![Word8]           -- flat block array
  } deriving stock (Show, Eq, Generic)

instance Binary ChunkSave

-- | Save directory path
worldSavePath :: FilePath -> FilePath
worldSavePath saveDir = saveDir </> "world"

playerSavePath :: FilePath -> FilePath
playerSavePath saveDir = saveDir </> "player.dat"

-- | Save the entire world to disk
saveWorld :: FilePath -> World -> IO ()
saveWorld saveDir world = do
  let chunkDir = worldSavePath saveDir </> "chunks"
  createDirectoryIfMissing True chunkDir

  chunks <- readTVarIO (worldChunks world)
  mapM_ (\(pos, chunk) -> saveChunk chunkDir pos chunk) (HM.toList chunks)

-- | Save a single chunk
saveChunk :: FilePath -> ChunkPos -> Chunk -> IO ()
saveChunk chunkDir (V2 cx cz) chunk = do
  blocks <- freezeBlocks chunk
  let cs = ChunkSave
        { csPos    = (cx, cz)
        , csBlocks = UV.toList blocks
        }
      filename = chunkDir </> show cx ++ "_" ++ show cz ++ ".chunk"
  BL.writeFile filename (encode cs)

-- | Load chunks from disk into the world. Returns True if save exists.
loadWorld :: FilePath -> World -> IO Bool
loadWorld saveDir world = do
  let chunkDir = worldSavePath saveDir </> "chunks"
  dirExists <- doesDirectoryExist chunkDir
  if not dirExists
    then pure False
    else do
      files <- listDirectory chunkDir
      let chunkFiles = filter (\f -> takeExtension f == ".chunk") files
      loadedChunks <- mapM (loadChunkFile chunkDir) chunkFiles
      let validChunks = [c | Just c <- loadedChunks]
      unless (null validChunks) $ atomically $ do
        let chunkMap = HM.fromList [(chunkPos c, c) | c <- validChunks]
        writeTVar (worldChunks world) chunkMap
      pure (not (null validChunks))

-- | Load a single chunk from a file
loadChunkFile :: FilePath -> FilePath -> IO (Maybe Chunk)
loadChunkFile chunkDir filename = do
  let path = chunkDir </> filename
  bytes <- BL.readFile path
  case decodeOrFail bytes of
    Left _ -> pure Nothing
    Right (_, _, cs) -> do
      let (cx, cz) = csPos cs
          pos = V2 cx cz
      chunk <- newChunk pos
      -- Write blocks into the mutable vector
      let blocks = csBlocks cs
      mapM_ (\(i, b) -> MV.write (chunkBlocks chunk) i b) (zip [0..] blocks)
      writeIORef (chunkDirty chunk) True
      pure (Just chunk)

-- | Save player state with version tag
savePlayer :: FilePath -> SaveData -> IO ()
savePlayer saveDir sd = do
  createDirectoryIfMissing True saveDir
  BL.writeFile (playerSavePath saveDir) $
    encode currentSaveVersion <> encode sd

-- | Load player state, handling version migration
loadPlayer :: FilePath -> IO (Maybe SaveData)
loadPlayer saveDir = do
  let path = playerSavePath saveDir
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile path
      case decodeOrFail bytes of
        Left _ -> pure Nothing
        Right (rest, _, version) -> case (version :: Word8) of
          2 -> case decodeOrFail rest of
            Right (_, _, sd) -> pure (Just sd)
            Left _ -> pure Nothing
          _ ->
            -- Version 1 (old format): try to parse the original SaveData fields
            pure (migrateV1 bytes)

-- | Attempt to migrate a version-1 save (no version tag) to version 2.
-- The old format encoded SaveData directly without a version prefix.
-- We try to decode the old 6-field SaveData from the raw bytes.
migrateV1 :: BL.ByteString -> Maybe SaveData
migrateV1 bytes =
  case decodeOrFail bytes of
    Right (_, _, old) -> Just (upgradeV1 old)
    Left _ -> Nothing

-- | Version 1 SaveData (old format without version tag)
data SaveDataV1 = SaveDataV1
  { v1PlayerPos    :: !(Float, Float, Float)
  , v1PlayerYaw    :: !Float
  , v1PlayerPitch  :: !Float
  , v1PlayerFlying :: !Bool
  , v1WorldSeed    :: !Int
  , v1DayTime      :: !Float
  } deriving stock (Show, Eq, Generic)

instance Binary SaveDataV1

-- | Upgrade a V1 save to the current format with sensible defaults
upgradeV1 :: SaveDataV1 -> SaveData
upgradeV1 v1 = SaveData
  { sdPlayerPos    = v1PlayerPos v1
  , sdPlayerYaw    = v1PlayerYaw v1
  , sdPlayerPitch  = v1PlayerPitch v1
  , sdPlayerFlying = v1PlayerFlying v1
  , sdWorldSeed    = v1WorldSeed v1
  , sdDayTime      = v1DayTime v1
  , sdDayCount     = 0
  , sdHealth       = 20
  , sdHunger       = 20
  , sdFallDist     = 0
  , sdInventory    = []
  , sdSelectedSlot = 0
  }

-- | Convert an Inventory to a serializable slot list.
-- Only non-empty slots are included.
inventoryToSlotList :: Inventory -> [(Int, Item, Int)]
inventoryToSlotList inv =
  [ (i, isItem stack, isCount stack)
  | i <- [0 .. inventorySlots - 1]
  , Just stack <- [getSlot inv i]
  ]

-- | Restore an Inventory from a serialized slot list.
slotListToInventory :: [(Int, Item, Int)] -> Int -> Inventory
slotListToInventory slots selected =
  let base = emptyInventory { invSelected = selected }
  in foldl (\inv (idx, item, cnt) -> setSlot inv idx (Just (ItemStack item cnt))) base slots

-- | List available save directories under "saves/"
listSaves :: IO [String]
listSaves = do
  let savesRoot = "saves"
  exists <- doesDirectoryExist savesRoot
  if not exists
    then pure []
    else do
      dirs <- listDirectory savesRoot
      -- Filter to directories that have a player.dat or world/ subdirectory
      validDirs <- filterM (\d -> do
        let path = savesRoot </> d
        isDir <- doesDirectoryExist path
        if not isDir then pure False
        else do
          hasPlayer <- doesFileExist (path </> "player.dat")
          hasWorld <- doesDirectoryExist (path </> "world")
          pure (hasPlayer || hasWorld)
        ) dirs
      pure (sort validDirs)

-- | Generate the next available save name (world1, world2, ...)
nextSaveName :: IO String
nextSaveName = do
  existing <- listSaves
  let nums = [n | name <- existing
                 , Just rest <- [stripPrefix "world" name]
                 , [(n, "")] <- [reads rest :: [(Int, String)]]]
      nextN = if null nums then 1 else maximum nums + 1
  pure ("world" ++ show nextN)
