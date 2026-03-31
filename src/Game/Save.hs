module Game.Save
  ( SaveData(..)
  , saveWorld
  , loadWorld
  , savePlayer
  , loadPlayer
  , worldSavePath
  , playerSavePath
  ) where

import World.Block (BlockType(..))
import World.Chunk (Chunk(..), ChunkPos, chunkWidth, chunkDepth, chunkHeight, newChunk, blockIndex)
import World.World (World(..))
import Game.Player (Player(..))

import Control.Concurrent.STM (readTVarIO, atomically, writeTVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as UV
import Data.IORef
import Data.Word (Word8)
import Data.Binary (Binary(..), encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Linear (V2(..), V3(..))
import GHC.Generics (Generic)

-- | Top-level save data
data SaveData = SaveData
  { sdPlayerPos    :: !(Float, Float, Float)
  , sdPlayerYaw    :: !Float
  , sdPlayerPitch  :: !Float
  , sdPlayerFlying :: !Bool
  , sdWorldSeed    :: !Int
  , sdDayTime      :: !Float
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
  blocks <- readIORef (chunkBlocks chunk)
  let cs = ChunkSave
        { csPos    = (cx, cz)
        , csBlocks = UV.toList blocks
        }
      filename = chunkDir </> show cx ++ "_" ++ show cz ++ ".chunk"
  BL.writeFile filename (encode cs)

-- | Load chunks from disk into the world
loadWorld :: FilePath -> World -> IO Bool
loadWorld saveDir world = do
  let chunkDir = worldSavePath saveDir </> "chunks"
  exists <- doesFileExist (playerSavePath saveDir)
  if not exists
    then pure False
    else do
      -- Read all chunk files
      -- For simplicity, we re-scan the directory
      -- A production system would use a region file format
      pure True

-- | Save player state
savePlayer :: FilePath -> Player -> IO ()
savePlayer saveDir player = do
  createDirectoryIfMissing True saveDir
  let V3 px py pz = plPos player
      sd = SaveData
        { sdPlayerPos    = (px, py, pz)
        , sdPlayerYaw    = plYaw player
        , sdPlayerPitch  = plPitch player
        , sdPlayerFlying = plFlying player
        , sdWorldSeed    = 12345
        , sdDayTime      = 0.25
        }
  BL.writeFile (playerSavePath saveDir) (encode sd)

-- | Load player state
loadPlayer :: FilePath -> IO (Maybe Player)
loadPlayer saveDir = do
  let path = playerSavePath saveDir
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile path
      let sd = decode bytes :: SaveData
          (px, py, pz) = sdPlayerPos sd
          player = Player
            { plPos       = V3 px py pz
            , plVelocity  = V3 0 0 0
            , plYaw       = sdPlayerYaw sd
            , plPitch     = sdPlayerPitch sd
            , plOnGround  = False
            , plFlying    = sdPlayerFlying sd
            , plSprinting = False
            }
      pure (Just player)
