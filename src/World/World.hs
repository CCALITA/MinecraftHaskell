module World.World
  ( World(..)
  , newWorld
  , getChunk
  , updateLoadedChunks
  , worldGetBlock
  , worldSetBlock
  , worldToChunkLocal
  , loadedChunkCount
  ) where

import World.Block (BlockType(..))
import World.Chunk
import World.Generation
import World.Noise (Seed)

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (forM_, when, unless)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Linear (V2(..), V3(..))

-- | The game world: a concurrent map of chunks
data World = World
  { worldChunks    :: !(TVar (HashMap ChunkPos Chunk))
  , worldGenConfig :: !GenerationConfig
  , worldRenderDist :: !Int   -- render distance in chunks
  }

-- | Create a new empty world
newWorld :: GenerationConfig -> Int -> IO World
newWorld cfg renderDist = do
  chunks <- newTVarIO HM.empty
  pure World
    { worldChunks     = chunks
    , worldGenConfig  = cfg
    , worldRenderDist = renderDist
    }

-- | Get a chunk if it's loaded
getChunk :: World -> ChunkPos -> IO (Maybe Chunk)
getChunk world pos = do
  chunks <- readTVarIO (worldChunks world)
  pure $ HM.lookup pos chunks

-- | Get block at world coordinates
worldGetBlock :: World -> V3 Int -> IO BlockType
worldGetBlock world (V3 wx wy wz) = do
  let (cx, lx) = worldToChunkLocal wx chunkWidth
      (cz, lz) = worldToChunkLocal wz chunkDepth
  mChunk <- getChunk world (V2 cx cz)
  case mChunk of
    Nothing    -> pure Air
    Just chunk -> getBlock chunk lx wy lz

-- | Set block at world coordinates
worldSetBlock :: World -> V3 Int -> BlockType -> IO ()
worldSetBlock world (V3 wx wy wz) bt = do
  let (cx, lx) = worldToChunkLocal wx chunkWidth
      (cz, lz) = worldToChunkLocal wz chunkDepth
  mChunk <- getChunk world (V2 cx cz)
  case mChunk of
    Nothing    -> pure ()
    Just chunk -> setBlock chunk lx wy lz bt

-- | Convert world coordinate to (chunk index, local offset).
--   Haskell's div/mod already use floor-division for positive divisors,
--   giving correct results for negative coordinates.
worldToChunkLocal :: Int -> Int -> (Int, Int)
worldToChunkLocal w size = w `divMod` size
{-# INLINE worldToChunkLocal #-}

-- | Update loaded chunks based on player position.
--   Loads chunks within render distance, unloads those outside.
--   Returns list of newly loaded chunks (need meshing).
updateLoadedChunks :: World -> V3 Float -> IO [Chunk]
updateLoadedChunks world (V3 px _ pz) = do
  let rd = worldRenderDist world
      playerCX = floor px `div` chunkWidth
      playerCZ = floor pz `div` chunkDepth

      -- All chunk positions within render distance
      needed = [ V2 cx cz
               | cx <- [playerCX - rd .. playerCX + rd]
               , cz <- [playerCZ - rd .. playerCZ + rd]
               , let dx = cx - playerCX
                     dz = cz - playerCZ
               , dx * dx + dz * dz <= rd * rd  -- circular distance
               ]

  chunks <- readTVarIO (worldChunks world)

  -- Find chunks that need loading
  let toLoad = filter (\pos -> not $ HM.member pos chunks) needed

  -- Generate missing chunks (async, batched)
  newChunks <- if null toLoad
    then pure []
    else do
      asyncChunks <- mapM (\pos -> async $ generateChunk (worldGenConfig world) pos) toLoad
      mapM wait asyncChunks

  -- Insert new chunks
  unless (null newChunks) $ atomically $ do
    current <- readTVar (worldChunks world)
    let updated = foldl (\m c -> HM.insert (chunkPos c) c m) current newChunks
    writeTVar (worldChunks world) updated

  -- Unload distant chunks (beyond 2x render distance)
  let unloadDist = rd * 2
  atomically $ modifyTVar (worldChunks world) $ \m ->
    HM.filterWithKey (\(V2 cx cz) _ ->
      let dx = cx - playerCX
          dz = cz - playerCZ
      in dx * dx + dz * dz <= unloadDist * unloadDist
    ) m

  pure newChunks

-- | Count of currently loaded chunks
loadedChunkCount :: World -> IO Int
loadedChunkCount world = HM.size <$> readTVarIO (worldChunks world)
