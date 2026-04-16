module World.World
  ( World(..)
  , newWorld
  , getChunk
  , updateLoadedChunks
  , worldGetBlock
  , worldSetBlock
  , worldToChunkLocal
  , loadedChunkCount
  , triggerGravityAbove
  , settleGravityBlock
  , settleChunkGravity
  ) where

import World.Block (BlockType(..), isGravityAffected)
import World.Chunk
import World.Generation
import World.Noise (Seed)

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import Control.Monad (forM_, when, unless)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
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

-- | Trigger gravity for blocks above a position that just became Air.
--   Cascades downward: if the block above is gravity-affected, it falls into
--   the given position, then we check the block above that, and so on.
--   Returns True if any blocks moved.
triggerGravityAbove :: World -> V3 Int -> IO Bool
triggerGravityAbove world = go
  where
    go (V3 x y z)
      | y >= 255  = pure False
      | otherwise = do
          let above = V3 x (y + 1) z
          bt <- worldGetBlock world above
          if isGravityAffected bt
            then do
              worldSetBlock world above Air
              worldSetBlock world (V3 x y z) bt
              _ <- go above
              pure True
            else pure False

-- | Settle a gravity-affected block at the given position by moving it
--   down to the lowest Air block in its column. Returns True if the
--   block moved (chunks need rebuilding).
settleGravityBlock :: World -> V3 Int -> IO Bool
settleGravityBlock world pos@(V3 x y z)
  | y <= 0    = pure False
  | otherwise = do
      bt <- worldGetBlock world pos
      if not (isGravityAffected bt)
        then pure False
        else do
          -- Find the lowest air block this block can fall to
          landY <- findLandingY world x (y - 1) z
          if landY == y
            then pure False  -- already resting on solid ground
            else do
              worldSetBlock world pos Air
              worldSetBlock world (V3 x landY z) bt
              pure True

-- | Find the Y coordinate where a falling block should land.
--   Scans downward from startY until hitting a non-Air block,
--   then returns the Y above it.
findLandingY :: World -> Int -> Int -> Int -> IO Int
findLandingY world x startY z = go startY
  where
    go y
      | y <= 0    = pure 0
      | otherwise = do
          below <- worldGetBlock world (V3 x y z)
          if below == Air
            then go (y - 1)
            else pure (y + 1)

-- | Settle all gravity-affected blocks in a chunk after generation.
--   Scans bottom-to-top so blocks cascade correctly in a single pass.
--   Returns True if any blocks were moved (chunk needs mesh rebuild).
settleChunkGravity :: World -> Chunk -> IO Bool
settleChunkGravity world chunk = do
  let V2 cx cz = chunkPos chunk
  anyMoved <- newIORef False
  forM_ [0..15] $ \lx ->
    forM_ [0..15] $ \lz ->
      forM_ [1..255] $ \ly -> do
        let wx = cx * 16 + lx
            wz = cz * 16 + lz
        bt <- worldGetBlock world (V3 wx ly wz)
        when (isGravityAffected bt) $ do
          below <- worldGetBlock world (V3 wx (ly - 1) wz)
          when (below == Air) $ do
            -- Find where this block should land
            landY <- findLandingY world wx (ly - 1) wz
            worldSetBlock world (V3 wx ly wz) Air
            worldSetBlock world (V3 wx landY wz) bt
            writeIORef anyMoved True
  readIORef anyMoved
