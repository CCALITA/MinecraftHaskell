-- | Chunk mesh management: build, rebuild, prune GPU meshes for world chunks.
--
-- Extracted from @app\/Main.hs@. Pure mechanical extraction — no behavior changes.
module Engine.ChunkMesh
  ( rebuildAllChunkMeshes
  , settleAllLoadedChunks
  , getNeighborData
  , meshSingleChunk
  , rebuildChunkAt
  , rebuildChunkWithNeighbors
  , pruneChunkMeshes
  , remeshDirtyChunks
  , rebuildExplosionChunks
  ) where

import qualified Vulkan as Vk
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as VS
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import Data.IORef
import Linear (V2(..), V3(..))

import Engine.Mesh (BlockVertex(..), MeshData(..), NeighborData(..), meshChunkWithLight)
import Engine.Vulkan.Memory
  ( BufferAllocation
  , createVertexBuffer
  , createIndexBuffer
  , destroyBuffer
  )
import World.Chunk
  ( Chunk
  , ChunkPos
  , chunkPos
  , chunkWidth
  , chunkDepth
  , freezeBlocks
  )
import World.Light (newLightMap, propagateBlockLight, propagateSkyLight)
import World.World (World, worldChunks, getChunk, settleChunkGravity)

-- | Build GPU meshes for all loaded chunks (initial load)
rebuildAllChunkMeshes
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
rebuildAllChunkMeshes world physDevice device cmdPool queue cacheRef = do
  chunks <- HM.elems <$> readTVarIO (worldChunks world)
  mapM_ (meshSingleChunk world physDevice device cmdPool queue cacheRef) chunks

-- | Settle gravity-affected blocks in all currently loaded chunks
settleAllLoadedChunks :: World -> IO ()
settleAllLoadedChunks world = do
  chunks <- HM.elems <$> readTVarIO (worldChunks world)
  mapM_ (settleChunkGravity world) chunks

-- | Freeze neighbor chunk blocks for cross-chunk face culling.
--   Returns NeighborData with frozen vectors for each loaded cardinal neighbor.
getNeighborData :: World -> ChunkPos -> IO NeighborData
getNeighborData world (V2 cx cz) = do
  allChunks <- readTVarIO (worldChunks world)
  let freeze pos = case HM.lookup pos allChunks of
        Nothing -> pure Nothing
        Just c  -> Just <$> freezeBlocks c
  north <- freeze (V2 cx (cz + 1))
  south <- freeze (V2 cx (cz - 1))
  east  <- freeze (V2 (cx + 1) cz)
  west  <- freeze (V2 (cx - 1) cz)
  pure NeighborData
    { ndNorth = north
    , ndSouth = south
    , ndEast  = east
    , ndWest  = west
    }

-- | Build GPU mesh for a single chunk and insert into cache
meshSingleChunk
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Chunk -> IO ()
meshSingleChunk world physDevice device cmdPool queue cacheRef chunk = do
  -- Propagate light for this chunk
  lm <- newLightMap
  propagateBlockLight chunk lm
  propagateSkyLight chunk lm
  -- Gather neighbor data for cross-chunk face culling
  nd <- getNeighborData world (chunkPos chunk)
  -- Mesh with light data and neighbor info
  mesh <- meshChunkWithLight chunk lm nd
  let ic = VS.length (mdIndices mesh)
      pos = chunkPos chunk
      V2 cx cz = pos
      offsetX = fromIntegral cx * fromIntegral chunkWidth
      offsetZ = fromIntegral cz * fromIntegral chunkDepth
      -- Offset vertices to world space
      worldVerts = VS.map (\v ->
        v { bvPosition = bvPosition v + V3 offsetX 0 offsetZ }
        ) (mdVertices mesh)

  -- Destroy old mesh for this chunk if it exists
  cache <- readIORef cacheRef
  case HM.lookup pos cache of
    Just (oldVb, oldIb, _) -> do
      destroyBuffer device oldVb
      destroyBuffer device oldIb
    Nothing -> pure ()

  if VS.null worldVerts
    then modifyIORef' cacheRef (HM.delete pos)
    else do
      vb <- createVertexBuffer physDevice device cmdPool queue worldVerts
      ib <- createIndexBuffer physDevice device cmdPool queue (mdIndices mesh)
      modifyIORef' cacheRef (HM.insert pos (vb, ib, ic))

-- | Rebuild mesh for the chunk containing world coordinates (wx, wz)
rebuildChunkAt
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> IO ()
rebuildChunkAt world physDevice device cmdPool queue cacheRef wx wz = do
  let cx = wx `div` chunkWidth
      cz = wz `div` chunkDepth
  mChunk <- getChunk world (V2 cx cz)
  case mChunk of
    Nothing -> pure ()
    Just chunk -> meshSingleChunk world physDevice device cmdPool queue cacheRef chunk

-- | Rebuild the chunk at (wx, wz) plus its 4 cardinal neighbors.
--   Light propagation is per-chunk, so neighbors must be re-meshed when
--   a light-emitting block changes near a chunk boundary.
rebuildChunkWithNeighbors
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> IO ()
rebuildChunkWithNeighbors world physDevice device cmdPool queue cacheRef wx wz = do
  let cx = wx `div` chunkWidth
      cz = wz `div` chunkDepth
      rebuild ncx ncz = rebuildChunkAt world physDevice device cmdPool queue cacheRef (ncx * chunkWidth) (ncz * chunkDepth)
  forM_ [ (cx, cz), (cx - 1, cz), (cx + 1, cz), (cx, cz - 1), (cx, cz + 1) ] $
    uncurry rebuild

-- | Remove cached meshes for chunks that are no longer loaded
pruneChunkMeshes
  :: World -> Vk.Device
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
pruneChunkMeshes world device cacheRef = do
  loadedChunks <- readTVarIO (worldChunks world)
  cache <- readIORef cacheRef
  let toRemove = HM.filterWithKey (\k _ -> not (HM.member k loadedChunks)) cache
  mapM_ (\(vb, ib, _) -> destroyBuffer device vb >> destroyBuffer device ib) (HM.elems toRemove)
  modifyIORef' cacheRef (\c -> HM.filterWithKey (\k _ -> HM.member k loadedChunks) c)

-- | Read accumulated dirty chunk positions, deduplicate, and remesh each
remeshDirtyChunks
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IORef [ChunkPos] -> IO ()
remeshDirtyChunks world physDevice device cmdPool queue cacheRef dirtyRef = do
  dirty <- readIORef dirtyRef
  let unique = HM.keys (HM.fromList [(cp, ()) | cp <- dirty])
  forM_ unique $ \cp -> do
    mCh <- getChunk world cp
    case mCh of
      Nothing -> pure ()
      Just ch -> meshSingleChunk world physDevice device cmdPool queue cacheRef ch

-- | Rebuild chunk meshes in the area affected by an explosion
rebuildExplosionChunks
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> Int -> IO ()
rebuildExplosionChunks world physDevice device cmdPool queue cacheRef centerX centerZ radius = do
  let bx0 = centerX - radius; bz0 = centerZ - radius
      bx1 = centerX + radius; bz1 = centerZ + radius
      cx0 = bx0 `div` chunkWidth; cz0 = bz0 `div` chunkDepth
      cx1 = bx1 `div` chunkWidth; cz1 = bz1 `div` chunkDepth
  forM_ [cx0 .. cx1] $ \chx ->
    forM_ [cz0 .. cz1] $ \chz -> do
      mChunk <- getChunk world (V2 chx chz)
      case mChunk of
        Nothing -> pure ()
        Just ch -> meshSingleChunk world physDevice device cmdPool queue cacheRef ch
