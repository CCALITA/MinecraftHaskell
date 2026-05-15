module World.ChunkPriority
  ( chunkPriority
  , sortChunksByPriority
  ) where

import Linear (V2(..), V3(..))
import World.Chunk (ChunkPos, chunkWidth, chunkDepth)
import Data.List (sortOn)

-- | Euclidean distance from player position to the center of the given chunk.
--   The chunk center is computed in world coordinates (block-space), using the
--   midpoints of the chunk's X/Z extent and a fixed Y of 128 (half of chunkHeight=256).
chunkPriority :: V3 Float -> ChunkPos -> Float
chunkPriority (V3 px py pz) (V2 cx cz) =
  let centerX = fromIntegral (cx * chunkWidth)  + fromIntegral chunkWidth  / 2.0
      centerZ = fromIntegral (cz * chunkDepth)  + fromIntegral chunkDepth  / 2.0
      centerY = 128.0 :: Float
      dx = px - centerX
      dy = py - centerY
      dz = pz - centerZ
  in sqrt (dx * dx + dy * dy + dz * dz)

-- | Sort chunk positions by ascending priority (nearest chunks first).
sortChunksByPriority :: V3 Float -> [ChunkPos] -> [ChunkPos]
sortChunksByPriority playerPos = sortOn (chunkPriority playerPos)
