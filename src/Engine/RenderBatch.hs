module Engine.RenderBatch
  ( batchChunks
  , optimalBatchSize
  ) where

import World.Chunk (ChunkPos)
import Linear (V2(..))
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)

-- | Maximum number of chunks per render batch.
optimalBatchSize :: Int
optimalBatchSize = 8

-- | Group chunks by spatial proximity into batches of at most 'optimalBatchSize'.
-- Each input pair is (ChunkPos, vertexCount); the vertex count is carried for
-- caller convenience but does not influence grouping.  Chunks closest to each
-- other (by Manhattan distance on their chunk coordinates) are grouped together.
batchChunks :: [(ChunkPos, Int)] -> [[ChunkPos]]
batchChunks [] = []
batchChunks pairs =
  let sorted = sortBy (comparing (\(V2 x z, _) -> (x, z))) pairs
  in buildBatches sorted []

-- Build batches greedily: pick the first remaining chunk as a seed, then pull
-- the nearest neighbours until the batch is full or no chunks remain.
buildBatches :: [(ChunkPos, Int)] -> [[ChunkPos]] -> [[ChunkPos]]
buildBatches [] acc = reverse acc
buildBatches remaining acc =
  let (batch, rest) = fillBatch remaining optimalBatchSize
  in buildBatches rest (batch : acc)

fillBatch :: [(ChunkPos, Int)] -> Int -> ([ChunkPos], [(ChunkPos, Int)])
fillBatch remaining 0 = ([], remaining)
fillBatch [] _ = ([], [])
fillBatch remaining slots =
  let seed = head remaining
      rest = tail remaining
      (picked, leftover) = pickNearest (fst seed) rest (slots - 1)
  in (fst seed : picked, leftover)

-- Greedily pick the nearest chunk to `centre`, repeat until we have `n` or
-- none are left.
pickNearest :: ChunkPos -> [(ChunkPos, Int)] -> Int -> ([ChunkPos], [(ChunkPos, Int)])
pickNearest _ remaining 0 = ([], remaining)
pickNearest _ [] _ = ([], [])
pickNearest centre remaining n =
  let nearest = minimumBy (comparing (chunkDist centre . fst)) remaining
      remaining' = filter (/= nearest) remaining
      (picked, leftover) = pickNearest (fst nearest) remaining' (n - 1)
  in (fst nearest : picked, leftover)

-- Manhattan distance between two chunk positions.
chunkDist :: ChunkPos -> ChunkPos -> Int
chunkDist (V2 x1 z1) (V2 x2 z2) = abs (x1 - x2) + abs (z1 - z2)
