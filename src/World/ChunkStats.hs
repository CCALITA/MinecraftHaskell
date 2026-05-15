module World.ChunkStats
  ( ChunkCacheStats(..)
  , emptyChunkStats
  , formatChunkStats
  ) where

-- | Snapshot of chunk-cache metrics for diagnostics / HUD display.
data ChunkCacheStats = ChunkCacheStats
  { loadedChunks  :: Int
  , dirtyChunks   :: Int
  , totalMeshes   :: Int
  , totalVertices :: Int
  } deriving (Show, Read, Eq)

-- | A stats value with every counter at zero.
emptyChunkStats :: ChunkCacheStats
emptyChunkStats = ChunkCacheStats
  { loadedChunks  = 0
  , dirtyChunks   = 0
  , totalMeshes   = 0
  , totalVertices = 0
  }

-- | Human-readable one-liner, e.g.
-- @"Chunks: 42 loaded, 3 dirty | Meshes: 40 (12345 verts)"@
formatChunkStats :: ChunkCacheStats -> String
formatChunkStats s =
  "Chunks: " <> show (loadedChunks s)
  <> " loaded, " <> show (dirtyChunks s)
  <> " dirty | Meshes: " <> show (totalMeshes s)
  <> " (" <> show (totalVertices s) <> " verts)"
