module Engine.RenderStats
  ( RenderStats(..)
  , emptyRenderStats
  , formatRenderStats
  , addDrawCall
  ) where

-- | Per-frame rendering statistics.
data RenderStats = RenderStats
  { rsDrawCalls     :: Int
  , rsTriangleCount :: Int
  , rsVertexCount   :: Int
  , rsFps           :: Int
  } deriving stock (Show, Eq)

-- | Zero-initialised statistics.
emptyRenderStats :: RenderStats
emptyRenderStats = RenderStats
  { rsDrawCalls     = 0
  , rsTriangleCount = 0
  , rsVertexCount   = 0
  , rsFps           = 0
  }

-- | Human-readable summary of render statistics.
formatRenderStats :: RenderStats -> String
formatRenderStats rs =
  "FPS: " <> show (rsFps rs)
  <> " | Draw calls: " <> show (rsDrawCalls rs)
  <> " | Tris: " <> show (rsTriangleCount rs)
  <> " | Verts: " <> show (rsVertexCount rs)

-- | Record a draw call with the given triangle and vertex counts.
--   Returns a new 'RenderStats' with the totals accumulated.
addDrawCall :: Int -> Int -> RenderStats -> RenderStats
addDrawCall tris verts rs = RenderStats
  { rsDrawCalls     = rsDrawCalls rs + 1
  , rsTriangleCount = rsTriangleCount rs + tris
  , rsVertexCount   = rsVertexCount rs + verts
  , rsFps           = rsFps rs
  }
