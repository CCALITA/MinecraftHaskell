module TestHelpers
  ( -- * Common block queries
    airHeightQuery
  , airQuery
  , waterQuery
    -- * Test world helpers
  , withTestWorld
  , withTestChunk
  ) where

import Game.Physics (BlockHeightQuery)
import World.Chunk (Chunk, newChunk)
import World.Generation (defaultGenConfig)
import World.World (World(..), newWorld)

import Control.Concurrent.STM (atomically, writeTVar)
import Linear (V2(..))
import qualified Data.HashMap.Strict as HM

-- | Block height query that always returns 0.0 (air everywhere).
-- Used for physics tests where no solid blocks exist.
airHeightQuery :: BlockHeightQuery
airHeightQuery _ _ _ = pure 0.0

-- | Block query that always returns False (no blocks present).
-- Suitable for water/lava/solid queries when the world is empty.
airQuery :: Int -> Int -> Int -> IO Bool
airQuery _ _ _ = pure False

-- | Block query that always returns True (water everywhere).
-- Used for underwater physics tests.
waterQuery :: Int -> Int -> Int -> IO Bool
waterQuery _ _ _ = pure True

-- | Create a test world with a single empty chunk at (0,0).
-- The world uses the default generation config with render distance 4.
withTestWorld :: (World -> IO a) -> IO a
withTestWorld action = do
  world <- newWorld defaultGenConfig 4
  chunk <- newChunk (V2 0 0)
  atomically $ writeTVar (worldChunks world) (HM.singleton (V2 0 0) chunk)
  action world

-- | Create a fresh empty chunk at position (0,0).
withTestChunk :: (Chunk -> IO a) -> IO a
withTestChunk action = do
  chunk <- newChunk (V2 0 0)
  action chunk
