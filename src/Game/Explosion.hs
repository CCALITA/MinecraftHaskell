module Game.Explosion
  ( explodeAt
  ) where

import Control.Monad (forM_, when)
import Linear (V3(..))
import System.Random (randomRIO)

import World.Block (BlockType(..))
import World.World (World, worldGetBlock, worldSetBlock)
import Game.Item (blockDrops)
import Game.DroppedItem (DroppedItems, spawnDrop)

-- | Trigger an explosion at a world position, clearing blocks within
-- @radius@ and spawning dropped items for 30% of destroyed blocks.
-- Air, Bedrock, Water and Lava are preserved.
explodeAt :: World -> V3 Float -> Int -> DroppedItems -> IO ()
explodeAt world (V3 centerX centerY centerZ) radius droppedItemsRef = do
  let cx = floor centerX :: Int
      cy = floor centerY :: Int
      cz = floor centerZ :: Int
      r  = fromIntegral radius :: Float
  forM_ [cx - radius .. cx + radius] $ \x ->
    forM_ [cy - radius .. cy + radius] $ \y ->
      forM_ [cz - radius .. cz + radius] $ \z -> do
        let dist = sqrt (fromIntegral ((x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int) + (z - cz) ^ (2 :: Int))) :: Float
        when (dist <= r) $ do
          bt <- worldGetBlock world (V3 x y z)
          when (bt /= Air && bt /= Bedrock && bt /= Water && bt /= Lava) $ do
            worldSetBlock world (V3 x y z) Air
            -- 30% chance to drop the block's items
            roll <- randomRIO (0.0, 1.0 :: Float)
            when (roll < 0.3) $ do
              let drops = blockDrops bt
              forM_ drops $ \(item, count) ->
                spawnDrop droppedItemsRef item count
                  (V3 (fromIntegral x + 0.5) (fromIntegral y + 0.5) (fromIntegral z + 0.5))
