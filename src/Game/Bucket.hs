module Game.Bucket
  ( BucketAction(..)
  , determineBucketAction
  , bucketTypeToFluidBlock
  , fluidBlockToBucketType
  ) where

import Game.Item (BucketType(..))
import World.Block (BlockType(..))

-- | The result of using a bucket via right-click.
data BucketAction
  = BucketPickup !BlockType !BucketType
    -- ^ Remove the targeted fluid block, replace held bucket with filled bucket.
    --   Fields: block type to remove, new bucket type to give the player.
  | BucketPlace !BlockType !BucketType
    -- ^ Place a fluid block at adjacent position, replace held bucket with empty.
    --   Fields: block type to place, resulting empty bucket type.
  | BucketNoAction
    -- ^ Bucket use has no effect (e.g., empty bucket on non-fluid, milk bucket).
  deriving stock (Show, Eq)

-- | Determine what a bucket right-click should do, given the bucket type
--   and the block being targeted.
--
--   Rules:
--   * BucketEmpty + Water target -> pick up water, get BucketWater
--   * BucketEmpty + Lava target  -> pick up lava, get BucketLava
--   * BucketWater -> place Water at adjacent pos, become BucketEmpty
--   * BucketLava  -> place Lava at adjacent pos, become BucketEmpty
--   * BucketMilk / BucketEmpty on non-fluid -> no action
determineBucketAction :: BucketType -> BlockType -> BucketAction
determineBucketAction BucketEmpty Water = BucketPickup Water BucketWater
determineBucketAction BucketEmpty Lava  = BucketPickup Lava BucketLava
determineBucketAction BucketEmpty _     = BucketNoAction
determineBucketAction BucketWater _     = BucketPlace Water BucketEmpty
determineBucketAction BucketLava  _     = BucketPlace Lava BucketEmpty
determineBucketAction BucketMilk  _     = BucketNoAction

-- | Convert a bucket type to the corresponding fluid block type.
--   Returns Nothing for empty/milk buckets.
bucketTypeToFluidBlock :: BucketType -> Maybe BlockType
bucketTypeToFluidBlock BucketWater = Just Water
bucketTypeToFluidBlock BucketLava  = Just Lava
bucketTypeToFluidBlock _           = Nothing

-- | Convert a fluid block type to the corresponding filled bucket type.
--   Returns Nothing for non-fluid blocks.
fluidBlockToBucketType :: BlockType -> Maybe BucketType
fluidBlockToBucketType Water = Just BucketWater
fluidBlockToBucketType Lava  = Just BucketLava
fluidBlockToBucketType _     = Nothing
