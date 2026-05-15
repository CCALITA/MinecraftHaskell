module Engine.MemoryPool
  ( PoolStats(..)
  , emptyPoolStats
  , trackAllocation
  , trackFree
  , poolUtilization
  ) where

-- | Statistics for a memory pool tracker.
data PoolStats = PoolStats
  { allocCount :: Int
  , freeCount  :: Int
  , totalBytes :: Int
  } deriving (Show, Eq)

-- | A fresh pool with no allocations.
emptyPoolStats :: PoolStats
emptyPoolStats = PoolStats
  { allocCount = 0
  , freeCount  = 0
  , totalBytes = 0
  }

-- | Record an allocation of the given byte size.
trackAllocation :: Int -> PoolStats -> PoolStats
trackAllocation bytes ps = PoolStats
  { allocCount = allocCount ps + 1
  , freeCount  = freeCount ps
  , totalBytes = totalBytes ps + bytes
  }

-- | Record a free of the given byte size.
trackFree :: Int -> PoolStats -> PoolStats
trackFree bytes ps = PoolStats
  { allocCount = allocCount ps
  , freeCount  = freeCount ps + 1
  , totalBytes = totalBytes ps - bytes
  }

-- | Ratio of live allocations to total allocations.
-- Returns 0.0 when no allocations have been made.
poolUtilization :: PoolStats -> Float
poolUtilization ps
  | allocCount ps == 0 = 0.0
  | otherwise = fromIntegral (allocCount ps - freeCount ps) / fromIntegral (allocCount ps)
