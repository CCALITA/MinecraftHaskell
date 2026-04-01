module World.Light
  ( LightMap
  , newLightMap
  , getBlockLight
  , getSkyLight
  , getTotalLight
  , propagateBlockLight
  , propagateSkyLight
  , updateLightOnBlockChange
  , maxLightLevel
  ) where

import World.Block (BlockType(..), BlockProperties(..), blockProperties)
import World.Chunk (Chunk(..), chunkWidth, chunkDepth, chunkHeight, getBlock, blockIndex, freezeBlocks)

import Data.Word (Word8)
import Data.IORef
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- | Maximum light level
maxLightLevel :: Word8
maxLightLevel = 15

-- | Light map stores block light and sky light for each position in a chunk
data LightMap = LightMap
  { lmBlockLight :: !(IORef (UV.Vector Word8))   -- block-emitted light (torches, lava)
  , lmSkyLight   :: !(IORef (UV.Vector Word8))   -- sky light (from above)
  }

chunkSize :: Int
chunkSize = chunkWidth * chunkDepth * chunkHeight

-- | Create a new light map (all zeros for block light, 15 for sky light at top)
newLightMap :: IO LightMap
newLightMap = do
  bl <- newIORef $ UV.replicate chunkSize 0
  sl <- newIORef $ UV.replicate chunkSize 0
  pure LightMap { lmBlockLight = bl, lmSkyLight = sl }

-- | Get block light at a position
getBlockLight :: LightMap -> Int -> Int -> Int -> IO Word8
getBlockLight lm x y z
  | not (inBounds x y z) = pure 0
  | otherwise = do
      v <- readIORef (lmBlockLight lm)
      pure $ v UV.! blockIndex x y z
{-# INLINE getBlockLight #-}

-- | Get sky light at a position
getSkyLight :: LightMap -> Int -> Int -> Int -> IO Word8
getSkyLight lm x y z
  | not (inBounds x y z) = pure 15  -- outside chunks get full sky light
  | otherwise = do
      v <- readIORef (lmSkyLight lm)
      pure $ v UV.! blockIndex x y z
{-# INLINE getSkyLight #-}

-- | Get total effective light level (max of block and sky light)
getTotalLight :: LightMap -> Int -> Int -> Int -> IO Word8
getTotalLight lm x y z = do
  bl <- getBlockLight lm x y z
  sl <- getSkyLight lm x y z
  pure (max bl sl)

inBounds :: Int -> Int -> Int -> Bool
inBounds x y z =
  x >= 0 && x < chunkWidth &&
  z >= 0 && z < chunkDepth &&
  y >= 0 && y < chunkHeight
{-# INLINE inBounds #-}

-- | 6-connected neighbors (dx, dy, dz)
neighbors :: [(Int, Int, Int)]
neighbors =
  [ (1, 0, 0), (-1, 0, 0)
  , (0, 1, 0), (0, -1, 0)
  , (0, 0, 1), (0, 0, -1)
  ]

-- | Propagate block light from all emitting blocks in a chunk (BFS flood fill)
propagateBlockLight :: Chunk -> LightMap -> IO ()
propagateBlockLight chunk lm = do
  -- Reset block light
  mv <- MV.replicate chunkSize (0 :: Word8)
  blocks <- freezeBlocks chunk

  -- Find all light-emitting blocks and seed the queue
  let findEmitters !i !queue
        | i >= chunkSize = pure queue
        | otherwise = do
            let bt = toEnum . fromIntegral $ blocks UV.! i :: BlockType
                emit = bpLightEmit (blockProperties bt)
            if emit > 0
              then do
                MV.write mv i emit
                let y = i `div` (chunkWidth * chunkDepth)
                    remainder = i `mod` (chunkWidth * chunkDepth)
                    z = remainder `div` chunkWidth
                    x = remainder `mod` chunkWidth
                findEmitters (i + 1) (queue |> (x, y, z, emit))
              else findEmitters (i + 1) queue

  initialQueue <- findEmitters 0 Seq.empty

  -- BFS propagation
  bfsPropagate mv blocks initialQueue

  -- Freeze and store
  result <- UV.unsafeFreeze mv
  writeIORef (lmBlockLight lm) result

-- | Propagate sky light downward from the top of the chunk, then spread horizontally
propagateSkyLight :: Chunk -> LightMap -> IO ()
propagateSkyLight chunk lm = do
  mv <- MV.replicate chunkSize (0 :: Word8)
  blocks <- freezeBlocks chunk

  -- Pass 1: vertical sky light. For each (x, z) column, trace downward from y=255
  -- Sky light is 15 until hitting an opaque block
  let seedQueue = go 0 0 Seq.empty
        where
          go !x !z !queue
            | x >= chunkWidth = pure queue
            | z >= chunkDepth = go (x + 1) 0 queue
            | otherwise = do
                queue' <- traceColumn mv blocks x z (chunkHeight - 1) 15 queue
                go x (z + 1) queue'

  queue0 <- seedQueue

  -- Pass 2: BFS horizontal spread
  bfsPropagate mv blocks queue0

  result <- UV.unsafeFreeze mv
  writeIORef (lmSkyLight lm) result

-- | Trace a column downward for sky light
traceColumn :: MV.IOVector Word8 -> UV.Vector Word8 -> Int -> Int -> Int -> Word8 -> Seq (Int, Int, Int, Word8) -> IO (Seq (Int, Int, Int, Word8))
traceColumn mv blocks x z y level queue
  | y < 0 = pure queue
  | level <= 0 = pure queue
  | otherwise = do
      let idx = blockIndex x y z
          bt = toEnum . fromIntegral $ blocks UV.! idx :: BlockType
          props = blockProperties bt
      if bpSolid props && not (bpTransparent props)
        then pure queue  -- opaque block stops sky light
        else do
          MV.write mv idx level
          -- Add to queue for horizontal spread if level > 1
          let queue' = if level > 1 then queue |> (x, y, z, level) else queue
          traceColumn mv blocks x z (y - 1) level queue'

-- | BFS light propagation shared by block light and sky light
bfsPropagate :: MV.IOVector Word8 -> UV.Vector Word8 -> Seq (Int, Int, Int, Word8) -> IO ()
bfsPropagate mv blocks queue = case Seq.viewl queue of
  Seq.EmptyL -> pure ()
  (x, y, z, level) Seq.:< rest
    | level <= 1 -> bfsPropagate mv blocks rest
    | otherwise -> do
        let newLevel = level - 1
        rest' <- foldNeighbors mv blocks rest x y z newLevel
        bfsPropagate mv blocks rest'

-- | Try to spread light to each neighbor
foldNeighbors :: MV.IOVector Word8 -> UV.Vector Word8 -> Seq (Int, Int, Int, Word8) -> Int -> Int -> Int -> Word8 -> IO (Seq (Int, Int, Int, Word8))
foldNeighbors mv blocks queue x y z newLevel = go neighbors queue
  where
    go [] q = pure q
    go ((dx, dy, dz):ns) q = do
      let nx = x + dx
          ny = y + dy
          nz = z + dz
      if not (inBounds nx ny nz)
        then go ns q
        else do
          let nidx = blockIndex nx ny nz
              bt = toEnum . fromIntegral $ blocks UV.! nidx :: BlockType
              props = blockProperties bt
          if bpSolid props && not (bpTransparent props)
            then go ns q  -- opaque blocks block light
            else do
              current <- MV.read mv nidx
              if newLevel > current
                then do
                  MV.write mv nidx newLevel
                  go ns (q |> (nx, ny, nz, newLevel))
                else go ns q

-- | Update lighting when a block is placed or removed.
--   For simplicity, re-propagates all light in the chunk.
--   A production system would do incremental updates.
updateLightOnBlockChange :: Chunk -> LightMap -> IO ()
updateLightOnBlockChange chunk lm = do
  propagateBlockLight chunk lm
  propagateSkyLight chunk lm
