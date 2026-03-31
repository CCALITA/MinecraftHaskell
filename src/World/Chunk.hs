module World.Chunk
  ( Chunk(..)
  , ChunkPos
  , chunkWidth
  , chunkDepth
  , chunkHeight
  , newChunk
  , getBlock
  , setBlock
  , blockIndex
  , isInBounds
  , forEachBlock
  ) where

import World.Block (BlockType(..))

import Data.Word (Word8)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.IORef
import Linear (V2(..), V3(..))

-- | Chunk dimensions (Minecraft standard)
chunkWidth, chunkDepth, chunkHeight :: Int
chunkWidth  = 16
chunkDepth  = 16
chunkHeight = 256

chunkSize :: Int
chunkSize = chunkWidth * chunkDepth * chunkHeight

-- | Chunk position in chunk coordinates (each unit = 16 blocks)
type ChunkPos = V2 Int

-- | A 16x16x256 chunk of blocks stored as a flat unboxed vector.
--   Indexed as: x + z * 16 + y * 16 * 16
data Chunk = Chunk
  { chunkPos    :: !ChunkPos
  , chunkBlocks :: !(IORef (UV.Vector Word8))
  , chunkDirty  :: !(IORef Bool)
  }

-- | Create a new chunk filled with Air
newChunk :: ChunkPos -> IO Chunk
newChunk pos = do
  blocks <- newIORef $ UV.replicate chunkSize (fromIntegral $ fromEnum Air)
  dirty  <- newIORef True
  pure Chunk
    { chunkPos    = pos
    , chunkBlocks = blocks
    , chunkDirty  = dirty
    }

-- | Convert (x, y, z) local coordinates to flat index.
--   x, z in [0..15], y in [0..255]
blockIndex :: Int -> Int -> Int -> Int
blockIndex x y z = x + z * chunkWidth + y * chunkWidth * chunkDepth
{-# INLINE blockIndex #-}

-- | Check if local coordinates are in bounds
isInBounds :: Int -> Int -> Int -> Bool
isInBounds x y z =
  x >= 0 && x < chunkWidth &&
  z >= 0 && z < chunkDepth &&
  y >= 0 && y < chunkHeight
{-# INLINE isInBounds #-}

-- | Get block at local coordinates. Returns Air for out-of-bounds.
getBlock :: Chunk -> Int -> Int -> Int -> IO BlockType
getBlock chunk x y z
  | not (isInBounds x y z) = pure Air
  | otherwise = do
      blocks <- readIORef (chunkBlocks chunk)
      let idx = blockIndex x y z
      pure . toEnum . fromIntegral $ blocks UV.! idx
{-# INLINE getBlock #-}

-- | Set block at local coordinates. No-op for out-of-bounds. Marks chunk dirty.
setBlock :: Chunk -> Int -> Int -> Int -> BlockType -> IO ()
setBlock chunk x y z bt
  | not (isInBounds x y z) = pure ()
  | otherwise = do
      blocks <- readIORef (chunkBlocks chunk)
      let idx = blockIndex x y z
          val = fromIntegral $ fromEnum bt
          blocks' = blocks UV.// [(idx, val)]
      writeIORef (chunkBlocks chunk) blocks'
      writeIORef (chunkDirty chunk) True

-- | Iterate over all blocks with coordinates. Callback receives (x, y, z, blockType).
forEachBlock :: Chunk -> (Int -> Int -> Int -> BlockType -> IO ()) -> IO ()
forEachBlock chunk action = do
  blocks <- readIORef (chunkBlocks chunk)
  let go !i
        | i >= chunkSize = pure ()
        | otherwise = do
            let y = i `div` (chunkWidth * chunkDepth)
                remainder = i `mod` (chunkWidth * chunkDepth)
                z = remainder `div` chunkWidth
                x = remainder `mod` chunkWidth
                bt = toEnum . fromIntegral $ blocks UV.! i
            action x y z bt
            go (i + 1)
  go 0
