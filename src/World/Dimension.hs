module World.Dimension
  ( DimensionType(..)
  , DimensionConfig(..)
  , overworldConfig
  , netherConfig
  , endConfig
  , generateNetherChunk
  ) where

import World.Block (BlockType(..))
import World.Chunk
import World.Noise

import Control.Monad (when, forM_)
import Data.Bits (xor, shiftR)
import Data.IORef (writeIORef)
import Data.Word (Word8)
import Linear (V2(..))
import qualified Data.Vector.Unboxed.Mutable as MUV

-- | The three Minecraft dimensions.
data DimensionType = Overworld | Nether | TheEnd
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Per-dimension configuration.
data DimensionConfig = DimensionConfig
  { dcName        :: !String
  , dcSeaLevel    :: !Int
  , dcHasWeather  :: !Bool
  , dcHasDayNight :: !Bool
  , dcCeilingY    :: !(Maybe Int)  -- Nothing = open sky, Just n = ceiling at y=n
  , dcGravity     :: !Float
  } deriving stock (Show, Eq)

-- | Overworld: open sky, weather, day/night cycle, sea level 63.
overworldConfig :: DimensionConfig
overworldConfig = DimensionConfig
  { dcName        = "Overworld"
  , dcSeaLevel    = 63
  , dcHasWeather  = True
  , dcHasDayNight = True
  , dcCeilingY    = Nothing
  , dcGravity     = 1.0
  }

-- | Nether: bedrock ceiling at 128, no weather, no day/night.
netherConfig :: DimensionConfig
netherConfig = DimensionConfig
  { dcName        = "Nether"
  , dcSeaLevel    = 32
  , dcHasWeather  = False
  , dcHasDayNight = False
  , dcCeilingY    = Just 128
  , dcGravity     = 1.0
  }

-- | The End: no ceiling, no weather, no day/night.
endConfig :: DimensionConfig
endConfig = DimensionConfig
  { dcName        = "The End"
  , dcSeaLevel    = 0
  , dcHasWeather  = False
  , dcHasDayNight = False
  , dcCeilingY    = Nothing
  , dcGravity     = 1.0
  }

-- ---------------------------------------------------------------------------
-- Nether chunk generation
-- ---------------------------------------------------------------------------

-- | Generate a single Nether chunk.
--
--  Layout (y = 0..127):
--    y = 0       : bedrock floor
--    y = 1..126  : netherrack, carved by 3-D noise caves
--    y = 127     : bedrock ceiling
--    y <= 32     : lava fills any air pocket (lava ocean)
--    ceiling     : glowstone clusters hang from the roof
--    lava level  : soul sand patches on shore blocks
generateNetherChunk :: Seed -> ChunkPos -> IO Chunk
generateNetherChunk seed chunkCoord = do
  chunk <- newChunk chunkCoord
  let V2 cx cz = chunkCoord
      mvec = chunkBlocks chunk
      lavaLevel = 32   :: Int
      ceilingY  = 127  :: Int

  -- Pass 1 --- solid netherrack fill with bedrock floor / ceiling
  forM_ [0 .. chunkWidth - 1] $ \lx ->
    forM_ [0 .. chunkDepth - 1] $ \lz ->
      forM_ [0 .. ceilingY] $ \y -> do
        let block
              | y == 0       = Bedrock
              | y == ceilingY = Bedrock
              | otherwise    = Stone   -- stands in for netherrack (no dedicated BlockType yet)
        MUV.write mvec (blockIndex lx y lz) (blockW8 block)

  -- Pass 2 --- 3-D cave carving
  forM_ [0 .. chunkWidth - 1] $ \lx ->
    forM_ [0 .. chunkDepth - 1] $ \lz ->
      forM_ [1 .. ceilingY - 1] $ \y -> do
        let wx = fromIntegral (cx * chunkWidth + lx) :: Double
            wz = fromIntegral (cz * chunkDepth + lz) :: Double
            wy = fromIntegral y :: Double
            cave = fractalNoise3D (seed + 10000) 3 2.0 0.5
                     (wx * 0.04) (wy * 0.04) (wz * 0.04)
        when (cave > 0.35) $
          MUV.write mvec (blockIndex lx y lz) (blockW8 Air)

  -- Pass 3 --- lava ocean: fill air at or below lava level with Lava
  forM_ [0 .. chunkWidth - 1] $ \lx ->
    forM_ [0 .. chunkDepth - 1] $ \lz ->
      forM_ [1 .. lavaLevel] $ \y -> do
        cur <- MUV.read mvec (blockIndex lx y lz)
        when (cur == blockW8 Air) $
          MUV.write mvec (blockIndex lx y lz) (blockW8 Lava)

  -- Pass 4 --- soul sand patches on solid blocks at lava-level shore
  forM_ [0 .. chunkWidth - 1] $ \lx ->
    forM_ [0 .. chunkDepth - 1] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          y  = lavaLevel + 1
      when (y < ceilingY) $ do
        cur <- MUV.read mvec (blockIndex lx y lz)
        when (cur == blockW8 Stone) $ do
          let roll = hashPos (seed + 11000) wx wz `mod` 100
          when (roll < 30) $
            MUV.write mvec (blockIndex lx y lz) (blockW8 Sand)
            -- Sand stands in for soul sand (no dedicated BlockType yet)

  -- Pass 5 --- glowstone clusters on ceiling (hang from y = ceilingY - 1)
  forM_ [0 .. chunkWidth - 1] $ \lx ->
    forM_ [0 .. chunkDepth - 1] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          roll = hashPos (seed + 12000) wx wz `mod` 100
      when (roll < 8) $ do  -- ~8 % surface coverage
        let baseY = ceilingY - 1
        cur <- MUV.read mvec (blockIndex lx baseY lz)
        -- Only attach to solid ceiling
        when (cur == blockW8 Stone) $ do
          -- Hang 1-3 blocks down
          let depth = 1 + hashPos (seed + 12500) wx wz `mod` 3
          forM_ [0 .. depth - 1] $ \dy -> do
            let gy = baseY - dy
            when (gy > lavaLevel) $ do
              g <- MUV.read mvec (blockIndex lx gy lz)
              -- Replace solid or air (but not lava)
              when (g == blockW8 Stone || g == blockW8 Air) $
                MUV.write mvec (blockIndex lx gy lz) (blockW8 Torch)
                -- Torch emits light=14 and is the closest proxy for glowstone

  writeIORef (chunkDirty chunk) True
  pure chunk

-- ---------------------------------------------------------------------------
-- Internal helpers (same as World.Generation to avoid circular imports)
-- ---------------------------------------------------------------------------

-- | Convert BlockType to Word8
blockW8 :: BlockType -> Word8
blockW8 = fromIntegral . fromEnum
{-# INLINE blockW8 #-}

-- | Position-based hash (non-negative Int)
hashPos :: Seed -> Int -> Int -> Int
hashPos seed x z =
  let h0 = seed * 374761393 + x * 668265263 + z * 2147483647
      h1 = (h0 `xor` (h0 `shiftR` 13)) * 1274126177
      h2 = h1 `xor` (h1 `shiftR` 16)
  in abs h2
