module World.Generation
  ( generateChunk
  , GenerationConfig(..)
  , defaultGenConfig
  ) where

import World.Block (BlockType(..))
import World.Chunk
import World.Biome
import World.Noise

import Control.Monad (when, forM_)
import Data.Bits (xor, shiftR)
import Data.IORef (readIORef, writeIORef)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Linear (V2(..))

-- | Configuration for world generation
data GenerationConfig = GenerationConfig
  { gcSeed          :: !Seed
  , gcSeaLevel      :: !Int
  , gcBedrockLayers :: !Int
  , gcCaveThreshold :: !Double
  } deriving stock (Show, Eq)

defaultGenConfig :: GenerationConfig
defaultGenConfig = GenerationConfig
  { gcSeed          = 42
  , gcSeaLevel      = 63
  , gcBedrockLayers = 5
  , gcCaveThreshold = 0.55
  }

-- | Generate a complete chunk with terrain, caves, ores, and trees.
generateChunk :: GenerationConfig -> ChunkPos -> IO Chunk
generateChunk cfg chunkCoord = do
  chunk <- newChunk chunkCoord
  let V2 cx cz = chunkCoord
      seed = gcSeed cfg

  blocksVec <- readIORef (chunkBlocks chunk)
  mvec <- UV.thaw blocksVec

  -- Pass 1: Terrain heightmap + biome fill
  forM_ [0..chunkWidth-1] $ \lx ->
    forM_ [0..chunkDepth-1] $ \lz -> do
      let wx = fromIntegral (cx * chunkWidth + lx) :: Double
          wz = fromIntegral (cz * chunkDepth + lz) :: Double
          biome = biomeAt seed wx wz
          bp    = biomeParams biome
          heightNoise = fractalNoise2D seed 6 2.0 0.5 (wx * 0.01) (wz * 0.01)
          surfaceHeight = max 1 . min (chunkHeight - 1) . round
                        $ bpBaseHeight bp + heightNoise * bpHeightScale bp

      -- Fill solid column
      forM_ [0..surfaceHeight] $ \y -> do
        let block
              | y == 0 = Bedrock
              | y < gcBedrockLayers cfg && noiseInt seed lx y lz < 2 = Bedrock
              | y == surfaceHeight = bpSurfaceBlock bp
              | y >= surfaceHeight - 3 = bpSubBlock bp
              | otherwise = Stone
        MUV.write mvec (blockIndex lx y lz) (blockW8 block)

      -- Fill water to sea level
      when (surfaceHeight < gcSeaLevel cfg) $
        forM_ [surfaceHeight + 1 .. gcSeaLevel cfg] $ \y ->
          MUV.write mvec (blockIndex lx y lz) (blockW8 Water)

  -- Pass 2: Cave carving (3D noise)
  forM_ [0..chunkWidth-1] $ \lx ->
    forM_ [0..chunkDepth-1] $ \lz ->
      forM_ [1..128] $ \y -> do
        let wx = fromIntegral (cx * chunkWidth + lx) :: Double
            wz = fromIntegral (cz * chunkDepth + lz) :: Double
            wy = fromIntegral y :: Double
            caveNoise = fractalNoise3D (seed + 3000) 3 2.0 0.5
                          (wx * 0.03) (wy * 0.03) (wz * 0.03)
        when (caveNoise > gcCaveThreshold cfg) $ do
          cur <- MUV.read mvec (blockIndex lx y lz)
          when (cur /= blockW8 Bedrock && cur /= blockW8 Water) $
            MUV.write mvec (blockIndex lx y lz) (blockW8 Air)

  -- Pass 3: Ore veins
  forM_ [0..chunkWidth-1] $ \lx ->
    forM_ [0..chunkDepth-1] $ \lz ->
      forM_ [1..80] $ \y -> do
        cur <- MUV.read mvec (blockIndex lx y lz)
        when (cur == blockW8 Stone) $ do
          let wx = fromIntegral (cx * chunkWidth + lx) :: Double
              wz = fromIntegral (cz * chunkDepth + lz) :: Double
              wy = fromIntegral y :: Double
              n = noise3D (seed + 4000) (wx * 0.1) (wy * 0.1) (wz * 0.1)
              mOre
                | y < 16 && n > 0.85 = Just DiamondOre
                | y < 32 && n > 0.80 = Just GoldOre
                | y < 64 && n > 0.75 = Just IronOre
                | y < 80 && n > 0.70 = Just CoalOre
                | otherwise           = Nothing
          case mOre of
            Just ore -> MUV.write mvec (blockIndex lx y lz) (blockW8 ore)
            Nothing  -> pure ()

  -- Pass 4: Trees (within chunk bounds to avoid border issues)
  forM_ [2..chunkWidth-3] $ \lx ->
    forM_ [2..chunkDepth-3] $ \lz -> do
      let wx = fromIntegral (cx * chunkWidth + lx) :: Double
          wz = fromIntegral (cz * chunkDepth + lz) :: Double
          biome = biomeAt seed wx wz
          bp = biomeParams biome
          treeNoise = abs (noise2D (seed + 5000) (wx * 0.5) (wz * 0.5))
      when (treeNoise < bpTreeDensity bp) $ do
        surfY <- findSurface mvec lx lz (chunkHeight - 1)
        when (surfY > gcSeaLevel cfg) $ do
          surfBlock <- MUV.read mvec (blockIndex lx surfY lz)
          when (surfBlock == blockW8 (bpSurfaceBlock bp)) $
            placeTree mvec lx surfY lz

  frozen <- UV.freeze mvec
  writeIORef (chunkBlocks chunk) frozen
  writeIORef (chunkDirty chunk) True
  pure chunk

-- | Find the highest non-air block in a column
findSurface :: MUV.IOVector Word8 -> Int -> Int -> Int -> IO Int
findSurface mvec lx lz y
  | y <= 0    = pure 0
  | otherwise = do
      b <- MUV.read mvec (blockIndex lx y lz)
      if b /= blockW8 Air && b /= blockW8 Water
        then pure y
        else findSurface mvec lx lz (y - 1)

-- | Place a simple oak tree at (x, surfY, z)
placeTree :: MUV.IOVector Word8 -> Int -> Int -> Int -> IO ()
placeTree mvec x surfY z = do
  let trunkH = 5

  -- Trunk
  forM_ [1..trunkH] $ \dy -> do
    let y = surfY + dy
    when (y < chunkHeight) $
      MUV.write mvec (blockIndex x y z) (blockW8 OakLog)

  -- Leaf canopy (3x3x2 around trunk top)
  let topY = surfY + trunkH
  forM_ [-1..1] $ \dx ->
    forM_ [-1..1] $ \dz ->
      forM_ [0..1] $ \dy -> do
        let lx = x + dx; lz = z + dz; ly = topY + dy
        when (isInBounds lx ly lz && not (dx == 0 && dz == 0 && dy == 0)) $ do
          cur <- MUV.read mvec (blockIndex lx ly lz)
          when (cur == blockW8 Air) $
            MUV.write mvec (blockIndex lx ly lz) (blockW8 OakLeaves)

  -- Crown leaf
  when (topY + 2 < chunkHeight) $
    MUV.write mvec (blockIndex x (topY + 2) z) (blockW8 OakLeaves)

-- | Convert BlockType to Word8
blockW8 :: BlockType -> Word8
blockW8 = fromIntegral . fromEnum
{-# INLINE blockW8 #-}

-- | Quick integer noise for bedrock randomization
noiseInt :: Seed -> Int -> Int -> Int -> Int
noiseInt seed x y z =
  let h0 = (seed * 31 + x * 7 + y * 13 + z * 17)
      h1 = (h0 `xor` (h0 `shiftR` 16)) * 0x45d9f3b
      h2 = (h1 `xor` (h1 `shiftR` 16)) * 0x45d9f3b
      h3 = h2 `xor` (h2 `shiftR` 16)
  in abs h3 `mod` 5
