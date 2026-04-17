module World.Generation
  ( generateChunk
  , GenerationConfig(..)
  , defaultGenConfig
  , placeTreeWorld
  ) where

import World.Block (BlockType(..))
import World.Chunk
import World.Biome
import World.Noise

import Control.Monad (when, forM_)
import Data.Bits (xor, shiftR)
import Data.IORef (writeIORef)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Linear (V2(..), V3(..))

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

  let mvec = chunkBlocks chunk

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

  -- Pass 4: Trees — hash-based scatter to avoid grid artifacts
  forM_ [2..chunkWidth-3] $ \lx ->
    forM_ [2..chunkDepth-3] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          wxd = fromIntegral wx :: Double
          wzd = fromIntegral wz :: Double
          biome = biomeAt seed wxd wzd
          bp = biomeParams biome
          -- Hash the world position to get a pseudo-random value in [0, 1)
          h = hashPos (seed + 5000) wx wz
          treeRoll = fromIntegral (h `mod` 10000) / 10000.0 :: Double
      when (treeRoll < bpTreeDensity bp) $ do
        surfY <- findSurface mvec lx lz (chunkHeight - 1)
        when (surfY > gcSeaLevel cfg && surfY < chunkHeight - 10) $ do
          surfBlock <- MUV.read mvec (blockIndex lx surfY lz)
          when (surfBlock == blockW8 (bpSurfaceBlock bp)) $ do
            -- Vary tree height based on position hash
            let hTree = hashPos (seed + 6000) wx wz
                trunkH = 4 + hTree `mod` 3  -- height 4-6
                canopyR = 1 + hTree `mod` 2  -- radius 1-2
            placeTree mvec lx surfY lz trunkH canopyR

  -- Pass 5: Surface decoration (gravel patches, clay near water)
  forM_ [0..chunkWidth-1] $ \lx ->
    forM_ [0..chunkDepth-1] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          h = hashPos (seed + 7000) wx wz
          roll = h `mod` 1000
      surfY <- findSurface mvec lx lz (chunkHeight - 1)
      surfBlock <- MUV.read mvec (blockIndex lx surfY lz)
      -- Gravel patches on stone/dirt surfaces
      when (roll < 15 && (surfBlock == blockW8 Dirt || surfBlock == blockW8 Stone)) $
        MUV.write mvec (blockIndex lx surfY lz) (blockW8 Gravel)
      -- Clay near water (1 block below sea level)
      when (surfY == gcSeaLevel cfg - 1 && surfBlock == blockW8 Sand && roll < 80) $
        MUV.write mvec (blockIndex lx surfY lz) (blockW8 Clay)

  -- Pass 6: Small ponds (dig down a few blocks and fill with water)
  forM_ [4..chunkWidth-5] $ \lx ->
    forM_ [4..chunkDepth-5] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          pondRoll = hashPos (seed + 8000) (wx `div` 8) (wz `div` 8) `mod` 1000
      -- ~1% chance per 8x8 area to start a pond
      when (pondRoll < 10 && wx `mod` 8 == 0 && wz `mod` 8 == 0) $ do
        surfY <- findSurface mvec lx lz (chunkHeight - 1)
        when (surfY > gcSeaLevel cfg + 2) $ do
          -- Dig a 5x5x2 depression and fill with water
          forM_ [-2..2] $ \dx ->
            forM_ [-2..2] $ \dz -> do
              let px = lx + dx
                  pz = lz + dz
                  dist = abs dx + abs dz
                  depth = if dist <= 1 then 2 else 1
              when (px >= 0 && px < chunkWidth && pz >= 0 && pz < chunkDepth) $
                forM_ [0..depth-1] $ \dy -> do
                  let py = surfY - dy
                  when (py > 0) $ do
                    MUV.write mvec (blockIndex px py pz) (blockW8 Water)
                    -- Sand below water
                    when (dy == depth - 1 && py > 1) $
                      MUV.write mvec (blockIndex px (py - 1) pz) (blockW8 Sand)

  -- Pass 7: Cactus placement in desert biomes
  forM_ [1..chunkWidth-2] $ \lx ->
    forM_ [1..chunkDepth-2] $ \lz -> do
      let wx = cx * chunkWidth + lx
          wz = cz * chunkDepth + lz
          wxd = fromIntegral wx :: Double
          wzd = fromIntegral wz :: Double
          biome = biomeAt seed wxd wzd
      when (biome == Desert) $ do
        let h = hashPos (seed + 9000) wx wz
            roll = h `mod` 1000
        -- 2% chance per surface block, with 4-block spacing via grid quantization
        when (roll < 20 && (wx `mod` 4 == 0) && (wz `mod` 4 == 0)) $ do
          surfY <- findSurface mvec lx lz (chunkHeight - 1)
          surfBlock <- MUV.read mvec (blockIndex lx surfY lz)
          when (surfBlock == blockW8 Sand && surfY > gcSeaLevel cfg && surfY < chunkHeight - 5) $ do
            -- Check 4-side air clearance (within chunk bounds)
            let checkAir dx dz = do
                  let px = lx + dx; pz = lz + dz
                  if px >= 0 && px < chunkWidth && pz >= 0 && pz < chunkDepth
                    then do b <- MUV.read mvec (blockIndex px (surfY + 1) pz)
                            pure (b == blockW8 Air)
                    else pure True  -- out of chunk = assume air
            n <- checkAir 1 0
            s <- checkAir (-1) 0
            e <- checkAir 0 1
            w <- checkAir 0 (-1)
            when (n && s && e && w) $ do
              -- Place cactus column (1-3 tall)
              let hHeight = hashPos (seed + 9500) wx wz
                  cactusH = 1 + hHeight `mod` 3  -- 1 to 3
              forM_ [1..cactusH] $ \dy -> do
                let y = surfY + dy
                when (y < chunkHeight) $
                  MUV.write mvec (blockIndex lx y lz) (blockW8 Cactus)

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

-- | Place a tree at (x, surfY, z) with variable trunk height and canopy radius
placeTree :: MUV.IOVector Word8 -> Int -> Int -> Int -> Int -> Int -> IO ()
placeTree mvec x surfY z trunkH canopyR = do
  -- Trunk
  forM_ [1..trunkH] $ \dy -> do
    let y = surfY + dy
    when (y < chunkHeight) $
      MUV.write mvec (blockIndex x y z) (blockW8 OakLog)

  -- Leaf canopy — spheroid around trunk top
  let topY = surfY + trunkH
      -- Lower canopy: wider ring at trunk top - 1 and trunk top
      -- Upper canopy: narrower ring above
  forM_ [-canopyR..canopyR] $ \dx ->
    forM_ [-canopyR..canopyR] $ \dz ->
      forM_ [-1..2] $ \dy -> do
        let lx = x + dx; lz = z + dz; ly = topY + dy
            dist2 = dx * dx + dz * dz
            -- Taper: wider at bottom (dy=-1,0), narrower at top (dy=1,2)
            maxR2 = case dy of
              -1 -> canopyR * canopyR + 1
              0  -> canopyR * canopyR + 1
              1  -> canopyR * canopyR
              _  -> if canopyR > 1 then 1 else 0
            isTrunk = dx == 0 && dz == 0 && dy <= 0
        when (dist2 <= maxR2 && not isTrunk && isInBounds lx ly lz) $ do
          cur <- MUV.read mvec (blockIndex lx ly lz)
          when (cur == blockW8 Air) $
            MUV.write mvec (blockIndex lx ly lz) (blockW8 OakLeaves)

  -- Crown leaf on top
  let crownY = topY + 3
  when (crownY < chunkHeight && isInBounds x crownY z) $ do
    cur <- MUV.read mvec (blockIndex x crownY z)
    when (cur == blockW8 Air) $
      MUV.write mvec (blockIndex x crownY z) (blockW8 OakLeaves)

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

-- | Position-based hash for scatter placement (returns non-negative Int)
hashPos :: Seed -> Int -> Int -> Int
hashPos seed x z =
  let h0 = seed * 374761393 + x * 668265263 + z * 2147483647
      h1 = (h0 `xor` (h0 `shiftR` 13)) * 1274126177
      h2 = h1 `xor` (h1 `shiftR` 16)
  in abs h2

-- | Place a tree at a given world-coordinate base position.
--   Uses callback functions for block access to avoid circular imports.
--   getBlockCb: read block at world position
--   setBlockCb: write block at world position
placeTreeWorld
  :: (V3 Int -> IO BlockType)       -- ^ getBlock callback
  -> (V3 Int -> BlockType -> IO ()) -- ^ setBlock callback
  -> V3 Int                         -- ^ base position (sapling location)
  -> IO ()
placeTreeWorld getBlockCb setBlockCb (V3 bx by bz) = do
  let trunkH = 5   -- standard trunk height
      canopyR = 2  -- standard canopy radius
  -- Place trunk
  forM_ [1..trunkH] $ \dy -> do
    let y = by + dy
    when (y < chunkHeight) $
      setBlockCb (V3 bx y bz) OakLog
  -- Leaf canopy around trunk top
  let topY = by + trunkH
  forM_ [-canopyR..canopyR] $ \dx ->
    forM_ [-canopyR..canopyR] $ \dz ->
      forM_ [-1..2] $ \dy -> do
        let lx = bx + dx; lz = bz + dz; ly = topY + dy
            dist2 = dx * dx + dz * dz
            maxR2 = case dy of
              -1 -> canopyR * canopyR + 1
              0  -> canopyR * canopyR + 1
              1  -> canopyR * canopyR
              _  -> if canopyR > 1 then 1 else 0
            isTrunk = dx == 0 && dz == 0 && dy <= 0
        when (dist2 <= maxR2 && not isTrunk && ly > 0 && ly < chunkHeight) $ do
          cur <- getBlockCb (V3 lx ly lz)
          when (cur == Air) $
            setBlockCb (V3 lx ly lz) OakLeaves
  -- Crown leaf on top
  let crownY = topY + 3
  when (crownY > 0 && crownY < chunkHeight) $ do
    cur <- getBlockCb (V3 bx crownY bz)
    when (cur == Air) $
      setBlockCb (V3 bx crownY bz) OakLeaves
