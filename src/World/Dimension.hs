module World.Dimension
  ( DimensionType(..)
  , DimensionConfig(..)
  , overworldConfig
  , netherConfig
  , endConfig
  , generateNetherChunk
  -- * Nether portal
  , PortalOrientation(..)
  , detectPortalFrame
  , netherCoords
  , overworldCoords
  , portalTransitTime
  ) where

import World.Block (BlockType(..))
import World.Chunk
import World.Noise

import Control.Monad (when, forM_)
import Data.Bits (xor, shiftR)
import Data.IORef (writeIORef)
import Data.Word (Word8)
import Linear (V2(..), V3(..))
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
-- Nether portal logic
-- ---------------------------------------------------------------------------

-- | Portal frame can face either X-axis or Z-axis.
data PortalOrientation = PortalX | PortalZ
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Time in seconds a player must stand on a NetherPortal to teleport.
portalTransitTime :: Float
portalTransitTime = 4.0

-- | Convert overworld coordinates to Nether coordinates (divide by 8).
netherCoords :: V3 Int -> V3 Int
netherCoords (V3 x y z) = V3 (x `div` 8) y (z `div` 8)

-- | Convert Nether coordinates to overworld coordinates (multiply by 8).
overworldCoords :: V3 Int -> V3 Int
overworldCoords (V3 x y z) = V3 (x * 8) y (z * 8)

-- | Detect a valid 4-wide x 5-tall obsidian portal frame surrounding the
--   given interior position. The frame must be:
--     - 4 obsidian columns (left/right edges) spanning 5 rows
--     - 2 obsidian blocks on top and bottom connecting the columns
--     - Interior is 2 wide x 3 tall (the positions that get filled with
--       NetherPortal blocks)
--
--   The @getBlock@ callback reads a block at a world position so this
--   function stays decoupled from the World module.
--
--   Returns @Just (orientation, [(V3 Int)])@ where the list contains the
--   interior positions to fill with NetherPortal, or @Nothing@ if no
--   valid frame is found.
detectPortalFrame
  :: (V3 Int -> IO BlockType)  -- ^ getBlock callback
  -> V3 Int                    -- ^ position of the obsidian block that was clicked
  -> IO (Maybe (PortalOrientation, [V3 Int]))
detectPortalFrame getBlock clickPos = do
  -- Try both orientations (X-axis portal, Z-axis portal)
  resultX <- tryOrientation PortalX clickPos getBlock
  case resultX of
    Just r  -> pure (Just r)
    Nothing -> tryOrientation PortalZ clickPos getBlock

-- | Try to find a valid portal frame in a given orientation.
--   We search for the bottom-left corner of the frame near the click position.
tryOrientation
  :: PortalOrientation
  -> V3 Int
  -> (V3 Int -> IO BlockType)
  -> IO (Maybe (PortalOrientation, [V3 Int]))
tryOrientation orient (V3 cx cy cz) getBlock = do
  -- The click could be on any obsidian block in the frame.
  -- Search a region around the click for a valid bottom-left corner.
  -- Frame is 4 wide x 5 tall. Bottom-left corner can be at most 3 blocks
  -- to the left and 4 blocks below the click.
  let offsets = [ (dx, dy) | dy <- [0, -1, -2, -3, -4]
                            , dx <- [0, -1, -2, -3] ]
  tryCorners offsets
  where
    tryCorners [] = pure Nothing
    tryCorners ((dx, dy) : rest) = do
      let cornerPos = case orient of
            PortalX -> V3 (cx + dx) (cy + dy) cz
            PortalZ -> V3 cx (cy + dy) (cz + dx)
      valid <- validateFrame orient cornerPos getBlock
      if valid
        then do
          let interior = interiorPositions orient cornerPos
          pure (Just (orient, interior))
        else tryCorners rest

-- | Validate that a 4x5 obsidian frame exists with the bottom-left corner
--   at the given position. The frame layout (2D cross-section):
--
--   @
--     .OO.     y+4  (top row: only middle 2 are obsidian in the top bar)
--     O..O     y+3
--     O..O     y+2
--     O..O     y+1
--     .OO.     y+0  (bottom row: only middle 2 are obsidian in the bottom bar)
--   @
--
--   Wait, standard Minecraft portal is:
--   @
--     .OO.     y+4
--     O..O     y+3
--     O..O     y+2
--     O..O     y+1
--     OOOO     y+0  (full bottom row)
--   @
--
--   Actually the standard 4x5 frame:
--   @
--     OOOO     y+4  (top)
--     O..O     y+3
--     O..O     y+2
--     O..O     y+1
--     OOOO     y+0  (bottom)
--   @
--   where the corners are obsidian too.
validateFrame
  :: PortalOrientation
  -> V3 Int            -- ^ bottom-left corner
  -> (V3 Int -> IO BlockType)
  -> IO Bool
validateFrame orient (V3 bx by bz) getBlock = do
  let pos w h = case orient of
        PortalX -> V3 (bx + w) (by + h) bz
        PortalZ -> V3 bx (by + h) (bz + w)
      -- Frame positions: the 4x5 outer ring
      framePositions =
        -- Bottom row (w = 0..3, h = 0)
        [ pos w 0 | w <- [0..3] ]
        -- Top row (w = 0..3, h = 4)
        ++ [ pos w 4 | w <- [0..3] ]
        -- Left column (w = 0, h = 1..3)
        ++ [ pos 0 h | h <- [1..3] ]
        -- Right column (w = 3, h = 1..3)
        ++ [ pos 3 h | h <- [1..3] ]
      -- Interior positions (should be Air or NetherPortal)
      interiorPos =
        [ pos w h | w <- [1..2], h <- [1..3] ]
  -- Check all frame blocks are Obsidian
  frameBlocks <- mapM getBlock framePositions
  let allObsidian = all (== Obsidian) frameBlocks
  -- Check interior is Air or already NetherPortal
  interiorBlocks <- mapM getBlock interiorPos
  let interiorClear = all (\b -> b == Air || b == NetherPortal) interiorBlocks
  pure (allObsidian && interiorClear)

-- | Compute the interior positions of a portal frame given its bottom-left corner.
--   Interior = 2 wide x 3 tall (w in [1..2], h in [1..3]).
interiorPositions :: PortalOrientation -> V3 Int -> [V3 Int]
interiorPositions orient (V3 bx by bz) =
  [ pos w h | w <- [1..2], h <- [1..3] ]
  where
    pos w h = case orient of
      PortalX -> V3 (bx + w) (by + h) bz
      PortalZ -> V3 bx (by + h) (bz + w)

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
