module UI.Minimap
  ( minimapVerts
  , minimapSize
  , chunkGridForPlayer
  , chunkColor
  , playerDotColor
  ) where

import Linear (V2(..), V3(..))

-- | Minimap grid size: 7x7 chunks centered on the player
minimapSize :: Int
minimapSize = 7

-- | Color for a loaded chunk (green)
loadedColor :: (Float, Float, Float, Float)
loadedColor = (0.2, 0.7, 0.2, 0.85)

-- | Color for an unloaded chunk (dark gray)
unloadedColor :: (Float, Float, Float, Float)
unloadedColor = (0.25, 0.25, 0.25, 0.85)

-- | Color for the player dot (white)
playerDotColor :: (Float, Float, Float, Float)
playerDotColor = (1.0, 1.0, 1.0, 1.0)

-- | Background color (semi-transparent black)
bgColor :: (Float, Float, Float, Float)
bgColor = (0.0, 0.0, 0.0, 0.6)

-- | Return the appropriate color for a chunk cell
chunkColor :: Bool -> (Float, Float, Float, Float)
chunkColor True  = loadedColor
chunkColor False = unloadedColor

-- | Compute the 7x7 grid of chunk positions centered on the player's chunk.
--   Returns list of (V2 chunkX chunkZ, gridX, gridZ) where grid coords are 0..6
chunkGridForPlayer :: V3 Float -> [(V2 Int, Int, Int)]
chunkGridForPlayer (V3 px _ pz) =
  let half = minimapSize `div` 2
      -- Player's chunk coordinate (floor division to handle negatives)
      pcx = floor px `div` 16
      pcz = floor pz `div` 16
  in [ (V2 (pcx + dx) (pcz + dz), dx + half, dz + half)
     | dz <- [-half .. half]
     , dx <- [-half .. half]
     ]

-- | Generate HUD vertices for the chunk minimap in the top-right corner.
--   Takes the player position and a list of (chunkPos, isLoaded) pairs.
--   Returns flat list of floats: [x, y, r, g, b, a, ...] with 6 verts per quad.
--
--   Vulkan NDC: x in [-1,1], y in [-1,1] where y=-1 is top, y=+1 is bottom.
--   The minimap is placed in the top-right corner.
minimapVerts :: V3 Float -> [(V2 Int, Bool)] -> [Float]
minimapVerts playerPos chunkStates =
  let -- Minimap placement in NDC (top-right corner)
      margin    = 0.02 :: Float
      cellSize  = 0.035 :: Float  -- size of each chunk square
      gap       = 0.003 :: Float  -- gap between cells
      totalSize = fromIntegral minimapSize * (cellSize + gap) - gap
      -- Top-right corner: x goes from (1 - margin - totalSize) to (1 - margin)
      -- y goes from (-1 + margin) downward (in Vulkan NDC, -1 is top)
      mapX0     = 1.0 - margin - totalSize
      mapY0     = -1.0 + margin

      -- Background quad (slightly larger than the grid)
      pad = 0.01 :: Float
      bg = quad (mapX0 - pad) (mapY0 - pad) (mapX0 + totalSize + pad) (mapY0 + totalSize + pad) bgColor

      -- Build lookup from chunk positions
      grid = chunkGridForPlayer playerPos

      -- For each grid cell, find whether the chunk is loaded
      cellVerts = concatMap (renderCell chunkStates mapX0 mapY0 cellSize gap) grid

      -- Player dot: small white square in the center cell
      half = minimapSize `div` 2
      dotSize  = cellSize * 0.35
      dotCx    = mapX0 + (fromIntegral half) * (cellSize + gap) + cellSize / 2.0
      dotCy    = mapY0 + (fromIntegral half) * (cellSize + gap) + cellSize / 2.0
      playerDot = quad (dotCx - dotSize / 2.0) (dotCy - dotSize / 2.0)
                       (dotCx + dotSize / 2.0) (dotCy + dotSize / 2.0) playerDotColor

  in bg ++ cellVerts ++ playerDot

-- | Render a single cell of the minimap grid
renderCell :: [(V2 Int, Bool)] -> Float -> Float -> Float -> Float -> (V2 Int, Int, Int) -> [Float]
renderCell chunkStates mapX0 mapY0 cellSize gap (cpos, gx, gz) =
  let isLoaded = case lookup cpos chunkStates of
        Just loaded -> loaded
        Nothing     -> False
      color = chunkColor isLoaded
      cx = mapX0 + fromIntegral gx * (cellSize + gap)
      cy = mapY0 + fromIntegral gz * (cellSize + gap)
  in quad cx cy (cx + cellSize) (cy + cellSize) color

-- | Produce 6 vertices (2 triangles) for an axis-aligned quad.
--   Each vertex is 6 floats: x, y, r, g, b, a.
quad :: Float -> Float -> Float -> Float -> (Float, Float, Float, Float) -> [Float]
quad x0 y0 x1 y1 (r, g, b, a) =
  [ x0, y0, r, g, b, a
  , x1, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y1, r, g, b, a
  ]
