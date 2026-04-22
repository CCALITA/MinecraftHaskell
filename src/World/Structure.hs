module World.Structure
  ( Structure(..)
  , StructureBlock(..)
  , placeStructure
  , wellStructure
  , desertTempleStructure
  , dungeonStructure
  , villageHouseStructure
  ) where

import World.Block (BlockType(..))
import Linear (V3(..))
import Control.Monad (forM_)

-- | A single block placement relative to a structure origin.
data StructureBlock = StructureBlock
  { sbOffset :: !(V3 Int)    -- ^ Relative position from structure origin
  , sbBlock  :: !BlockType   -- ^ Block type to place
  } deriving stock (Show, Eq)

-- | A template structure defined by a name, bounding box, and block list.
data Structure = Structure
  { stName   :: !String         -- ^ Human-readable structure name
  , stSize   :: !(V3 Int)       -- ^ Bounding box dimensions (width, height, depth)
  , stBlocks :: ![StructureBlock] -- ^ All non-air blocks in the structure
  } deriving stock (Show, Eq)

-- | Place all blocks of a structure at the given origin using a callback.
--   The callback receives a world position and block type.
placeStructure :: (V3 Int -> BlockType -> IO ()) -> V3 Int -> Structure -> IO ()
placeStructure setBlock (V3 ox oy oz) structure =
  forM_ (stBlocks structure) $ \(StructureBlock (V3 dx dy dz) bt) ->
    setBlock (V3 (ox + dx) (oy + dy) (oz + dz)) bt

-- | 5x5 cobblestone well with water in the center column.
--   Origin is the bottom-southwest corner.
--   Layout (top-down view at y=0):
--     C C C C C
--     C W W W C
--     C W W W C
--     C W W W C
--     C C C C C
--   Walls rise 3 blocks high; water fills the interior at y=0.
wellStructure :: Structure
wellStructure = Structure
  { stName   = "Well"
  , stSize   = V3 5 4 5
  , stBlocks = floorBlocks <> wallBlocks <> waterBlocks
  }
  where
    -- Cobblestone floor at y=0
    floorBlocks =
      [ StructureBlock (V3 x 0 z) Cobblestone
      | x <- [0..4], z <- [0..4]
      ]
    -- Walls: perimeter columns from y=1 to y=3
    wallBlocks =
      [ StructureBlock (V3 x y z) Cobblestone
      | y <- [1..3]
      , x <- [0..4], z <- [0..4]
      , x == 0 || x == 4 || z == 0 || z == 4
      ]
    -- Water fills the 3x3 interior at y=1
    waterBlocks =
      [ StructureBlock (V3 x 1 z) Water
      | x <- [1..3], z <- [1..3]
      ]

-- | Desert temple: a sandstone pyramid with a hidden chamber below.
--   Origin is at the bottom of the hidden chamber.
--   Chamber occupies y=0..2, ground level at y=3, pyramid at y=4..7.
desertTempleStructure :: Structure
desertTempleStructure = Structure
  { stName   = "Desert Temple"
  , stSize   = V3 9 8 9
  , stBlocks = chamberBlocks <> baseBlocks <> pyramidBlocks <> chestBlock
  }
  where
    -- Hidden chamber: floor at y=0, walls at y=1..2, ceiling at y=3
    chamberBlocks =
      [ StructureBlock (V3 x 0 z) StoneBrick
      | x <- [2..6], z <- [2..6]
      ]
      <>
      [ StructureBlock (V3 x y z) StoneBrick
      | y <- [1, 2]
      , x <- [2..6], z <- [2..6]
      , x == 2 || x == 6 || z == 2 || z == 6
      ]
      <>
      [ StructureBlock (V3 x 3 z) StoneBrick
      | x <- [2..6], z <- [2..6]
      ]
    -- Sand base floor at y=3 (ground level, overwrites chamber ceiling outside)
    baseBlocks =
      [ StructureBlock (V3 x 3 z) Sand
      | x <- [0..8], z <- [0..8]
      , x < 2 || x > 6 || z < 2 || z > 6
      ]
    -- Stepped pyramid layers from y=4..7
    pyramidBlocks = concatMap pyramidLayer [0..3]
    pyramidLayer i =
      let lo = i
          hi = 8 - i
          y  = 4 + i
      in [ StructureBlock (V3 x y z) Sand
         | x <- [lo..hi], z <- [lo..hi]
         , x == lo || x == hi || z == lo || z == hi
         ]
    -- Chest in the center of the hidden chamber
    chestBlock =
      [ StructureBlock (V3 4 1 4) Chest ]

-- | Small dungeon room: 7x5x7 cobblestone room with a chest inside.
--   Walls are cobblestone, interior is air except for the chest.
dungeonStructure :: Structure
dungeonStructure = Structure
  { stName   = "Dungeon"
  , stSize   = V3 7 5 7
  , stBlocks = floorBlocks <> wallBlocks <> ceilingBlocks <> interiorBlocks
  }
  where
    -- Cobblestone floor at y=0
    floorBlocks =
      [ StructureBlock (V3 x 0 z) Cobblestone
      | x <- [0..6], z <- [0..6]
      ]
    -- Walls from y=1..3 (perimeter only)
    wallBlocks =
      [ StructureBlock (V3 x y z) Cobblestone
      | y <- [1..3]
      , x <- [0..6], z <- [0..6]
      , x == 0 || x == 6 || z == 0 || z == 6
      ]
    -- Ceiling at y=4
    ceilingBlocks =
      [ StructureBlock (V3 x 4 z) Cobblestone
      | x <- [0..6], z <- [0..6]
      ]
    -- Interior: chest at center, torch on wall
    interiorBlocks =
      [ StructureBlock (V3 3 1 3) Chest
      , StructureBlock (V3 1 2 3) Torch
      ]

-- | Simple village house: 5x4x5 oak planks shell with a door, torch, and bed.
villageHouseStructure :: Structure
villageHouseStructure = Structure
  { stName   = "Village House"
  , stSize   = V3 5 5 5
  , stBlocks = floorBlocks <> wallBlocks <> roofBlocks <> doorBlock <> interiorBlocks
  }
  where
    -- Oak planks floor at y=0
    floorBlocks =
      [ StructureBlock (V3 x 0 z) OakPlanks
      | x <- [0..4], z <- [0..4]
      ]
    -- Walls from y=1..3 (perimeter, leaving a gap for the door at (2,1,0) and (2,2,0))
    wallBlocks =
      [ StructureBlock (V3 x y z) OakPlanks
      | y <- [1..3]
      , x <- [0..4], z <- [0..4]
      , x == 0 || x == 4 || z == 0 || z == 4  -- perimeter
      , not (x == 2 && z == 0 && (y == 1 || y == 2))  -- door gap
      ]
    -- Roof at y=4
    roofBlocks =
      [ StructureBlock (V3 x 4 z) OakPlanks
      | x <- [0..4], z <- [0..4]
      ]
    -- Door in the wall gap
    doorBlock =
      [ StructureBlock (V3 2 1 0) OakDoorClosed ]
    -- Interior furnishings
    interiorBlocks =
      [ StructureBlock (V3 1 1 3) Bed
      , StructureBlock (V3 3 2 3) Torch
      ]
