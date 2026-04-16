module Game.Crafting
  ( Recipe(..)
  , CraftingGrid(..)
  , CraftResult(..)
  , emptyCraftingGrid
  , setCraftingSlot
  , getCraftingSlot
  , tryCraft
  , allRecipes
  ) where

import World.Block (BlockType(..))
import Game.Item (Item(..), ToolType(..), ToolMaterial(..), toolInfo, ToolInfo(..))
import qualified Data.Vector as V

-- | Crafting grid (2x2 or 3x3)
data CraftingGrid = CraftingGrid
  { cgSize  :: !Int                            -- 2 or 3
  , cgSlots :: !(V.Vector (Maybe Item))        -- row-major
  } deriving stock (Show, Eq)

-- | A crafting recipe
data Recipe = Recipe
  { rcPattern  :: ![[Maybe Item]]     -- 2D pattern (rows of columns)
  , rcResult   :: !Item
  , rcCount    :: !Int                -- how many items produced
  } deriving stock (Show, Eq)

-- | Result of attempting to craft
data CraftResult
  = CraftSuccess !Item !Int   -- produced item and count
  | CraftFailure              -- no matching recipe
  deriving stock (Show, Eq)

-- | Empty crafting grid
emptyCraftingGrid :: Int -> CraftingGrid
emptyCraftingGrid size = CraftingGrid
  { cgSize  = size
  , cgSlots = V.replicate (size * size) Nothing
  }

-- | Set a slot in the crafting grid (row, col)
setCraftingSlot :: CraftingGrid -> Int -> Int -> Maybe Item -> CraftingGrid
setCraftingSlot grid row col bt
  | row >= 0 && row < cgSize grid && col >= 0 && col < cgSize grid =
      grid { cgSlots = cgSlots grid V.// [(row * cgSize grid + col, bt)] }
  | otherwise = grid

-- | Get a slot from the crafting grid (row, col)
getCraftingSlot :: CraftingGrid -> Int -> Int -> Maybe Item
getCraftingSlot grid row col
  | row >= 0 && row < cgSize grid && col >= 0 && col < cgSize grid =
      cgSlots grid V.! (row * cgSize grid + col)
  | otherwise = Nothing

-- | Try to craft from the current grid state
tryCraft :: CraftingGrid -> CraftResult
tryCraft grid =
  let pattern = extractPattern grid
  in case findRecipe pattern of
    Just recipe -> CraftSuccess (rcResult recipe) (rcCount recipe)
    Nothing     -> CraftFailure

-- | Extract the minimal bounding pattern from the grid (trim empty rows/cols)
extractPattern :: CraftingGrid -> [[Maybe Item]]
extractPattern grid =
  let size = cgSize grid
      rows = [ [ getCraftingSlot grid r c | c <- [0..size-1] ] | r <- [0..size-1] ]
      -- Trim empty rows from top and bottom
      trimmedRows = trimEmpty rows
      -- Trim empty columns from left and right
      trimmedCols = transposeGrid (trimEmpty (transposeGrid trimmedRows))
  in trimmedCols

-- | Trim empty rows from start and end
trimEmpty :: [[Maybe a]] -> [[Maybe a]]
trimEmpty = reverse . dropWhile allNothing . reverse . dropWhile allNothing
  where
    allNothing = all isNothing'
    isNothing' Nothing = True
    isNothing' _       = False

-- | Transpose a 2D list
transposeGrid :: [[a]] -> [[a]]
transposeGrid [] = []
transposeGrid ([] : _) = []
transposeGrid xss = map head xss : transposeGrid (map tail xss)

-- | Find a recipe matching the pattern
findRecipe :: [[Maybe Item]] -> Maybe Recipe
findRecipe pattern = go allRecipes
  where
    go [] = Nothing
    go (r:rs)
      | matchesPattern (rcPattern r) pattern = Just r
      | otherwise = go rs

-- | Check if a recipe pattern matches the extracted grid pattern
matchesPattern :: [[Maybe Item]] -> [[Maybe Item]] -> Bool
matchesPattern recipe grid =
  length recipe == length grid &&
  all (\(rRow, gRow) -> length rRow == length gRow && all matchSlot (zip rRow gRow)) (zip recipe grid)
  where
    matchSlot (Nothing, Nothing) = True
    matchSlot (Just a, Just b)   = a == b
    matchSlot _                  = False

-- | Helper to create a block item pattern entry
bi :: BlockType -> Maybe Item
bi = Just . BlockItem

-- | All crafting recipes
allRecipes :: [Recipe]
allRecipes =
  -- Basic block conversions
  [ -- 4 wood planks from 1 log
    Recipe
      { rcPattern = [[bi OakLog]]
      , rcResult  = BlockItem OakPlanks
      , rcCount   = 4
      }
  -- Crafting table from 4 planks (2x2)
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem CraftingTable
      , rcCount   = 1
      }
  -- Sticks from 2 planks (vertical) — using OakLog as "stick" placeholder
  , Recipe
      { rcPattern = [[bi OakPlanks]
                    ,[bi OakPlanks]]
      , rcResult  = BlockItem OakLog
      , rcCount   = 4
      }
  -- Furnace from 8 cobblestone
  , Recipe
      { rcPattern = [[bi Cobblestone, bi Cobblestone, bi Cobblestone]
                    ,[bi Cobblestone, Nothing,        bi Cobblestone]
                    ,[bi Cobblestone, bi Cobblestone, bi Cobblestone]]
      , rcResult  = BlockItem Furnace
      , rcCount   = 1
      }
  -- Chest from 8 planks
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, Nothing,      bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem Chest
      , rcCount   = 1
      }
  -- Torch from coal + stick (OakLog placeholder)
  , Recipe
      { rcPattern = [[bi CoalOre]
                    ,[bi OakLog]]
      , rcResult  = BlockItem Torch
      , rcCount   = 4
      }
  -- Stone bricks from 4 stone
  , Recipe
      { rcPattern = [[bi Stone, bi Stone]
                    ,[bi Stone, bi Stone]]
      , rcResult  = BlockItem StoneBrick
      , rcCount   = 4
      }
  -- Glass from sand (smelting placeholder - 1:1)
  , Recipe
      { rcPattern = [[bi Sand]]
      , rcResult  = BlockItem Glass
      , rcCount   = 1
      }
  -- Bricks from clay
  , Recipe
      { rcPattern = [[bi Clay, bi Clay]
                    ,[bi Clay, bi Clay]]
      , rcResult  = BlockItem Brick
      , rcCount   = 1
      }
  -- TNT from sand + gravel pattern
  , Recipe
      { rcPattern = [[bi Sand,   bi Gravel, bi Sand]
                    ,[bi Gravel, bi Sand,   bi Gravel]
                    ,[bi Sand,   bi Gravel, bi Sand]]
      , rcResult  = BlockItem TNT
      , rcCount   = 1
      }
  -- Door from 6 planks in 2x3 pattern
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem OakDoorClosed
      , rcCount   = 3
      }
  ] ++ toolRecipes

-- | Helper to create a tool item with full durability
tool :: ToolType -> ToolMaterial -> Item
tool tt tm = ToolItem tt tm (tiMaxDurability (toolInfo tm))

-- | Generate crafting recipes for all tool tiers
toolRecipes :: [Recipe]
toolRecipes = concatMap tierRecipes [(Wood, OakPlanks), (StoneTier, Cobblestone), (Iron, IronOre), (Diamond, DiamondOre)]
  where
    stick = bi OakLog  -- OakLog as stick placeholder
    tierRecipes (mat, matBlock) =
      let m = bi matBlock
      in [ -- Pickaxe: 3 material on top, 2 sticks below
           Recipe [[m, m, m], [Nothing, stick, Nothing], [Nothing, stick, Nothing]]
                  (tool Pickaxe mat) 1
         , -- Axe: 2 material + 1 material, 2 sticks
           Recipe [[m, m, Nothing], [m, stick, Nothing], [Nothing, stick, Nothing]]
                  (tool Axe mat) 1
         , -- Shovel: 1 material on top, 2 sticks
           Recipe [[m], [stick], [stick]]
                  (tool Shovel mat) 1
         , -- Sword: 2 material + 1 stick
           Recipe [[m], [m], [stick]]
                  (tool Sword mat) 1
         ]
