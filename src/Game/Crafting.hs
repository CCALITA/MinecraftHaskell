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
import Game.Inventory (ItemStack(..))
import qualified Data.Vector as V

-- | Crafting grid (2x2 or 3x3)
data CraftingGrid = CraftingGrid
  { cgSize  :: !Int                            -- 2 or 3
  , cgSlots :: !(V.Vector (Maybe BlockType))   -- row-major
  } deriving stock (Show, Eq)

-- | A crafting recipe
data Recipe = Recipe
  { rcPattern  :: ![[Maybe BlockType]]   -- 2D pattern (rows of columns)
  , rcResult   :: !BlockType
  , rcCount    :: !Int                   -- how many items produced
  } deriving stock (Show, Eq)

-- | Result of attempting to craft
data CraftResult
  = CraftSuccess !BlockType !Int   -- produced item and count
  | CraftFailure                   -- no matching recipe
  deriving stock (Show, Eq)

-- | Empty crafting grid
emptyCraftingGrid :: Int -> CraftingGrid
emptyCraftingGrid size = CraftingGrid
  { cgSize  = size
  , cgSlots = V.replicate (size * size) Nothing
  }

-- | Set a slot in the crafting grid (row, col)
setCraftingSlot :: CraftingGrid -> Int -> Int -> Maybe BlockType -> CraftingGrid
setCraftingSlot grid row col bt
  | row >= 0 && row < cgSize grid && col >= 0 && col < cgSize grid =
      grid { cgSlots = cgSlots grid V.// [(row * cgSize grid + col, bt)] }
  | otherwise = grid

-- | Get a slot from the crafting grid (row, col)
getCraftingSlot :: CraftingGrid -> Int -> Int -> Maybe BlockType
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
extractPattern :: CraftingGrid -> [[Maybe BlockType]]
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
findRecipe :: [[Maybe BlockType]] -> Maybe Recipe
findRecipe pattern = go allRecipes
  where
    go [] = Nothing
    go (r:rs)
      | matchesPattern (rcPattern r) pattern = Just r
      | otherwise = go rs

-- | Check if a recipe pattern matches the extracted grid pattern
matchesPattern :: [[Maybe BlockType]] -> [[Maybe BlockType]] -> Bool
matchesPattern recipe grid =
  length recipe == length grid &&
  all (\(rRow, gRow) -> length rRow == length gRow && all matchSlot (zip rRow gRow)) (zip recipe grid)
  where
    matchSlot (Nothing, Nothing) = True
    matchSlot (Just a, Just b)   = a == b
    matchSlot _                  = False

-- | All crafting recipes
allRecipes :: [Recipe]
allRecipes =
  -- Basic block conversions
  [ -- 4 wood planks from 1 log
    Recipe
      { rcPattern = [[Just OakLog]]
      , rcResult  = OakPlanks
      , rcCount   = 4
      }
  -- Crafting table from 4 planks (2x2)
  , Recipe
      { rcPattern = [[Just OakPlanks, Just OakPlanks]
                    ,[Just OakPlanks, Just OakPlanks]]
      , rcResult  = CraftingTable
      , rcCount   = 1
      }
  -- Sticks from 2 planks (vertical) — using OakLog as "stick" placeholder
  , Recipe
      { rcPattern = [[Just OakPlanks]
                    ,[Just OakPlanks]]
      , rcResult  = OakLog
      , rcCount   = 4
      }
  -- Furnace from 8 cobblestone
  , Recipe
      { rcPattern = [[Just Cobblestone, Just Cobblestone, Just Cobblestone]
                    ,[Just Cobblestone, Nothing,          Just Cobblestone]
                    ,[Just Cobblestone, Just Cobblestone, Just Cobblestone]]
      , rcResult  = Furnace
      , rcCount   = 1
      }
  -- Chest from 8 planks
  , Recipe
      { rcPattern = [[Just OakPlanks, Just OakPlanks, Just OakPlanks]
                    ,[Just OakPlanks, Nothing,        Just OakPlanks]
                    ,[Just OakPlanks, Just OakPlanks, Just OakPlanks]]
      , rcResult  = Chest
      , rcCount   = 1
      }
  -- Torch from coal + stick (OakLog placeholder)
  , Recipe
      { rcPattern = [[Just CoalOre]
                    ,[Just OakLog]]
      , rcResult  = Torch
      , rcCount   = 4
      }
  -- Stone bricks from 4 stone
  , Recipe
      { rcPattern = [[Just Stone, Just Stone]
                    ,[Just Stone, Just Stone]]
      , rcResult  = StoneBrick
      , rcCount   = 4
      }
  -- Glass from sand (smelting placeholder - 1:1)
  , Recipe
      { rcPattern = [[Just Sand]]
      , rcResult  = Glass
      , rcCount   = 1
      }
  -- Bricks from clay
  , Recipe
      { rcPattern = [[Just Clay, Just Clay]
                    ,[Just Clay, Just Clay]]
      , rcResult  = Brick
      , rcCount   = 1
      }
  -- TNT from sand + gravel pattern
  , Recipe
      { rcPattern = [[Just Sand,   Just Gravel, Just Sand]
                    ,[Just Gravel, Just Sand,   Just Gravel]
                    ,[Just Sand,   Just Gravel, Just Sand]]
      , rcResult  = TNT
      , rcCount   = 1
      }
  ]
