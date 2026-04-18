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
import Game.Item (Item(..), ToolType(..), ToolMaterial(..), toolInfo, ToolInfo(..), MaterialType(..), FoodType(..))
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

-- | Helper for non-block item pattern entries
ji :: Item -> Maybe Item
ji = Just

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
  -- Sticks from 2 planks (vertical)
  , Recipe
      { rcPattern = [[bi OakPlanks]
                    ,[bi OakPlanks]]
      , rcResult  = StickItem
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
  -- Torch from coal material + stick
  , Recipe
      { rcPattern = [[ji (MaterialItem Coal)]
                    ,[ji StickItem]]
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
  -- TNT from gunpowder + sand pattern
  , Recipe
      { rcPattern = [[ji (MaterialItem Gunpowder), bi Sand,                      ji (MaterialItem Gunpowder)]
                    ,[bi Sand,                      ji (MaterialItem Gunpowder), bi Sand]
                    ,[ji (MaterialItem Gunpowder), bi Sand,                      ji (MaterialItem Gunpowder)]]
      , rcResult  = BlockItem TNT
      , rcCount   = 1
      }
  -- Oak door: 6 planks in 2x3
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem OakDoorClosed
      , rcCount   = 1
      }
  -- Trapdoor: 6 planks in 2x3 → 2 trapdoors
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks, bi OakPlanks]
                    ,[bi OakPlanks, bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem TrapdoorClosed
      , rcCount   = 2
      }
  -- Ladder: sticks in H-pattern (3x3)
  , Recipe
      { rcPattern = [[ji StickItem, Nothing,       ji StickItem]
                    ,[ji StickItem, ji StickItem,   ji StickItem]
                    ,[ji StickItem, Nothing,       ji StickItem]]
      , rcResult  = BlockItem Ladder
      , rcCount   = 3
      }
  -- Oak fence: alternating planks and sticks in 2 rows
  , Recipe
      { rcPattern = [[bi OakPlanks, ji StickItem, bi OakPlanks]
                    ,[bi OakPlanks, ji StickItem, bi OakPlanks]]
      , rcResult  = BlockItem OakFence
      , rcCount   = 3
      }
  -- Fence gate: sticks and planks in alternating pattern
  , Recipe
      { rcPattern = [[ji StickItem, bi OakPlanks, ji StickItem]
                    ,[ji StickItem, bi OakPlanks, ji StickItem]]
      , rcResult  = BlockItem FenceGateClosed
      , rcCount   = 1
      }
  -- Bed: 3 wool on top + 3 planks on bottom
  , Recipe
      { rcPattern = [[bi Wool, bi Wool, bi Wool]
                    ,[bi OakPlanks, bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem Bed
      , rcCount   = 1
      }
  -- Bread: 3 wheat in a row
  , Recipe
      { rcPattern = [[ji (MaterialItem Wheat), ji (MaterialItem Wheat), ji (MaterialItem Wheat)]]
      , rcResult  = FoodItem Bread
      , rcCount   = 1
      }
  -- Lever: 1 stick on top of 1 cobblestone (vertical)
  , Recipe
      { rcPattern = [[ji StickItem]
                    ,[bi Cobblestone]]
      , rcResult  = BlockItem Lever
      , rcCount   = 1
      }
  -- Redstone dust: 1 cobblestone + 1 coal (placeholder recipe)
  , Recipe
      { rcPattern = [[bi Cobblestone]
                    ,[ji (MaterialItem Coal)]]
      , rcResult  = BlockItem RedstoneDust
      , rcCount   = 4
      }
  -- Stone stairs: 6 stone in stair pattern
  , Recipe
      { rcPattern = [[bi Stone,   Nothing,  Nothing]
                    ,[bi Stone,   bi Stone,  Nothing]
                    ,[bi Stone,   bi Stone,  bi Stone]]
      , rcResult  = BlockItem StoneStairs
      , rcCount   = 4
      }
  -- Oak stairs: 6 oak planks in stair pattern
  , Recipe
      { rcPattern = [[bi OakPlanks, Nothing,     Nothing]
                    ,[bi OakPlanks, bi OakPlanks, Nothing]
                    ,[bi OakPlanks, bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem OakStairs
      , rcCount   = 4
      }
  -- Iron door: 6 iron ingots in 2x3
  , Recipe
      { rcPattern = [[ji (MaterialItem IronIngot), ji (MaterialItem IronIngot)]
                    ,[ji (MaterialItem IronIngot), ji (MaterialItem IronIngot)]
                    ,[ji (MaterialItem IronIngot), ji (MaterialItem IronIngot)]]
      , rcResult  = BlockItem IronDoorClosed
      , rcCount   = 1
      }
  -- Flint and steel: iron ingot + flint
  , Recipe
      { rcPattern = [[ji (MaterialItem IronIngot)]
                    ,[ji (MaterialItem Flint)]]
      , rcResult  = FlintAndSteelItem 64
      , rcCount   = 1
      }
  -- Paper: 3 sugar cane in a row
  , Recipe
      { rcPattern = [[bi SugarCane, bi SugarCane, bi SugarCane]]
      , rcResult  = MaterialItem Paper
      , rcCount   = 3
      }
  -- Stone slabs: 3 stone in a row
  , Recipe
      { rcPattern = [[bi Stone, bi Stone, bi Stone]]
      , rcResult  = BlockItem StoneSlab
      , rcCount   = 6
      }
  -- Oak slabs: 3 oak planks in a row
  , Recipe
      { rcPattern = [[bi OakPlanks, bi OakPlanks, bi OakPlanks]]
      , rcResult  = BlockItem OakSlab
      , rcCount   = 6
      }
  -- Piston: planks on top, cobblestone + iron + cobblestone in middle, cobblestone + redstone + cobblestone on bottom
  , Recipe
      { rcPattern = [[bi OakPlanks,   bi OakPlanks,                 bi OakPlanks]
                    ,[bi Cobblestone, ji (MaterialItem IronIngot),  bi Cobblestone]
                    ,[bi Cobblestone, bi RedstoneDust,              bi Cobblestone]]
      , rcResult  = BlockItem Piston
      , rcCount   = 1
      }
  ] ++ toolRecipes ++ shearsRecipe

-- | Shears crafting recipe: 2 iron ingots diagonal
shearsRecipe :: [Recipe]
shearsRecipe =
  [ Recipe [[Nothing, ji (MaterialItem IronIngot)]
           ,[ji (MaterialItem IronIngot), Nothing]]
           (ShearsItem 238) 1
  ]

-- | Helper to create a tool item with full durability
tool :: ToolType -> ToolMaterial -> Item
tool tt tm = ToolItem tt tm (tiMaxDurability (toolInfo tm))

-- | Generate crafting recipes for all tool tiers
toolRecipes :: [Recipe]
toolRecipes = concatMap tierRecipes
  [ (Wood,      bi OakPlanks)
  , (StoneTier, bi Cobblestone)
  , (Iron,      ji (MaterialItem IronIngot))
  , (Diamond,   ji (MaterialItem DiamondGem))
  ]
  where
    stick = ji StickItem
    tierRecipes (mat, m) =
      [ -- Pickaxe: 3 material on top, 2 sticks below
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
      , -- Hoe: 2 material + 2 sticks
        Recipe [[m, m], [Nothing, stick], [Nothing, stick]]
               (tool Hoe mat) 1
      ]
