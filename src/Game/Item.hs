module Game.Item
  ( Item(..)
  , ToolType(..)
  , ToolMaterial(..)
  , FoodType(..)
  , MaterialType(..)
  , ToolInfo(..)
  , itemFromBlock
  , itemToBlock
  , isBlockItem
  , toolInfo
  , toolMiningSpeed
  , toolHarvestLevel
  , itemStackLimit
  , blockDrops
  , blockRequiredHarvestLevel
  , blockPreferredTool
  , mobDrops
  ) where

import World.Block (BlockType(..))
import System.Random (randomRIO)

-- | Tool types
data ToolType
  = Pickaxe
  | Shovel
  | Axe
  | Sword
  | Hoe
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Tool material tiers (ascending power)
data ToolMaterial
  = Wood
  | StoneTier     -- "StoneTier" to avoid clash with BlockType.Stone
  | Iron
  | Diamond
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Food item types (raw/cooked meats, bread, etc.)
data FoodType
  = RawPorkchop
  | CookedPorkchop
  | RawBeef
  | Steak
  | RawChicken
  | CookedChicken
  | Bread
  | Apple
  | RottenFlesh
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Material item types (crafting/drop materials)
data MaterialType
  = Coal
  | DiamondGem
  | IronIngot
  | GoldIngot
  | Bone
  | ArrowMat
  | StringMat
  | Gunpowder
  | Feather
  | Leather
  | WheatSeeds
  | Wheat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | An item in the inventory — block, tool, food, or material
data Item
  = BlockItem    !BlockType
  | ToolItem     !ToolType !ToolMaterial !Int  -- tool type, material, remaining durability
  | FoodItem     !FoodType
  | MaterialItem !MaterialType
  deriving stock (Show, Eq)

-- | Tool properties
data ToolInfo = ToolInfo
  { tiMiningSpeed   :: !Float   -- multiplier vs hand speed (1.0)
  , tiMaxDurability :: !Int     -- uses before breaking
  , tiHarvestLevel  :: !Int     -- 0=hand, 1=wood, 2=stone, 3=iron, 4=diamond
  , tiAttackDamage  :: !Float
  } deriving stock (Show, Eq)

-- | Get tool info for a material tier
toolInfo :: ToolMaterial -> ToolInfo
toolInfo = \case
  Wood      -> ToolInfo 2.0   59   1 2.0
  StoneTier -> ToolInfo 4.0  131   2 3.0
  Iron      -> ToolInfo 6.0  250   3 4.0
  Diamond   -> ToolInfo 8.0 1561   4 5.0

-- | Mining speed for a specific tool (affected by tool type matching)
toolMiningSpeed :: ToolType -> ToolMaterial -> Float
toolMiningSpeed _ mat = tiMiningSpeed (toolInfo mat)

-- | Harvest level for a material
toolHarvestLevel :: ToolMaterial -> Int
toolHarvestLevel = tiHarvestLevel . toolInfo

-- | Convert a block type to a block item
itemFromBlock :: BlockType -> Item
itemFromBlock = BlockItem

-- | Try to convert an item to a block type (for placement)
itemToBlock :: Item -> Maybe BlockType
itemToBlock (BlockItem bt)    = Just bt
itemToBlock (ToolItem {})     = Nothing
itemToBlock (FoodItem _)      = Nothing
itemToBlock (MaterialItem _)  = Nothing

-- | Is this a placeable block item?
isBlockItem :: Item -> Bool
isBlockItem (BlockItem _) = True
isBlockItem _             = False

-- | Stack limit for an item (tools stack to 1, everything else to 64)
itemStackLimit :: Item -> Int
itemStackLimit (ToolItem {})    = 1
itemStackLimit _                = 64

-- | What items a block drops when broken. Returns (item, count).
--   Some blocks require a minimum harvest level to drop anything.
blockDrops :: BlockType -> [(Item, Int)]
blockDrops = \case
  Air         -> []
  Stone       -> [(BlockItem Cobblestone, 1)]
  Dirt        -> [(BlockItem Dirt, 1)]
  Grass       -> [(BlockItem Dirt, 1)]
  Sand        -> [(BlockItem Sand, 1)]
  Gravel      -> [(BlockItem Gravel, 1)]
  OakLog      -> [(BlockItem OakLog, 1)]
  OakLeaves   -> []  -- sometimes drops saplings, simplified to nothing
  Water       -> []
  Lava        -> []
  Cobblestone -> [(BlockItem Cobblestone, 1)]
  OakPlanks   -> [(BlockItem OakPlanks, 1)]
  Glass       -> []  -- glass breaks without dropping
  Bedrock     -> []
  IronOre     -> [(BlockItem IronOre, 1)]  -- needs smelting for ingot
  CoalOre     -> [(BlockItem CoalOre, 1)]
  GoldOre     -> [(BlockItem GoldOre, 1)]
  DiamondOre  -> [(BlockItem DiamondOre, 1)]
  Snow        -> [(BlockItem Snow, 1)]
  Clay        -> [(BlockItem Clay, 1)]
  CraftingTable -> [(BlockItem CraftingTable, 1)]
  Furnace     -> [(BlockItem Furnace, 1)]
  Chest       -> [(BlockItem Chest, 1)]
  Torch       -> [(BlockItem Torch, 1)]
  StoneBrick  -> [(BlockItem StoneBrick, 1)]
  Brick       -> [(BlockItem Brick, 1)]
  TNT         -> [(BlockItem TNT, 1)]

-- | Minimum harvest level required to get drops from this block.
--   0 = hand, 1 = wood, 2 = stone, 3 = iron, 4 = diamond
blockRequiredHarvestLevel :: BlockType -> Int
blockRequiredHarvestLevel = \case
  IronOre     -> 2  -- stone pickaxe or better
  GoldOre     -> 3  -- iron pickaxe or better
  DiamondOre  -> 3  -- iron pickaxe or better
  Cobblestone -> 1  -- wood pickaxe or better
  Stone       -> 1
  StoneBrick  -> 1
  Brick       -> 1
  _           -> 0  -- hand is fine

-- | The preferred tool type for faster mining of this block.
--   Nothing means no tool preference (hand speed).
blockPreferredTool :: BlockType -> Maybe ToolType
blockPreferredTool = \case
  Stone       -> Just Pickaxe
  Cobblestone -> Just Pickaxe
  StoneBrick  -> Just Pickaxe
  Brick       -> Just Pickaxe
  IronOre     -> Just Pickaxe
  CoalOre     -> Just Pickaxe
  GoldOre     -> Just Pickaxe
  DiamondOre  -> Just Pickaxe
  Furnace     -> Just Pickaxe
  Dirt        -> Just Shovel
  Grass       -> Just Shovel
  Sand        -> Just Shovel
  Gravel      -> Just Shovel
  Clay        -> Just Shovel
  Snow        -> Just Shovel
  OakLog      -> Just Axe
  OakPlanks   -> Just Axe
  CraftingTable -> Just Axe
  Chest       -> Just Axe
  _           -> Nothing

-- | Loot table for mob deaths. Returns a list of (item, count) pairs
--   with randomized drop quantities.
mobDrops :: String -> IO [(Item, Int)]
mobDrops tag = case tag of
  "Pig"      -> randomDrops [(FoodItem RawPorkchop, 1, 3)]
  "Cow"      -> do meat  <- randomDrops [(FoodItem RawBeef, 1, 3)]
                   leath <- randomDrops [(MaterialItem Leather, 0, 2)]
                   pure (meat ++ leath)
  "Chicken"  -> do meat  <- randomDrops [(FoodItem RawChicken, 1, 1)]
                   feath <- randomDrops [(MaterialItem Feather, 0, 2)]
                   pure (meat ++ feath)
  "Sheep"    -> pure [(BlockItem OakPlanks, 1)]  -- wool placeholder
  "Zombie"   -> randomDrops [(FoodItem RottenFlesh, 0, 2)]
  "Skeleton" -> do bones  <- randomDrops [(MaterialItem Bone, 0, 2)]
                   arrows <- randomDrops [(MaterialItem ArrowMat, 0, 2)]
                   pure (bones ++ arrows)
  "Creeper"  -> randomDrops [(MaterialItem Gunpowder, 0, 2)]
  "Spider"   -> randomDrops [(MaterialItem StringMat, 0, 2)]
  _          -> pure []
 where
  randomDrops :: [(Item, Int, Int)] -> IO [(Item, Int)]
  randomDrops entries = do
    results <- mapM (\(item, lo, hi) -> do
      n <- randomRIO (lo, hi)
      pure (item, n)) entries
    pure $ filter (\(_, n) -> n > 0) results
