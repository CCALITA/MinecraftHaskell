module Game.Item
  ( Item(..)
  , ToolType(..)
  , ToolMaterial(..)
  , ToolInfo(..)
  , FoodType(..)
  , MaterialType(..)
  , ArmorSlot(..)
  , ArmorMaterial(..)
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
  , foodHungerRestore
  , foodName
  , materialName
  , armorDefensePoints
  ) where

import World.Block (BlockType(..))

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

-- | Food types
data FoodType
  = RawPorkchop | CookedPorkchop | RawBeef | Steak
  | RawChicken | CookedChicken | Bread | Apple | RottenFlesh
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Material / resource types
data MaterialType
  = Coal | DiamondGem | IronIngot | GoldIngot
  | Bone | ArrowMat | StringMat | Gunpowder
  | Feather | Leather | WheatSeeds | Wheat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Armor equipment slots
data ArmorSlot = Helmet | Chestplate | Leggings | Boots
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Armor material tiers
data ArmorMaterial = LeatherArmor | IronArmor | GoldArmor | DiamondArmor
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | An item in the inventory
data Item
  = BlockItem    !BlockType
  | ToolItem     !ToolType !ToolMaterial !Int  -- tool type, material, remaining durability
  | StickItem
  | FoodItem     !FoodType
  | MaterialItem !MaterialType
  | ArmorItem    !ArmorSlot !ArmorMaterial !Int  -- slot, material, remaining durability
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
itemToBlock StickItem         = Nothing
itemToBlock (FoodItem _)      = Nothing
itemToBlock (MaterialItem _)  = Nothing
itemToBlock (ArmorItem {})    = Nothing

-- | Is this a placeable block item?
isBlockItem :: Item -> Bool
isBlockItem (BlockItem _) = True
isBlockItem _             = False

-- | Stack limit for an item
itemStackLimit :: Item -> Int
itemStackLimit (BlockItem _)    = 64
itemStackLimit (ToolItem {})    = 1
itemStackLimit StickItem        = 64
itemStackLimit (FoodItem _)     = 64
itemStackLimit (MaterialItem _) = 64
itemStackLimit (ArmorItem {})   = 1

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
  CoalOre     -> [(MaterialItem Coal, 1)]
  GoldOre     -> [(BlockItem GoldOre, 1)]
  DiamondOre  -> [(MaterialItem DiamondGem, 1)]
  Snow        -> [(BlockItem Snow, 1)]
  Clay        -> [(BlockItem Clay, 1)]
  CraftingTable -> [(BlockItem CraftingTable, 1)]
  Furnace     -> [(BlockItem Furnace, 1)]
  Chest       -> [(BlockItem Chest, 1)]
  Torch       -> [(BlockItem Torch, 1)]
  StoneBrick  -> [(BlockItem StoneBrick, 1)]
  Brick       -> [(BlockItem Brick, 1)]
  TNT         -> [(BlockItem TNT, 1)]
  Obsidian    -> [(BlockItem Obsidian, 1)]
  OakDoorClosed -> [(BlockItem OakDoorClosed, 1)]
  OakDoorOpen -> [(BlockItem OakDoorClosed, 1)]  -- always drops closed door
  Ladder      -> [(BlockItem Ladder, 1)]
  Bed         -> [(BlockItem Bed, 1)]
  OakFence    -> [(BlockItem OakFence, 1)]
  Farmland    -> [(BlockItem Dirt, 1)]
  WheatCrop   -> [(MaterialItem Wheat, 1)]
  OakSapling  -> [(BlockItem OakSapling, 1)]

-- | Minimum harvest level required to get drops from this block.
--   0 = hand, 1 = wood, 2 = stone, 3 = iron, 4 = diamond
blockRequiredHarvestLevel :: BlockType -> Int
blockRequiredHarvestLevel = \case
  IronOre     -> 2  -- stone pickaxe or better
  GoldOre     -> 3  -- iron pickaxe or better
  DiamondOre  -> 3  -- iron pickaxe or better
  Obsidian    -> 4  -- diamond pickaxe only
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
  Obsidian    -> Just Pickaxe
  Dirt        -> Just Shovel
  Grass       -> Just Shovel
  Sand        -> Just Shovel
  Gravel      -> Just Shovel
  Clay        -> Just Shovel
  Snow        -> Just Shovel
  Farmland    -> Just Shovel
  OakLog      -> Just Axe
  OakPlanks   -> Just Axe
  CraftingTable -> Just Axe
  Chest       -> Just Axe
  OakDoorClosed -> Just Axe
  OakDoorOpen -> Just Axe
  OakFence    -> Just Axe
  Bed         -> Just Axe
  _           -> Nothing

-- | How much hunger a food type restores
foodHungerRestore :: FoodType -> Int
foodHungerRestore = \case
  RawPorkchop    -> 3
  CookedPorkchop -> 8
  RawBeef        -> 3
  Steak          -> 8
  RawChicken     -> 2
  CookedChicken  -> 6
  Bread          -> 5
  Apple          -> 4
  RottenFlesh    -> 4

-- | Display name for a food type
foodName :: FoodType -> String
foodName = \case
  RawPorkchop    -> "Raw Porkchop"
  CookedPorkchop -> "Cooked Porkchop"
  RawBeef        -> "Raw Beef"
  Steak          -> "Steak"
  RawChicken     -> "Raw Chicken"
  CookedChicken  -> "Cooked Chicken"
  Bread          -> "Bread"
  Apple          -> "Apple"
  RottenFlesh    -> "Rotten Flesh"

-- | Display name for a material type
materialName :: MaterialType -> String
materialName = \case
  Coal       -> "Coal"
  DiamondGem -> "Diamond"
  IronIngot  -> "Iron Ingot"
  GoldIngot  -> "Gold Ingot"
  Bone       -> "Bone"
  ArrowMat   -> "Arrow"
  StringMat  -> "String"
  Gunpowder  -> "Gunpowder"
  Feather    -> "Feather"
  Leather    -> "Leather"
  WheatSeeds -> "Wheat Seeds"
  Wheat      -> "Wheat"

-- | Defense points for armor by slot and material
armorDefensePoints :: ArmorSlot -> ArmorMaterial -> Int
armorDefensePoints slot mat = case (slot, mat) of
  (Helmet, LeatherArmor)    -> 1; (Helmet, IronArmor)    -> 2; (Helmet, GoldArmor)    -> 2; (Helmet, DiamondArmor)    -> 3
  (Chestplate, LeatherArmor) -> 3; (Chestplate, IronArmor) -> 6; (Chestplate, GoldArmor) -> 5; (Chestplate, DiamondArmor) -> 8
  (Leggings, LeatherArmor)  -> 2; (Leggings, IronArmor)  -> 5; (Leggings, GoldArmor)  -> 3; (Leggings, DiamondArmor)  -> 6
  (Boots, LeatherArmor)     -> 1; (Boots, IronArmor)     -> 2; (Boots, GoldArmor)     -> 1; (Boots, DiamondArmor)     -> 3
