module Game.Item
  ( Item(..)
  , ToolType(..)
  , ToolMaterial(..)
  , ToolInfo(..)
  , itemFromBlock
  , itemToBlock
  , isBlockItem
  , toolInfo
  , toolMiningSpeed
  , toolHarvestLevel
  , itemStackLimit
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

-- | An item in the inventory — either a placeable block or a tool
data Item
  = BlockItem !BlockType
  | ToolItem  !ToolType !ToolMaterial !Int  -- tool type, material, remaining durability
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
itemToBlock (BlockItem bt) = Just bt
itemToBlock (ToolItem {})  = Nothing

-- | Is this a placeable block item?
isBlockItem :: Item -> Bool
isBlockItem (BlockItem _) = True
isBlockItem _             = False

-- | Stack limit for an item (tools stack to 1, blocks to 64)
itemStackLimit :: Item -> Int
itemStackLimit (BlockItem _) = 64
itemStackLimit (ToolItem {}) = 1
