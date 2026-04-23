module Game.Item
  ( Item(..)
  , ToolType(..)
  , ToolMaterial(..)
  , FoodType(..)
  , MaterialType(..)
  , ToolInfo(..)
  , ArmorSlot(..)
  , ArmorMaterial(..)
  , PotionType(..)
  , BucketType(..)
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
  , foodSaturation
  , foodName
  , materialName
  , armorDefensePoints
  , mobDrops
  , potionName
  , potionColor
  , itemMaxDurability
  , lookupItemByName
  ) where

import World.Block (BlockType(..))
import System.Random (randomRIO)
import Data.Binary (Binary(..), Get)
import Data.Word (Word8)
import Data.Char (toLower)

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
  | RawFish | CookedFish | RawSalmon | CookedSalmon
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Material / resource types
data MaterialType
  = Coal | DiamondGem | IronIngot | GoldIngot
  | Bone | ArrowMat | StringMat | Gunpowder
  | Feather | Leather | WheatSeeds | Wheat | Flint
  | Paper
  | LapisGem | Emerald | RedstoneDustMat
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Armor equipment slots
data ArmorSlot = Helmet | Chestplate | Leggings | Boots
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Armor material tiers
data ArmorMaterial = LeatherArmor | IronArmor | GoldArmor | DiamondArmor
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Potion types
data PotionType = WaterBottle | AwkwardPotion | PoisonPotion | HealingPotion | SpeedPotion
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Bucket content types
data BucketType = BucketEmpty | BucketWater | BucketLava | BucketMilk
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | An item in the inventory — a placeable block, a tool, or a material
data Item
  = BlockItem    !BlockType
  | ToolItem     !ToolType !ToolMaterial !Int  -- tool type, material, remaining durability
  | StickItem
  | FoodItem     !FoodType
  | MaterialItem !MaterialType
  | ArmorItem    !ArmorSlot !ArmorMaterial !Int  -- slot, material, remaining durability
  | ShearsItem   !Int                           -- remaining durability (max 238)
  | FlintAndSteelItem !Int  -- remaining durability (max 64)
  | CompassItem              -- shows direction to spawn point
  | ClockItem                -- shows time of day
  | FishingRodItem !Int     -- remaining durability (max 64)
  | GlassBottleItem
  | PotionItem !PotionType
  | BoatItem                -- placeable boat for water travel
  | MinecartItem
  | BucketItem !BucketType
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

-- | Maximum durability for items that have durability.
--   Returns Nothing for items without durability tracking.
itemMaxDurability :: Item -> Maybe Int
itemMaxDurability (ToolItem _ mat _) = Just (tiMaxDurability (toolInfo mat))
itemMaxDurability (ArmorItem slot mat _) = Just (armorMaxDur slot mat)
  where
    armorMaxDur Helmet     LeatherArmor  = 55
    armorMaxDur Helmet     IronArmor     = 165
    armorMaxDur Helmet     GoldArmor     = 77
    armorMaxDur Helmet     DiamondArmor  = 363
    armorMaxDur Chestplate LeatherArmor  = 80
    armorMaxDur Chestplate IronArmor     = 240
    armorMaxDur Chestplate GoldArmor     = 112
    armorMaxDur Chestplate DiamondArmor  = 528
    armorMaxDur Leggings   LeatherArmor  = 75
    armorMaxDur Leggings   IronArmor     = 225
    armorMaxDur Leggings   GoldArmor     = 105
    armorMaxDur Leggings   DiamondArmor  = 495
    armorMaxDur Boots      LeatherArmor  = 65
    armorMaxDur Boots      IronArmor     = 195
    armorMaxDur Boots      GoldArmor     = 91
    armorMaxDur Boots      DiamondArmor  = 429
itemMaxDurability (ShearsItem _) = Just 238
itemMaxDurability (FlintAndSteelItem _) = Just 64
itemMaxDurability (FishingRodItem _) = Just 64
itemMaxDurability _ = Nothing

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
itemToBlock (ShearsItem _)   = Nothing
itemToBlock (FlintAndSteelItem _) = Nothing
itemToBlock CompassItem = Nothing
itemToBlock ClockItem = Nothing
itemToBlock (FishingRodItem _) = Nothing
itemToBlock GlassBottleItem  = Nothing
itemToBlock (PotionItem _)   = Nothing
itemToBlock BoatItem = Nothing
itemToBlock MinecartItem     = Nothing
itemToBlock (BucketItem _)   = Nothing

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
itemStackLimit (ShearsItem _)   = 1
itemStackLimit (FlintAndSteelItem _) = 1
itemStackLimit CompassItem = 1
itemStackLimit ClockItem = 1
itemStackLimit (FishingRodItem _) = 1
itemStackLimit GlassBottleItem  = 16
itemStackLimit (PotionItem _)   = 1
itemStackLimit BoatItem = 1
itemStackLimit MinecartItem     = 1
itemStackLimit (BucketItem _)   = 16

-- | What items a block drops when broken. Returns (item, count).
--   Some blocks require a minimum harvest level to drop anything.
blockDrops :: BlockType -> [(Item, Int)]
blockDrops = \case
  Air         -> []
  Stone       -> [(BlockItem Cobblestone, 1)]
  Dirt        -> [(BlockItem Dirt, 1)]
  Grass       -> [(BlockItem Dirt, 1)]
  Sand        -> [(BlockItem Sand, 1)]
  Gravel      -> [(MaterialItem Flint, 1)]
  OakLog      -> [(BlockItem OakLog, 1)]
  OakLeaves   -> [(BlockItem OakSapling, 1)]  -- always drops sapling when broken by player
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
  Wool        -> [(BlockItem Wool, 1)]
  FenceGateClosed -> [(BlockItem FenceGateClosed, 1)]
  FenceGateOpen   -> [(BlockItem FenceGateClosed, 1)]  -- always drops closed form
  Lever       -> [(BlockItem Lever, 1)]
  RedstoneDust -> [(BlockItem RedstoneDust, 1)]
  TrapdoorClosed -> [(BlockItem TrapdoorClosed, 1)]
  TrapdoorOpen   -> [(BlockItem TrapdoorClosed, 1)]  -- always drops closed form
  StoneStairs  -> [(BlockItem StoneStairs, 1)]
  OakStairs    -> [(BlockItem OakStairs, 1)]
  IronDoorClosed -> [(BlockItem IronDoorClosed, 1)]
  IronDoorOpen -> [(BlockItem IronDoorClosed, 1)]  -- always drops closed door
  Fire        -> []
  Cactus      -> [(BlockItem Cactus, 1)]
  SugarCane   -> [(BlockItem SugarCane, 1)]
  StoneSlab   -> [(BlockItem StoneSlab, 1)]
  OakSlab     -> [(BlockItem OakSlab, 1)]
  Piston      -> [(BlockItem Piston, 1)]
  PistonHead  -> []
  Rail        -> [(BlockItem Rail, 1)]
  Dispenser   -> [(BlockItem Dispenser, 1)]
  EnchantingTable -> [(BlockItem EnchantingTable, 1)]
  Netherrack       -> [(BlockItem Netherrack, 1)]
  SoulSand         -> [(BlockItem SoulSand, 1)]
  Glowstone        -> [(MaterialItem RedstoneDustMat, 2)]  -- drops glowstone dust (redstone-like)
  NetherBrick      -> [(BlockItem NetherBrick, 1)]
  NetherPortal     -> []  -- cannot be broken for drops
  RedstoneOre      -> [(MaterialItem RedstoneDustMat, 4)]
  LapisOre         -> [(MaterialItem LapisGem, 4)]
  EmeraldOre       -> [(MaterialItem Emerald, 1)]
  MossyCobblestone -> [(BlockItem MossyCobblestone, 1)]
  MossyStoneBrick  -> [(BlockItem MossyStoneBrick, 1)]
  BirchLog         -> [(BlockItem BirchLog, 1)]
  BirchLeaves      -> [(BlockItem OakSapling, 1)]  -- drops sapling
  BirchPlanks      -> [(BlockItem BirchPlanks, 1)]
  SpruceLog        -> [(BlockItem SpruceLog, 1)]
  SpruceLeaves     -> [(BlockItem OakSapling, 1)]  -- drops sapling
  SprucePlanks     -> [(BlockItem SprucePlanks, 1)]
  JungleLog        -> [(BlockItem JungleLog, 1)]
  JungleLeaves     -> [(BlockItem OakSapling, 1)]  -- drops sapling
  JunglePlanks     -> [(BlockItem JunglePlanks, 1)]
  TallGrass        -> []  -- no drops
  Dandelion        -> [(BlockItem Dandelion, 1)]
  Rose             -> [(BlockItem Rose, 1)]
  BrownMushroom    -> [(BlockItem BrownMushroom, 1)]
  RedMushroom      -> [(BlockItem RedMushroom, 1)]
  Bookshelf        -> [(BlockItem OakPlanks, 3)]
  Anvil            -> [(BlockItem Anvil, 1)]
  BrewingStand     -> [(BlockItem BrewingStand, 1)]
  Ice              -> []  -- melts, drops nothing
  PackedIce        -> [(BlockItem PackedIce, 1)]
  RedstoneLamp     -> [(BlockItem RedstoneLamp, 1)]
  Hopper           -> [(BlockItem Hopper, 1)]
  WheatCrop1       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop2       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop3       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop4       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop5       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop6       -> [(MaterialItem WheatSeeds, 1)]
  WheatCrop7       -> [(MaterialItem Wheat, 1), (MaterialItem WheatSeeds, 1)]
  PistonNorth      -> [(BlockItem Piston, 1)]
  PistonSouth      -> [(BlockItem Piston, 1)]
  PistonEast       -> [(BlockItem Piston, 1)]
  PistonWest       -> [(BlockItem Piston, 1)]
  PistonDown       -> [(BlockItem Piston, 1)]
  PistonHeadNorth  -> []
  PistonHeadSouth  -> []
  PistonHeadEast   -> []
  PistonHeadWest   -> []
  PistonHeadDown   -> []

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
  StoneStairs -> 1
  RedstoneOre -> 3  -- iron pickaxe or better
  LapisOre    -> 2  -- stone pickaxe or better
  EmeraldOre  -> 3  -- iron pickaxe or better
  Anvil       -> 1  -- wood pickaxe or better
  Hopper      -> 1  -- wood pickaxe or better
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
  BirchLog    -> Just Axe
  BirchPlanks -> Just Axe
  SpruceLog   -> Just Axe
  SprucePlanks -> Just Axe
  JungleLog   -> Just Axe
  JunglePlanks -> Just Axe
  CraftingTable -> Just Axe
  Chest       -> Just Axe
  OakDoorClosed -> Just Axe
  OakDoorOpen -> Just Axe
  IronDoorClosed -> Just Pickaxe
  IronDoorOpen -> Just Pickaxe
  OakFence    -> Just Axe
  FenceGateClosed -> Just Axe
  FenceGateOpen   -> Just Axe
  Bed         -> Just Axe
  TrapdoorClosed -> Just Axe
  TrapdoorOpen   -> Just Axe
  StoneStairs -> Just Pickaxe
  OakStairs   -> Just Axe
  StoneSlab   -> Just Pickaxe
  OakSlab     -> Just Axe
  Piston      -> Just Pickaxe
  Rail        -> Just Pickaxe
  Dispenser   -> Just Pickaxe
  EnchantingTable -> Just Pickaxe
  Netherrack       -> Just Pickaxe
  NetherBrick      -> Just Pickaxe
  RedstoneOre      -> Just Pickaxe
  LapisOre         -> Just Pickaxe
  EmeraldOre       -> Just Pickaxe
  MossyCobblestone -> Just Pickaxe
  MossyStoneBrick  -> Just Pickaxe
  SoulSand         -> Just Shovel
  Glowstone        -> Just Pickaxe
  Bookshelf        -> Just Axe
  Anvil            -> Just Pickaxe
  BrewingStand     -> Just Pickaxe
  Ice              -> Just Pickaxe
  PackedIce        -> Just Pickaxe
  RedstoneLamp     -> Just Pickaxe
  Hopper           -> Just Pickaxe
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
  RawFish        -> 2
  CookedFish     -> 5
  RawSalmon      -> 2
  CookedSalmon   -> 5

-- | How much saturation a food type restores
foodSaturation :: FoodType -> Float
foodSaturation = \case
  RawPorkchop    -> 1.8
  CookedPorkchop -> 12.8
  RawBeef        -> 1.8
  Steak          -> 12.8
  RawChicken     -> 1.2
  CookedChicken  -> 7.2
  Bread          -> 6.0
  Apple          -> 2.4
  RottenFlesh    -> 0.8
  RawFish        -> 0.4
  CookedFish     -> 6.0
  RawSalmon      -> 0.4
  CookedSalmon   -> 9.6

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
  RawFish        -> "Raw Fish"
  CookedFish     -> "Cooked Fish"
  RawSalmon      -> "Raw Salmon"
  CookedSalmon   -> "Cooked Salmon"

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
  Flint      -> "Flint"
  Paper      -> "Paper"
  LapisGem       -> "Lapis Lazuli"
  Emerald        -> "Emerald"
  RedstoneDustMat -> "Redstone Dust"

-- | Defense points for armor by slot and material
armorDefensePoints :: ArmorSlot -> ArmorMaterial -> Int
armorDefensePoints slot mat = case (slot, mat) of
  (Helmet, LeatherArmor)    -> 1; (Helmet, IronArmor)    -> 2; (Helmet, GoldArmor)    -> 2; (Helmet, DiamondArmor)    -> 3
  (Chestplate, LeatherArmor) -> 3; (Chestplate, IronArmor) -> 6; (Chestplate, GoldArmor) -> 5; (Chestplate, DiamondArmor) -> 8
  (Leggings, LeatherArmor)  -> 2; (Leggings, IronArmor)  -> 5; (Leggings, GoldArmor)  -> 3; (Leggings, DiamondArmor)  -> 6
  (Boots, LeatherArmor)     -> 1; (Boots, IronArmor)     -> 2; (Boots, GoldArmor)     -> 1; (Boots, DiamondArmor)     -> 3

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
  "Sheep"    -> pure [(BlockItem Wool, 1)]
  "Zombie"   -> randomDrops [(FoodItem RottenFlesh, 0, 2)]
  "Skeleton" -> do bones  <- randomDrops [(MaterialItem Bone, 0, 2)]
                   arrows <- randomDrops [(MaterialItem ArrowMat, 0, 2)]
                   pure (bones ++ arrows)
  "Creeper"  -> randomDrops [(MaterialItem Gunpowder, 0, 2)]
  "Spider"   -> randomDrops [(MaterialItem StringMat, 0, 2)]
  _ | tag `elem` ["Wolf", "TamedWolf", "TamedWolfSitting"]
              -> randomDrops [(MaterialItem Bone, 0, 2)]
    | tag == "Villager" -> pure []
    | otherwise -> pure []
 where
  randomDrops :: [(Item, Int, Int)] -> IO [(Item, Int)]
  randomDrops entries = do
    results <- mapM (\(item, lo, hi) -> do
      n <- randomRIO (lo, hi)
      pure (item, n)) entries
    pure $ filter (\(_, n) -> n > 0) results

-- | Display name for a potion type
potionName :: PotionType -> String
potionName = \case
  WaterBottle   -> "Water Bottle"
  AwkwardPotion -> "Awkward Potion"
  PoisonPotion  -> "Poison Potion"
  HealingPotion -> "Healing Potion"
  SpeedPotion   -> "Speed Potion"

-- | Display color for a potion type (RGBA)
potionColor :: PotionType -> (Float, Float, Float, Float)
potionColor = \case
  WaterBottle   -> (0.2,  0.4,  0.9, 0.8)   -- blue
  AwkwardPotion -> (0.5,  0.3,  0.6, 0.8)   -- purple
  PoisonPotion  -> (0.3,  0.7,  0.2, 0.8)   -- green
  HealingPotion -> (0.9,  0.2,  0.2, 0.8)   -- red
  SpeedPotion   -> (0.3,  0.8,  0.9, 0.8)   -- cyan

-- ---------------------------------------------------------------------------
-- Binary instances
-- ---------------------------------------------------------------------------

instance Binary BlockType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary ToolType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary ToolMaterial where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary FoodType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary MaterialType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary ArmorSlot where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary ArmorMaterial where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary PotionType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary BucketType where
  put = put . fromEnum
  get = toEnum <$> get

instance Binary Item where
  put (BlockItem bt) = put (0 :: Word8) >> put bt
  put (ToolItem tt tm dur) = put (1 :: Word8) >> put tt >> put tm >> put dur
  put StickItem = put (2 :: Word8)
  put (FoodItem ft) = put (3 :: Word8) >> put ft
  put (MaterialItem mt) = put (4 :: Word8) >> put mt
  put (ArmorItem slot mat dur) = put (5 :: Word8) >> put slot >> put mat >> put dur
  put (ShearsItem dur) = put (6 :: Word8) >> put dur
  put (FlintAndSteelItem dur) = put (7 :: Word8) >> put dur
  put CompassItem = put (8 :: Word8)
  put ClockItem = put (9 :: Word8)
  put (FishingRodItem dur) = put (10 :: Word8) >> put dur
  put GlassBottleItem = put (11 :: Word8)
  put (PotionItem pt) = put (12 :: Word8) >> put pt
  put BoatItem = put (13 :: Word8)
  put MinecartItem = put (14 :: Word8)
  put (BucketItem bt) = put (15 :: Word8) >> put bt
  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> BlockItem <$> get
      1 -> ToolItem <$> get <*> get <*> get
      2 -> pure StickItem
      3 -> FoodItem <$> get
      4 -> MaterialItem <$> get
      5 -> ArmorItem <$> get <*> get <*> get
      6 -> ShearsItem <$> get
      7 -> FlintAndSteelItem <$> get
      8 -> pure CompassItem
      9 -> pure ClockItem
      10 -> FishingRodItem <$> get
      11 -> pure GlassBottleItem
      12 -> PotionItem <$> get
      13 -> pure BoatItem
      14 -> pure MinecartItem
      15 -> BucketItem <$> get
      _ -> fail "Unknown Item tag"

-- | Look up an item by its lowercase name string.
-- Supports block items (by block type name) and common non-block items.
lookupItemByName :: String -> Maybe Item
lookupItemByName name = case map toLower name of
  -- Common block items
  "stone"           -> Just (BlockItem Stone)
  "dirt"            -> Just (BlockItem Dirt)
  "grass"           -> Just (BlockItem Grass)
  "sand"            -> Just (BlockItem Sand)
  "gravel"          -> Just (BlockItem Gravel)
  "cobblestone"     -> Just (BlockItem Cobblestone)
  "oak_planks"      -> Just (BlockItem OakPlanks)
  "oakplanks"       -> Just (BlockItem OakPlanks)
  "planks"          -> Just (BlockItem OakPlanks)
  "glass"           -> Just (BlockItem Glass)
  "oak_log"         -> Just (BlockItem OakLog)
  "oaklog"          -> Just (BlockItem OakLog)
  "log"             -> Just (BlockItem OakLog)
  "iron_ore"        -> Just (BlockItem IronOre)
  "ironore"         -> Just (BlockItem IronOre)
  "coal_ore"        -> Just (BlockItem CoalOre)
  "coalore"         -> Just (BlockItem CoalOre)
  "gold_ore"        -> Just (BlockItem GoldOre)
  "goldore"         -> Just (BlockItem GoldOre)
  "diamond_ore"     -> Just (BlockItem DiamondOre)
  "diamondore"      -> Just (BlockItem DiamondOre)
  "obsidian"        -> Just (BlockItem Obsidian)
  "tnt"             -> Just (BlockItem TNT)
  "torch"           -> Just (BlockItem Torch)
  "wool"            -> Just (BlockItem Wool)
  "brick"           -> Just (BlockItem Brick)
  "stonebrick"      -> Just (BlockItem StoneBrick)
  "stone_brick"     -> Just (BlockItem StoneBrick)
  "crafting_table"  -> Just (BlockItem CraftingTable)
  "craftingtable"   -> Just (BlockItem CraftingTable)
  "furnace"         -> Just (BlockItem Furnace)
  "chest"           -> Just (BlockItem Chest)
  "ladder"          -> Just (BlockItem Ladder)
  "bed"             -> Just (BlockItem Bed)
  "snow"            -> Just (BlockItem Snow)
  "clay"            -> Just (BlockItem Clay)
  "bedrock"         -> Just (BlockItem Bedrock)
  "water"           -> Just (BlockItem Water)
  "lava"            -> Just (BlockItem Lava)
  -- Tools (wood, default durability)
  "wooden_pickaxe"  -> Just (ToolItem Pickaxe Wood 59)
  "wooden_sword"    -> Just (ToolItem Sword Wood 59)
  "wooden_axe"      -> Just (ToolItem Axe Wood 59)
  "wooden_shovel"   -> Just (ToolItem Shovel Wood 59)
  "stone_pickaxe"   -> Just (ToolItem Pickaxe StoneTier 131)
  "stone_sword"     -> Just (ToolItem Sword StoneTier 131)
  "iron_pickaxe"    -> Just (ToolItem Pickaxe Iron 250)
  "iron_sword"      -> Just (ToolItem Sword Iron 250)
  "diamond_pickaxe" -> Just (ToolItem Pickaxe Diamond 1561)
  "diamond_sword"   -> Just (ToolItem Sword Diamond 1561)
  -- Simple items
  "stick"           -> Just StickItem
  "compass"         -> Just CompassItem
  "clock"           -> Just ClockItem
  "boat"            -> Just BoatItem
  "minecart"        -> Just MinecartItem
  "glass_bottle"    -> Just GlassBottleItem
  -- Food
  "apple"           -> Just (FoodItem Apple)
  "bread"           -> Just (FoodItem Bread)
  "raw_beef"        -> Just (FoodItem RawBeef)
  "cooked_beef"     -> Just (FoodItem Steak)
  "steak"           -> Just (FoodItem Steak)
  "raw_pork"        -> Just (FoodItem RawPorkchop)
  "cooked_pork"     -> Just (FoodItem CookedPorkchop)
  -- Materials
  "coal"            -> Just (MaterialItem Coal)
  "iron_ingot"      -> Just (MaterialItem IronIngot)
  "ironingot"       -> Just (MaterialItem IronIngot)
  "gold_ingot"      -> Just (MaterialItem GoldIngot)
  "goldingot"       -> Just (MaterialItem GoldIngot)
  "diamond"         -> Just (MaterialItem DiamondGem)
  "bone"            -> Just (MaterialItem Bone)
  _                 -> Nothing
