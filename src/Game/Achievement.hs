module Game.Achievement
  ( AchievementType(..)
  , AchievementState
  , AchievementTrigger(..)
  , DimensionType(..)
  , newAchievementState
  , checkAchievement
  , unlockAchievement
  , isUnlocked
  , achievementName
  , achievementDescription
  , allAchievements
  ) where

import qualified Data.Set as Set

import World.Block (BlockType(..))
import Game.Item (Item(..), ToolType(..), FoodType(..), MaterialType(..))

-- | All achievement types in the game
data AchievementType
  = AchOpenInventory
  | AchMineWood
  | AchCraftPlanks
  | AchCraftTable
  | AchCraftPickaxe
  | AchSmeltIron
  | AchBakeBread
  | AchMakeSword
  | AchKillMob
  | AchMineDiamond
  | AchEnterNether
  | AchBrewPotion
  | AchEnchantItem
  | AchDefeatDragon
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Dimension types for dimension-related triggers
data DimensionType
  = Overworld
  | Nether
  | TheEnd
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Events that can trigger achievement unlocks
data AchievementTrigger
  = TrigOpenInventory
  | TrigMineBlock BlockType
  | TrigCraftItem Item
  | TrigSmeltItem Item
  | TrigKillEntity String
  | TrigEnterDimension DimensionType
  deriving stock (Show, Eq)

-- | Set of unlocked achievements (immutable -- callers replace via unlockAchievement)
type AchievementState = Set.Set AchievementType

-- | Empty achievement state with nothing unlocked
newAchievementState :: AchievementState
newAchievementState = Set.empty

-- | Check whether a trigger should unlock a not-yet-unlocked achievement.
--   Returns 'Just' the achievement if it qualifies and is still locked,
--   'Nothing' otherwise.
checkAchievement :: AchievementState -> AchievementTrigger -> Maybe AchievementType
checkAchievement state trigger =
  case triggerToAchievement trigger of
    Just ach | not (Set.member ach state) -> Just ach
    _ -> Nothing

-- | Unlock an achievement, returning a new state (immutable update).
unlockAchievement :: AchievementState -> AchievementType -> AchievementState
unlockAchievement = flip Set.insert

-- | Query whether an achievement is already unlocked.
isUnlocked :: AchievementState -> AchievementType -> Bool
isUnlocked = flip Set.member

-- | Human-readable name for each achievement.
achievementName :: AchievementType -> String
achievementName = \case
  AchOpenInventory -> "Taking Inventory"
  AchMineWood      -> "Getting Wood"
  AchCraftPlanks   -> "Benchmarking"
  AchCraftTable    -> "Crafting Table"
  AchCraftPickaxe  -> "Time to Mine!"
  AchSmeltIron     -> "Acquire Hardware"
  AchBakeBread     -> "Bake Bread"
  AchMakeSword     -> "Time to Strike!"
  AchKillMob       -> "Monster Hunter"
  AchMineDiamond   -> "Diamonds!"
  AchEnterNether   -> "We Need to Go Deeper"
  AchBrewPotion    -> "Local Brewery"
  AchEnchantItem   -> "Enchanter"
  AchDefeatDragon  -> "The End."

-- | Short description of what the player must do.
achievementDescription :: AchievementType -> String
achievementDescription = \case
  AchOpenInventory -> "Open the inventory"
  AchMineWood      -> "Mine a wood log"
  AchCraftPlanks   -> "Craft wooden planks"
  AchCraftTable    -> "Craft a crafting table"
  AchCraftPickaxe  -> "Craft a wooden pickaxe"
  AchSmeltIron     -> "Smelt an iron ingot"
  AchBakeBread     -> "Craft bread from wheat"
  AchMakeSword     -> "Craft a sword"
  AchKillMob       -> "Kill a hostile mob"
  AchMineDiamond   -> "Mine a diamond ore block"
  AchEnterNether   -> "Enter the Nether"
  AchBrewPotion    -> "Brew a potion"
  AchEnchantItem   -> "Enchant an item"
  AchDefeatDragon  -> "Defeat the Ender Dragon"

-- | All defined achievement types.
allAchievements :: [AchievementType]
allAchievements = [minBound .. maxBound]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Map a trigger to the achievement it would unlock (if any).
triggerToAchievement :: AchievementTrigger -> Maybe AchievementType
triggerToAchievement = \case
  TrigOpenInventory       -> Just AchOpenInventory
  TrigMineBlock bt        -> mineBlockAchievement bt
  TrigCraftItem item      -> craftItemAchievement item
  TrigSmeltItem item      -> smeltItemAchievement item
  TrigKillEntity tag      -> killEntityAchievement tag
  TrigEnterDimension dim  -> enterDimensionAchievement dim

mineBlockAchievement :: BlockType -> Maybe AchievementType
mineBlockAchievement = \case
  OakLog     -> Just AchMineWood
  DiamondOre -> Just AchMineDiamond
  _          -> Nothing

craftItemAchievement :: Item -> Maybe AchievementType
craftItemAchievement = \case
  BlockItem OakPlanks     -> Just AchCraftPlanks
  BlockItem CraftingTable -> Just AchCraftTable
  ToolItem Pickaxe _ _    -> Just AchCraftPickaxe
  ToolItem Sword _ _      -> Just AchMakeSword
  FoodItem Bread          -> Just AchBakeBread
  _                       -> Nothing

smeltItemAchievement :: Item -> Maybe AchievementType
smeltItemAchievement = \case
  MaterialItem IronIngot -> Just AchSmeltIron
  _                      -> Nothing

killEntityAchievement :: String -> Maybe AchievementType
killEntityAchievement tag
  | tag `elem` ["Zombie", "Skeleton", "Creeper", "Spider"] = Just AchKillMob
  | otherwise = Nothing

enterDimensionAchievement :: DimensionType -> Maybe AchievementType
enterDimensionAchievement = \case
  Nether -> Just AchEnterNether
  _      -> Nothing
