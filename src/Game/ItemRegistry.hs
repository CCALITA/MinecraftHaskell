module Game.ItemRegistry
  ( ItemDef(..)
  , ItemRegistry
  , newItemRegistry
  , registerItem
  , lookupItem
  , registeredItems
  , mkItemDef
  , defaultItemRegistry
  ) where

import Game.Item
  ( Item(..), ToolMaterial(..)
  , ArmorSlot(..), ArmorMaterial(..)
  , itemStackLimit, itemToBlock
  , toolInfo, ToolInfo(..)
  )
import Game.ItemDisplay (itemColor, itemMiniIcon)
import World.Block (BlockType)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Word (Word8)

-- | Complete definition for an item type, consolidating display and
-- gameplay properties into a single record.
data ItemDef = ItemDef
  { idStackLimit :: !Int
  , idColor      :: !(Float, Float, Float, Float)
  , idMiniIcon   :: ![(Int, Int, (Float, Float, Float, Float))]
  , idToBlock    :: !(Maybe BlockType)
  , idBinaryTag  :: !Word8
  } deriving stock (Show, Eq)

-- | Mutable registry mapping Word8 item keys to their definitions.
type ItemRegistry = IORef (HM.HashMap Word8 ItemDef)

-- | Create an empty item registry.
newItemRegistry :: IO ItemRegistry
newItemRegistry = newIORef HM.empty

-- | Register an item definition for a given key.
-- Overwrites any previous definition for the same key.
registerItem :: ItemRegistry -> Word8 -> ItemDef -> IO ()
registerItem reg key def =
  modifyIORef' reg (HM.insert key def)

-- | Look up the definition for an item by key.
lookupItem :: ItemRegistry -> Word8 -> IO (Maybe ItemDef)
lookupItem reg key =
  HM.lookup key <$> readIORef reg

-- | List all registered (itemKey, definition) pairs.
registeredItems :: ItemRegistry -> IO [(Word8, ItemDef)]
registeredItems reg = HM.toList <$> readIORef reg

-- | Build an ItemDef from an Item using existing pattern-match
-- functions in Item.hs and ItemDisplay.hs.
mkItemDef :: Word8 -> Item -> ItemDef
mkItemDef tag item = ItemDef
  { idStackLimit = itemStackLimit item
  , idColor      = itemColor item
  , idMiniIcon   = itemMiniIcon item
  , idToBlock    = itemToBlock item
  , idBinaryTag  = tag
  }

-- | All representative items with their binary tags and registry keys.
-- Each entry is (registryKey, binaryTag, representativeItem).
allRepresentativeItems :: [(Word8, Word8, Item)]
allRepresentativeItems = concat
  [ blockItems
  , toolItems
  , [(20, 2, StickItem)]
  , foodItems
  , materialItems
  , armorItems
  , [ (80, 6, ShearsItem 238)
    , (81, 7, FlintAndSteelItem 64)
    , (82, 8, CompassItem)
    , (83, 9, ClockItem)
    , (84, 10, FishingRodItem 64)
    , (85, 11, GlassBottleItem)
    ]
  , potionItems
  , [ (100, 13, BoatItem)
    , (101, 14, MinecartItem)
    ]
  ]
  where
    blockItems =
      [ (bt, 0, BlockItem b)
      | (bt, b) <- zip [0..] [minBound .. maxBound]
      ]
    toolItems =
      [ (fromIntegral (15 + fromEnum tt * 4 + fromEnum tm), 1
        , ToolItem tt tm (tiMaxDurability (toolInfo tm)))
      | tt <- [minBound .. maxBound]
      , tm <- [minBound .. maxBound]
      ]
    foodItems =
      [ (fromIntegral (40 + fromEnum ft), 3, FoodItem ft)
      | ft <- [minBound .. maxBound]
      ]
    materialItems =
      [ (fromIntegral (60 + fromEnum mt), 4, MaterialItem mt)
      | mt <- [minBound .. maxBound]
      ]
    armorItems =
      [ (fromIntegral (90 + fromEnum sl * 4 + fromEnum am), 5
        , ArmorItem sl am (armorDur sl am))
      | sl <- [minBound .. maxBound]
      , am <- [minBound .. maxBound]
      ]
    armorDur Helmet     LeatherArmor = 55;  armorDur Helmet     IronArmor = 165
    armorDur Helmet     GoldArmor    = 77;  armorDur Helmet     DiamondArmor = 363
    armorDur Chestplate LeatherArmor = 80;  armorDur Chestplate IronArmor = 240
    armorDur Chestplate GoldArmor    = 112; armorDur Chestplate DiamondArmor = 528
    armorDur Leggings   LeatherArmor = 75;  armorDur Leggings   IronArmor = 225
    armorDur Leggings   GoldArmor    = 105; armorDur Leggings   DiamondArmor = 495
    armorDur Boots      LeatherArmor = 65;  armorDur Boots      IronArmor = 195
    armorDur Boots      GoldArmor    = 91;  armorDur Boots      DiamondArmor = 429
    potionItems =
      [ (fromIntegral (110 + fromEnum pt), 12, PotionItem pt)
      | pt <- [minBound .. maxBound]
      ]

-- | Pre-populate a registry with all representative item types.
-- Builds the entire map in one pass rather than inserting one-by-one.
defaultItemRegistry :: IO ItemRegistry
defaultItemRegistry =
  newIORef $ HM.fromList
    [ (key, mkItemDef tag item) | (key, tag, item) <- allRepresentativeItems ]
