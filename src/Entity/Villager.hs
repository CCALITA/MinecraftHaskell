module Entity.Villager
  ( VillagerProfession(..)
  , TradeOffer(..)
  , generateTrades
  , professionName
  , allProfessions
  , executeTrade
  ) where

import Game.Item (Item(..), MaterialType(..), FoodType(..), ToolType(..), ToolMaterial(..))
import Game.Inventory (Inventory, hasItem, removeItem, addItem)
import World.Block (BlockType(..))

-- | Villager professions determining available trades
data VillagerProfession = Farmer | Librarian | Blacksmith | Butcher | Cleric | Shepherd
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A single trade offer from a villager
data TradeOffer = TradeOffer
  { toInputItem   :: !Item
  , toInputCount  :: !Int
  , toOutputItem  :: !Item
  , toOutputCount :: !Int
  , toMaxUses     :: !Int
  , toUsesLeft    :: !Int
  } deriving stock (Show, Eq)

-- | Currency placeholder (GoldIngot stands in for Emerald)
currency :: Item
currency = MaterialItem GoldIngot

-- | Generate the trade list for a given profession
generateTrades :: VillagerProfession -> [TradeOffer]
generateTrades Farmer =
  [ TradeOffer (MaterialItem Wheat) 20 currency 1 12 12
  , TradeOffer currency 1 (FoodItem Bread) 4 12 12
  ]
generateTrades Librarian =
  [ TradeOffer (MaterialItem Paper) 24 currency 1 12 12
  , TradeOffer currency 1 (BlockItem Glass) 4 12 12
  ]
generateTrades Blacksmith =
  [ TradeOffer (MaterialItem Coal) 15 currency 1 12 12
  , TradeOffer currency 3 (ToolItem Pickaxe Iron 250) 1 3 3
  , TradeOffer currency 5 (ToolItem Sword Iron 250) 1 3 3
  ]
generateTrades Butcher =
  [ TradeOffer (FoodItem RawPorkchop) 14 currency 1 12 12
  , TradeOffer (FoodItem RawChicken) 14 currency 1 12 12
  , TradeOffer currency 1 (FoodItem CookedPorkchop) 5 12 12
  ]
generateTrades Cleric =
  [ TradeOffer currency 1 (BlockItem Glass) 2 12 12
  , TradeOffer currency 3 (BlockItem RedstoneDust) 4 12 12
  ]
generateTrades Shepherd =
  [ TradeOffer (BlockItem Wool) 18 currency 1 16 16
  , TradeOffer currency 1 (ShearsItem 238) 1 3 3
  ]

-- | Human-readable name for a profession
professionName :: VillagerProfession -> String
professionName = show

-- | All professions as a list (convenience for iteration)
allProfessions :: [VillagerProfession]
allProfessions = [minBound .. maxBound]

executeTrade :: Inventory -> TradeOffer -> Maybe (Inventory, TradeOffer)
executeTrade inv trade
  | toUsesLeft trade <= 0 = Nothing
  | not (hasItem inv (toInputItem trade) (toInputCount trade)) = Nothing
  | otherwise =
      let (inv1, _) = removeItem inv (toInputItem trade) (toInputCount trade)
          (inv2, _) = addItem inv1 (toOutputItem trade) (toOutputCount trade)
          trade2 = trade { toUsesLeft = toUsesLeft trade - 1 }
      in Just (inv2, trade2)
