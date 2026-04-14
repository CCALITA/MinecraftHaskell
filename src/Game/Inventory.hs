module Game.Inventory
  ( Inventory(..)
  , ItemStack(..)
  , emptyInventory
  , hotbarSlots
  , inventorySlots
  , getSlot
  , setSlot
  , getHotbarSlot
  , selectedItem
  , selectHotbar
  , addItem
  , removeItem
  , stackLimit
  ) where

import Game.Item (Item(..), itemStackLimit)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | Default maximum stack size
stackLimit :: Int
stackLimit = 64

-- | Number of hotbar slots
hotbarSlots :: Int
hotbarSlots = 9

-- | Total inventory slots (hotbar + main inventory)
inventorySlots :: Int
inventorySlots = 36

-- | A stack of items in a slot
data ItemStack = ItemStack
  { isItem  :: !Item
  , isCount :: !Int
  } deriving stock (Show, Eq, Generic)

-- | Player inventory: 36 slots (0-8 = hotbar, 9-35 = main inventory)
data Inventory = Inventory
  { invSlots    :: !(V.Vector (Maybe ItemStack))
  , invSelected :: !Int  -- hotbar index 0-8
  } deriving stock (Show, Eq)

-- | Empty inventory
emptyInventory :: Inventory
emptyInventory = Inventory
  { invSlots    = V.replicate inventorySlots Nothing
  , invSelected = 0
  }

-- | Get item in a slot
getSlot :: Inventory -> Int -> Maybe ItemStack
getSlot inv idx
  | idx >= 0 && idx < inventorySlots = invSlots inv V.! idx
  | otherwise = Nothing

-- | Set item in a slot
setSlot :: Inventory -> Int -> Maybe ItemStack -> Inventory
setSlot inv idx stack
  | idx >= 0 && idx < inventorySlots = inv { invSlots = invSlots inv V.// [(idx, stack)] }
  | otherwise = inv

-- | Get item in hotbar slot (0-8)
getHotbarSlot :: Inventory -> Int -> Maybe ItemStack
getHotbarSlot inv idx = getSlot inv idx

-- | Get currently selected hotbar item
selectedItem :: Inventory -> Maybe ItemStack
selectedItem inv = getHotbarSlot inv (invSelected inv)

-- | Select a hotbar slot (0-8)
selectHotbar :: Inventory -> Int -> Inventory
selectHotbar inv idx
  | idx >= 0 && idx < hotbarSlots = inv { invSelected = idx }
  | otherwise = inv

-- | Add items to inventory, returns updated inventory and leftover count
addItem :: Inventory -> Item -> Int -> (Inventory, Int)
addItem inv item count =
  let (inv1, remaining1) = mergeIntoExisting inv item count
      (inv2, remaining2) = placeInEmpty inv1 item remaining1
  in (inv2, remaining2)

-- | Try to merge items into existing stacks of the same type
mergeIntoExisting :: Inventory -> Item -> Int -> (Inventory, Int)
mergeIntoExisting inv _ 0 = (inv, 0)
mergeIntoExisting inv item remaining = go inv 0 remaining
  where
    maxStack = itemStackLimit item
    go inv' idx rem'
      | idx >= inventorySlots = (inv', rem')
      | rem' <= 0 = (inv', 0)
      | otherwise = case getSlot inv' idx of
          Just (ItemStack sItem cnt) | sItem == item && cnt < maxStack ->
            let space = maxStack - cnt
                toAdd = min space rem'
                inv'' = setSlot inv' idx (Just (ItemStack item (cnt + toAdd)))
            in go inv'' (idx + 1) (rem' - toAdd)
          _ -> go inv' (idx + 1) rem'

-- | Place items in first available empty slot
placeInEmpty :: Inventory -> Item -> Int -> (Inventory, Int)
placeInEmpty inv _ 0 = (inv, 0)
placeInEmpty inv item remaining = go inv 0 remaining
  where
    maxStack = itemStackLimit item
    go inv' idx rem'
      | idx >= inventorySlots = (inv', rem')
      | rem' <= 0 = (inv', 0)
      | otherwise = case getSlot inv' idx of
          Nothing ->
            let toPlace = min maxStack rem'
                inv'' = setSlot inv' idx (Just (ItemStack item toPlace))
            in go inv'' (idx + 1) (rem' - toPlace)
          _ -> go inv' (idx + 1) rem'

-- | Remove count items of a type from inventory, returns updated inventory and actual removed count
removeItem :: Inventory -> Item -> Int -> (Inventory, Int)
removeItem inv item count = go inv (inventorySlots - 1) count 0
  where
    go inv' idx remaining removed
      | idx < 0 = (inv', removed)
      | remaining <= 0 = (inv', removed)
      | otherwise = case getSlot inv' idx of
          Just (ItemStack sItem cnt) | sItem == item ->
            let toRemove = min cnt remaining
                newCnt = cnt - toRemove
                newSlot = if newCnt <= 0 then Nothing else Just (ItemStack item newCnt)
                inv'' = setSlot inv' idx newSlot
            in go inv'' (idx - 1) (remaining - toRemove) (removed + toRemove)
          _ -> go inv' (idx - 1) remaining removed
