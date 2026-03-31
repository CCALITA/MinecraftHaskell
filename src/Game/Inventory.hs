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
  , canStack
  , stackLimit
  ) where

import World.Block (BlockType(..))
import qualified Data.Vector as V

-- | Maximum stack size for most items
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
  { isBlockType :: !BlockType
  , isCount     :: !Int
  } deriving stock (Show, Eq)

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

-- | Check if two item types can stack together
canStack :: BlockType -> BlockType -> Bool
canStack a b = a == b

-- | Add items to inventory, returns updated inventory and leftover count
addItem :: Inventory -> BlockType -> Int -> (Inventory, Int)
addItem inv bt count =
  let -- First try to merge into existing stacks
      (inv1, remaining1) = mergeIntoExisting inv bt count
      -- Then try to place in empty slots
      (inv2, remaining2) = placeInEmpty inv1 bt remaining1
  in (inv2, remaining2)

-- | Try to merge items into existing stacks of the same type
mergeIntoExisting :: Inventory -> BlockType -> Int -> (Inventory, Int)
mergeIntoExisting inv _ 0 = (inv, 0)
mergeIntoExisting inv bt remaining = go inv 0 remaining
  where
    go inv' idx rem'
      | idx >= inventorySlots = (inv', rem')
      | rem' <= 0 = (inv', 0)
      | otherwise = case getSlot inv' idx of
          Just (ItemStack sbt cnt) | sbt == bt && cnt < stackLimit ->
            let space = stackLimit - cnt
                toAdd = min space rem'
                inv'' = setSlot inv' idx (Just (ItemStack bt (cnt + toAdd)))
            in go inv'' (idx + 1) (rem' - toAdd)
          _ -> go inv' (idx + 1) rem'

-- | Place items in first available empty slot
placeInEmpty :: Inventory -> BlockType -> Int -> (Inventory, Int)
placeInEmpty inv _ 0 = (inv, 0)
placeInEmpty inv bt remaining = go inv 0 remaining
  where
    go inv' idx rem'
      | idx >= inventorySlots = (inv', rem')
      | rem' <= 0 = (inv', 0)
      | otherwise = case getSlot inv' idx of
          Nothing ->
            let toPlace = min stackLimit rem'
                inv'' = setSlot inv' idx (Just (ItemStack bt toPlace))
            in go inv'' (idx + 1) (rem' - toPlace)
          _ -> go inv' (idx + 1) rem'

-- | Remove count items of a type from inventory, returns updated inventory and actual removed count
removeItem :: Inventory -> BlockType -> Int -> (Inventory, Int)
removeItem inv bt count = go inv (inventorySlots - 1) count 0
  where
    -- Remove from end first (main inventory before hotbar)
    go inv' idx remaining removed
      | idx < 0 = (inv', removed)
      | remaining <= 0 = (inv', removed)
      | otherwise = case getSlot inv' idx of
          Just (ItemStack sbt cnt) | sbt == bt ->
            let toRemove = min cnt remaining
                newCnt = cnt - toRemove
                newSlot = if newCnt <= 0 then Nothing else Just (ItemStack bt newCnt)
                inv'' = setSlot inv' idx newSlot
            in go inv'' (idx - 1) (remaining - toRemove) (removed + toRemove)
          _ -> go inv' (idx - 1) remaining removed
