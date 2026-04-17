module Game.BlockEntity
  ( BlockEntityMap
  , BlockEntityData(..)
  , newBlockEntityMap
  , getChestInventory
  , setChestInventory
  , getFurnaceState
  , setFurnaceState
  , allFurnaceEntities
  , removeBlockEntity
  , hasBlockEntity
  , chestSlots
  , emptyChestInventory
  , getChestSlot
  , setChestSlot
  ) where

import Data.IORef
import qualified Data.HashMap.Strict as HM
import Linear (V3(..))
import qualified Data.Vector as V

import Game.Inventory (Inventory(..), ItemStack)
import Game.Furnace (FurnaceState)

-- | Number of slots in a chest (3 rows of 9)
chestSlots :: Int
chestSlots = 27

-- | An empty chest inventory (27 slots, selected = 0)
emptyChestInventory :: Inventory
emptyChestInventory = Inventory
  { invSlots    = V.replicate chestSlots Nothing
  , invSelected = 0
  }

-- | Data stored in a block entity
data BlockEntityData
  = ChestData !Inventory
  | FurnaceData !FurnaceState
  deriving stock (Show)

-- | Map from world position to block entity data
type BlockEntityMap = IORef (HM.HashMap (V3 Int) BlockEntityData)

-- | Create an empty block entity map
newBlockEntityMap :: IO BlockEntityMap
newBlockEntityMap = newIORef HM.empty

-- | Get the chest inventory at a position, if it exists
getChestInventory :: BlockEntityMap -> V3 Int -> IO (Maybe Inventory)
getChestInventory ref pos = do
  m <- readIORef ref
  pure $ case HM.lookup pos m of
    Just (ChestData inv) -> Just inv
    _                    -> Nothing

-- | Set or update the chest inventory at a position
setChestInventory :: BlockEntityMap -> V3 Int -> Inventory -> IO ()
setChestInventory ref pos inv = modifyIORef' ref (HM.insert pos (ChestData inv))

-- | Get the furnace state at a position, if it exists
getFurnaceState :: BlockEntityMap -> V3 Int -> IO (Maybe FurnaceState)
getFurnaceState ref pos = do
  m <- readIORef ref
  pure $ case HM.lookup pos m of
    Just (FurnaceData fs) -> Just fs
    _                     -> Nothing

-- | Set or update the furnace state at a position
setFurnaceState :: BlockEntityMap -> V3 Int -> FurnaceState -> IO ()
setFurnaceState ref pos fs = modifyIORef' ref (HM.insert pos (FurnaceData fs))

-- | Return all furnace entities as a list of (position, FurnaceState)
allFurnaceEntities :: BlockEntityMap -> IO [(V3 Int, FurnaceState)]
allFurnaceEntities ref = do
  m <- readIORef ref
  pure [ (p, fs) | (p, FurnaceData fs) <- HM.toList m ]

-- | Remove a block entity at a position, returning its data if present
removeBlockEntity :: BlockEntityMap -> V3 Int -> IO (Maybe BlockEntityData)
removeBlockEntity ref pos = do
  m <- readIORef ref
  let mbe = HM.lookup pos m
  writeIORef ref (HM.delete pos m)
  pure mbe

-- | Check whether a block entity exists at a position
hasBlockEntity :: BlockEntityMap -> V3 Int -> IO Bool
hasBlockEntity ref pos = HM.member pos <$> readIORef ref

-- | Get item in a chest slot (0-26)
getChestSlot :: Inventory -> Int -> Maybe ItemStack
getChestSlot inv idx
  | idx >= 0 && idx < chestSlots = invSlots inv V.! idx
  | otherwise = Nothing

-- | Set item in a chest slot (0-26), returning a new Inventory
setChestSlot :: Inventory -> Int -> Maybe ItemStack -> Inventory
setChestSlot inv idx stack
  | idx >= 0 && idx < chestSlots = inv { invSlots = invSlots inv V.// [(idx, stack)] }
  | otherwise = inv
