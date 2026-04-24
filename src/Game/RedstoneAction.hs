{-# LANGUAGE OverloadedStrings #-}

-- | Redstone-driven world actions: iron doors, pistons, and dispensers.
--
-- Extracted from "Main" for modularity. These operations consult the
-- 'RedstoneState' power map and mutate the 'World' and associated caches
-- accordingly. Chunk remeshing is delegated via a callback supplied by the
-- caller to avoid a dependency on the main loop's rendering helpers.
module Game.RedstoneAction
  ( RebuildChunkAt
  , neighborDirs
  , updateIronDoors
  , updatePistons
  , isImmovable
  , tryPistonPush
  , dispenseFromDispensers
  ) where

import Control.Monad (forM_, when)
import Data.IORef (IORef)
import qualified Data.HashMap.Strict as HM
import Linear (V3(..))
import qualified Vulkan as Vk
import System.Random (randomRIO)

import Engine.Vulkan.Memory (BufferAllocation)
import Game.BlockEntity
  ( BlockEntityMap
  , dispenserSlots
  , getDispenserInventory
  , getDispenserSlot
  , setDispenserInventory
  , setDispenserSlot
  )
import Game.DroppedItem (DroppedItems, spawnDrop)
import Game.Inventory (ItemStack(..))
import World.Block
  ( BlockType(..)
  , isPistonBlock
  , isPistonHeadBlock
  , pistonDirection
  , pistonHeadForPiston
  )
import World.Chunk (ChunkPos)
import World.Redstone (RedstoneState, getPower)
import World.World (World, worldGetBlock, worldSetBlock)

-- | Callback that rebuilds the chunk mesh containing world coordinates
--   (wx, wz). Supplied by the caller because meshing lives in the main loop.
type RebuildChunkAt
  =  Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.CommandPool
  -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> IO ()

-- | 6-connected neighbor offsets (shared by redstone-driven updates)
neighborDirs :: [V3 Int]
neighborDirs = [ V3 1 0 0, V3 (-1) 0 0
               , V3 0 1 0, V3 0 (-1) 0
               , V3 0 0 1, V3 0 0 (-1) ]

-- | Update iron doors adjacent to a redstone source position.
--   If powered > 0, open the door; if power drops to 0, close it.
updateIronDoors
  :: RebuildChunkAt
  -> World -> RedstoneState -> V3 Int
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
updateIronDoors rebuildChunkAt world rsState sourcePos physDevice device cmdPool queue meshCacheRef = do
  forM_ [ sourcePos + d | d <- neighborDirs ] $ \nPos@(V3 nx _ny nz) -> do
    blk <- worldGetBlock world nPos
    power <- getPower rsState nPos
    case blk of
      IronDoorClosed | power > 0 -> do
        worldSetBlock world nPos IronDoorOpen
        rebuildChunkAt physDevice device cmdPool queue meshCacheRef nx nz
      IronDoorOpen | power == 0 -> do
        worldSetBlock world nPos IronDoorClosed
        rebuildChunkAt physDevice device cmdPool queue meshCacheRef nx nz
      _ -> pure ()

-- | Update pistons adjacent to a redstone source position.
--   Each piston variant pushes in its facing direction. Cannot push Bedrock or Obsidian. Max 12 blocks.
updatePistons
  :: RebuildChunkAt
  -> World -> RedstoneState -> V3 Int
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
updatePistons rebuildChunkAt world rsState sourcePos physDevice device cmdPool queue meshCacheRef = do
  forM_ [ sourcePos + d | d <- neighborDirs ] $ \nPos@(V3 nx _ny nz) -> do
    blk <- worldGetBlock world nPos
    when (isPistonBlock blk) $ do
      power <- getPower rsState nPos
      let dir = pistonDirection blk
          headType = pistonHeadForPiston blk
          frontPos = nPos + dir
      frontBlk <- worldGetBlock world frontPos
      if power > 0 && not (isPistonHeadBlock frontBlk)
        then do
          chainLen <- tryPistonPush world nPos dir 12
          when (chainLen >= 0) $ do
            worldSetBlock world frontPos headType
            rebuildChunkAt physDevice device cmdPool queue meshCacheRef nx nz
            forM_ [1..chainLen + 1] $ \step -> do
              let V3 rx _ rz = nPos + fmap (* (step + 1)) dir
              rebuildChunkAt physDevice device cmdPool queue meshCacheRef rx rz
        else when (power == 0 && isPistonHeadBlock frontBlk) $ do
          let beyondHead = frontPos + dir
          beyondBlk <- worldGetBlock world beyondHead
          worldSetBlock world frontPos Air
          when (beyondBlk /= Air && not (isImmovable beyondBlk)) $ do
            worldSetBlock world frontPos beyondBlk
            worldSetBlock world beyondHead Air
          rebuildChunkAt physDevice device cmdPool queue meshCacheRef nx nz
          let V3 rx _ rz = beyondHead
          rebuildChunkAt physDevice device cmdPool queue meshCacheRef rx rz

-- | Check if a block cannot be pushed by a piston
isImmovable :: BlockType -> Bool
isImmovable Bedrock = True
isImmovable Obsidian = True
isImmovable bt = isPistonHeadBlock bt

-- | Push a chain of blocks above the piston in the given direction.
--   Returns the number of blocks moved (>= 0), or -1 on failure.
tryPistonPush :: World -> V3 Int -> V3 Int -> Int -> IO Int
tryPistonPush world pistonPos direction maxBlocks = do
  let startPos = pistonPos + direction
  chain <- collectChain startPos 0
  case chain of
    Nothing -> pure (-1)
    Just blocks -> do
      forM_ (reverse blocks) $ \(pos, bt) -> do
        worldSetBlock world (pos + direction) bt
        worldSetBlock world pos Air
      pure (length blocks)
  where
    collectChain pos count
      | count >= maxBlocks = pure Nothing
      | otherwise = do
          bt <- worldGetBlock world pos
          if bt == Air
            then pure (Just [])
            else if isImmovable bt
              then pure Nothing
              else do
                rest <- collectChain (pos + direction) (count + 1)
                pure $ fmap ((pos, bt) :) rest

-- | Dispense items from dispensers adjacent to a redstone source.
--   When a neighbor is a Dispenser block, pick a random non-empty slot and
--   spawn the item as a dropped entity in front of the dispenser.
dispenseFromDispensers
  :: World -> RedstoneState -> BlockEntityMap -> DroppedItems -> V3 Int
  -> IO ()
dispenseFromDispensers world _rsState blockEntityMapRef droppedItemsRef sourcePos = do
  let neighbors = [ sourcePos + d | d <- neighborDirs ]
  forM_ neighbors $ \nPos -> do
    blk <- worldGetBlock world nPos
    when (blk == Dispenser) $ do
      mDispInv <- getDispenserInventory blockEntityMapRef nPos
      case mDispInv of
        Nothing -> pure ()
        Just dispInv -> do
          -- Find all non-empty slots
          let nonEmpty = [ (i, s) | i <- [0..dispenserSlots - 1]
                                  , Just s <- [getDispenserSlot dispInv i] ]
          case nonEmpty of
            [] -> pure ()
            slots -> do
              -- Pick a random non-empty slot
              idx <- randomRIO (0, length slots - 1)
              let (slotIdx, ItemStack item cnt) = slots !! idx
                  -- Dispense one item: decrement or clear slot
                  newStack = if cnt <= 1 then Nothing else Just (ItemStack item (cnt - 1))
                  newInv = setDispenserSlot dispInv slotIdx newStack
                  -- Spawn in front of the dispenser (away from source)
                  V3 dx dy dz = nPos - sourcePos
                  dropPos = fmap fromIntegral nPos + V3 (fromIntegral dx + 0.5) (fromIntegral dy + 0.5) (fromIntegral dz + 0.5)
              setDispenserInventory blockEntityMapRef nPos newInv
              spawnDrop droppedItemsRef item 1 dropPos
