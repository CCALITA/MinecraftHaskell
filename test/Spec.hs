module Main (main) where

import Test.Hspec
import Test.QuickCheck

import World.Block
import World.Chunk
import World.Noise
import World.World (worldToChunkLocal, World(..), newWorld, worldGetBlock, worldSetBlock, triggerGravityAbove, settleGravityBlock, settleChunkGravity)
import World.Generation (defaultGenConfig)
import Game.Inventory
import Game.Item
import Game.Crafting
import Game.Player
import Game.DroppedItem
import Game.Save (SaveData(..), inventoryToSlotList, slotListToInventory)
import Game.BlockEntity
import Game.Furnace
import World.Weather
import Entity.ECS
import World.Redstone

import Game.Physics (BlockHeightQuery)

import Data.Binary (encode, decode)
import Linear (V2(..), V3(..))
import qualified Data.Vector as V
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = hspec $ do
  blockSpec
  chunkSpec
  noiseSpec
  inventorySpec
  craftingSpec
  worldCoordSpec
  itemSpec
  droppedItemSpec
  playerInputSpec
  newBlockSpec
  newItemSpec
  gravitySpec
  saveSpec
  blockEntitySpec
  furnaceSpec
  voidDamageSpec
  weatherSpec
  waterPhysicsSpec
  paintingSpec
  redstoneBlockSpec
  cactusBlockSpec
  slabBlockSpec
  dispenserBlockSpec

-- =========================================================================
-- Block
-- =========================================================================
blockSpec :: Spec
blockSpec = describe "World.Block" $ do
  it "Air is transparent and not solid" $ do
    isTransparent Air `shouldBe` True
    isSolid Air `shouldBe` False

  it "Stone is solid and not transparent" $ do
    isSolid Stone `shouldBe` True
    isTransparent Stone `shouldBe` False

  it "Water is transparent and not solid" $ do
    isTransparent Water `shouldBe` True
    isSolid Water `shouldBe` False

  it "Glass is solid and transparent" $ do
    isSolid Glass `shouldBe` True
    isTransparent Glass `shouldBe` True

  it "Bed is solid and not transparent" $ do
    isSolid Bed `shouldBe` True
    isTransparent Bed `shouldBe` False

  it "Bed has low hardness" $ do
    bpHardness (blockProperties Bed) `shouldBe` 0.2

  it "all block types roundtrip through Enum" $
    property $ \i -> let idx = abs i `mod` (fromEnum (maxBound :: BlockType) + 1)
                         bt = toEnum idx :: BlockType
                     in fromEnum bt `shouldBe` idx

  it "blockFaceTexCoords returns valid atlas coords" $ do
    let V2 u v = blockFaceTexCoords Stone FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)
    u `shouldSatisfy` (< 16)
    v `shouldSatisfy` (< 16)

  it "Grass has different top/side textures" $ do
    blockFaceTexCoords Grass FaceTop `shouldNotBe` blockFaceTexCoords Grass FaceEast

  it "Ladder is not solid and is transparent" $ do
    isSolid Ladder `shouldBe` False
    isTransparent Ladder `shouldBe` True

  it "Ladder has hardness 0.4" $ do
    bpHardness (blockProperties Ladder) `shouldBe` 0.4

  it "OakDoorClosed is solid and not transparent" $ do
    isSolid OakDoorClosed `shouldBe` True
    isTransparent OakDoorClosed `shouldBe` False

  it "OakDoorOpen is not solid and is transparent" $ do
    isSolid OakDoorOpen `shouldBe` False
    isTransparent OakDoorOpen `shouldBe` True

  it "OakDoorClosed and OakDoorOpen have different properties" $ do
    blockProperties OakDoorClosed `shouldNotBe` blockProperties OakDoorOpen

-- =========================================================================
-- Chunk
-- =========================================================================
chunkSpec :: Spec
chunkSpec = describe "World.Chunk" $ do
  it "blockIndex is consistent with dimensions" $ do
    blockIndex 0 0 0 `shouldBe` 0
    blockIndex 15 0 0 `shouldBe` 15
    blockIndex 0 0 15 `shouldBe` 15 * chunkWidth
    blockIndex 0 1 0 `shouldBe` chunkWidth * chunkDepth

  it "new chunk is all Air" $ do
    chunk <- newChunk (V2 0 0)
    bt <- getBlock chunk 8 64 8
    bt `shouldBe` Air

  it "setBlock then getBlock roundtrips" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    bt <- getBlock chunk 5 10 5
    bt `shouldBe` Stone

  it "out of bounds getBlock returns Air" $ do
    chunk <- newChunk (V2 0 0)
    bt <- getBlock chunk (-1) 0 0
    bt `shouldBe` Air
    bt2 <- getBlock chunk 0 256 0
    bt2 `shouldBe` Air

  it "isInBounds checks all axes" $ do
    isInBounds 0 0 0 `shouldBe` True
    isInBounds 15 255 15 `shouldBe` True
    isInBounds 16 0 0 `shouldBe` False
    isInBounds 0 (-1) 0 `shouldBe` False
    isInBounds 0 0 16 `shouldBe` False

  it "setBlock does not affect other positions" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    bt <- getBlock chunk 5 10 6
    bt `shouldBe` Air

-- =========================================================================
-- Noise
-- =========================================================================
noiseSpec :: Spec
noiseSpec = describe "World.Noise" $ do
  it "noise2D is deterministic (same seed+coords = same value)" $ do
    let a = noise2D 42 1.5 2.5
        b = noise2D 42 1.5 2.5
    a `shouldBe` b

  it "noise2D returns values in [-1, 1]" $
    property $ \(x', y') ->
      let x = fromIntegral (x' :: Int) * 0.1
          y = fromIntegral (y' :: Int) * 0.1
          n = noise2D 42 x y
      in n >= -1.5 && n <= 1.5  -- slight tolerance for floating point

  it "different seeds produce different output" $ do
    let a = noise2D 42 1.7 2.3
        b = noise2D 99 1.7 2.3
    a `shouldNotBe` b

  it "fractalNoise2D is deterministic" $ do
    let a = fractalNoise2D 42 4 2.0 0.5 10.0 20.0
        b = fractalNoise2D 42 4 2.0 0.5 10.0 20.0
    a `shouldBe` b

  it "noise3D is deterministic" $ do
    let a = noise3D 42 1.0 2.0 3.0
        b = noise3D 42 1.0 2.0 3.0
    a `shouldBe` b

-- =========================================================================
-- Inventory
-- =========================================================================
inventorySpec :: Spec
inventorySpec = describe "Game.Inventory" $ do
  it "empty inventory has no selected item" $ do
    selectedItem emptyInventory `shouldBe` Nothing

  it "addItem places items in first empty slot" $ do
    let (inv, remaining) = addItem emptyInventory (BlockItem Stone) 10
    remaining `shouldBe` 0
    getSlot inv 0 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

  it "addItem merges into existing stacks" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 32
        (inv2, _) = addItem inv1 (BlockItem Stone) 16
    getSlot inv2 0 `shouldBe` Just (ItemStack (BlockItem Stone) 48)

  it "addItem respects stack limit of 64" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 64
        (inv2, _) = addItem inv1 (BlockItem Stone) 10
    getSlot inv2 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)
    getSlot inv2 1 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

  it "addItem returns leftover when inventory is full" $ do
    let fillAll inv 0 = inv
        fillAll inv n = let (inv', _) = addItem inv (BlockItem Stone) 64
                        in fillAll inv' (n - 1)
        full = fillAll emptyInventory inventorySlots
        (_, leftover) = addItem full (BlockItem Stone) 10
    leftover `shouldBe` 10

  it "removeItem removes from end first" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 32
        (inv2, _) = addItem inv1 (BlockItem Dirt) 16
        (inv3, removed) = removeItem inv2 (BlockItem Stone) 10
    removed `shouldBe` 10
    getSlot inv3 0 `shouldBe` Just (ItemStack (BlockItem Stone) 22)

  it "removeItem returns actual count removed" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 5
        (inv2, removed) = removeItem inv1 (BlockItem Stone) 10
    removed `shouldBe` 5
    getSlot inv2 0 `shouldBe` Nothing

  it "selectHotbar changes selected slot" $ do
    let inv = selectHotbar emptyInventory 3
    invSelected inv `shouldBe` 3

  it "selectHotbar clamps to valid range" $ do
    let inv = selectHotbar emptyInventory 99
    invSelected inv `shouldBe` 0  -- unchanged

  it "selectedItem returns item in active hotbar slot" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 10
        inv2 = selectHotbar inv1 0
    selectedItem inv2 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

  it "tool items stack to 1" $ do
    let tool = ToolItem Pickaxe Diamond 1561
        (inv1, _) = addItem emptyInventory tool 1
        (inv2, _) = addItem inv1 tool 1
    getSlot inv2 0 `shouldBe` Just (ItemStack tool 1)
    getSlot inv2 1 `shouldBe` Just (ItemStack tool 1)

-- =========================================================================
-- Crafting
-- =========================================================================
craftingSpec :: Spec
craftingSpec = describe "Game.Crafting" $ do
  it "empty grid produces CraftFailure" $ do
    tryCraft (emptyCraftingGrid 3) `shouldBe` CraftFailure

  it "single OakLog produces 4 OakPlanks" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    tryCraft grid `shouldBe` CraftSuccess (BlockItem OakPlanks) 4

  it "2x2 planks produces CraftingTable" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 1 0 p) 1 1 p
    tryCraft grid `shouldBe` CraftSuccess (BlockItem CraftingTable) 1

  it "recipe works regardless of position in grid" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 1 1 (Just (BlockItem OakLog))
    tryCraft grid `shouldBe` CraftSuccess (BlockItem OakPlanks) 4

  it "wrong pattern produces CraftFailure" $ do
    let grid = setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 (Just (BlockItem Stone)))
                0 1 (Just (BlockItem Dirt))
    tryCraft grid `shouldBe` CraftFailure

  it "2x2 grid can craft simple recipes" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 2) 0 0 (Just (BlockItem OakLog))
    tryCraft grid `shouldBe` CraftSuccess (BlockItem OakPlanks) 4

  it "sand produces glass" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem Sand))
    tryCraft grid `shouldBe` CraftSuccess (BlockItem Glass) 1

  it "wood pickaxe recipe works" $ do
    let p = Just (BlockItem OakPlanks)
        s = Just StickItem
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 0 2 p) 1 1 s) 2 1 s
    case tryCraft grid of
      CraftSuccess (ToolItem Pickaxe Wood _) 1 -> pure ()
      other -> expectationFailure $ "Expected wood pickaxe, got: " ++ show other

  it "6 planks in 2x3 pattern produces door" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 1 0 p) 1 1 p) 2 0 p) 2 1 p
    tryCraft grid `shouldBe` CraftSuccess (BlockItem OakDoorClosed) 1

-- =========================================================================
-- World coordinate mapping
-- =========================================================================
worldCoordSpec :: Spec
worldCoordSpec = describe "World.World.worldToChunkLocal" $ do
  it "positive coordinates map correctly" $ do
    worldToChunkLocal 0 16 `shouldBe` (0, 0)
    worldToChunkLocal 15 16 `shouldBe` (0, 15)
    worldToChunkLocal 16 16 `shouldBe` (1, 0)
    worldToChunkLocal 31 16 `shouldBe` (1, 15)

  it "negative coordinates map correctly" $ do
    worldToChunkLocal (-1) 16 `shouldBe` (-1, 15)
    worldToChunkLocal (-16) 16 `shouldBe` (-1, 0)
    worldToChunkLocal (-17) 16 `shouldBe` (-2, 15)

  it "local offset is always in [0, size)" $
    property $ \w -> let size = 16
                         (_, l) = worldToChunkLocal w size
                     in l >= 0 && l < size

-- =========================================================================
-- Player input queue
-- =========================================================================
playerInputSpec :: Spec
playerInputSpec = describe "Game.Player input queue" $ do
  let airQuery _ _ _ = pure False
      airHeightQuery :: BlockHeightQuery
      airHeightQuery _ _ _ = pure 0.0
      queuedToggle = noInput { piToggleFly = True, piMouseDX = 4, piMouseDY = -2 }

  it "preserves fly toggle when no physics tick ran" $ do
    endFrameInput False queuedToggle `shouldBe` noInput { piToggleFly = True }

  it "clears fly toggle after a physics tick runs" $ do
    endFrameInput True queuedToggle `shouldBe` noInput

  it "applies a queued fly toggle exactly once" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = True }

    player1 <- updatePlayer 0 queuedToggle airHeightQuery airQuery airQuery player0
    player2 <- updatePlayer 0 (endFrameInput True queuedToggle) airHeightQuery airQuery airQuery player1

    plFlying player1 `shouldBe` False
    plFlying player2 `shouldBe` False

-- =========================================================================
-- Item system
-- =========================================================================
itemSpec :: Spec
itemSpec = describe "Game.Item" $ do
  it "BlockItem converts to block type" $ do
    itemToBlock (BlockItem Stone) `shouldBe` Just Stone
    itemToBlock (BlockItem Dirt) `shouldBe` Just Dirt

  it "ToolItem does not convert to block type" $ do
    itemToBlock (ToolItem Pickaxe Wood 59) `shouldBe` Nothing

  it "block items stack to 64" $ do
    itemStackLimit (BlockItem Stone) `shouldBe` 64

  it "tool items stack to 1" $ do
    itemStackLimit (ToolItem Sword Diamond 1561) `shouldBe` 1

  it "tool info has correct durability for diamond" $ do
    tiMaxDurability (toolInfo Diamond) `shouldBe` 1561

  it "food items stack to 64" $ do
    itemStackLimit (FoodItem RawPorkchop) `shouldBe` 64
    itemStackLimit (FoodItem RottenFlesh) `shouldBe` 64

  it "material items stack to 64" $ do
    itemStackLimit (MaterialItem Bone) `shouldBe` 64
    itemStackLimit (MaterialItem Gunpowder) `shouldBe` 64

  it "food items are not block items" $ do
    isBlockItem (FoodItem RawBeef) `shouldBe` False
    isBlockItem (FoodItem Apple) `shouldBe` False

  it "material items are not block items" $ do
    isBlockItem (MaterialItem StringMat) `shouldBe` False
    isBlockItem (MaterialItem Leather) `shouldBe` False

  it "food items do not convert to block type" $ do
    itemToBlock (FoodItem RawPorkchop) `shouldBe` Nothing

  it "material items do not convert to block type" $ do
    itemToBlock (MaterialItem Bone) `shouldBe` Nothing

  it "blockDrops returns cobblestone for stone" $ do
    blockDrops Stone `shouldBe` [(BlockItem Cobblestone, 1)]

  it "blockDrops returns nothing for glass" $ do
    blockDrops Glass `shouldBe` []

  it "blockDrops returns nothing for air" $ do
    blockDrops Air `shouldBe` []

  it "blockDrops for OakDoorClosed returns closed door" $ do
    blockDrops OakDoorClosed `shouldBe` [(BlockItem OakDoorClosed, 1)]

  it "blockDrops for OakDoorOpen returns closed door" $ do
    blockDrops OakDoorOpen `shouldBe` [(BlockItem OakDoorClosed, 1)]

-- =========================================================================
-- Dropped items
-- =========================================================================
droppedItemSpec :: Spec
droppedItemSpec = describe "Game.DroppedItem" $ do
  it "new dropped items list is empty" $ do
    di <- newDroppedItems
    collected <- collectNearby di (V3 0 0 0) 100
    collected `shouldBe` []

  it "spawned items can be collected nearby" $ do
    di <- newDroppedItems
    spawnDrop di (BlockItem Stone) 5 (V3 10 65 10)
    collected <- collectNearby di (V3 10 65 10) 2.0
    length collected `shouldBe` 1
    case collected of
      [(item, cnt)] -> do
        item `shouldBe` BlockItem Stone
        cnt `shouldBe` 5
      _ -> expectationFailure "Expected exactly one item"

  it "items too far away are not collected" $ do
    di <- newDroppedItems
    spawnDrop di (BlockItem Dirt) 1 (V3 100 65 100)
    collected <- collectNearby di (V3 0 65 0) 5.0
    collected `shouldBe` []

-- =========================================================================
-- New block types
-- =========================================================================
newBlockSpec :: Spec
newBlockSpec = describe "New block types (WU-00)" $ do
  it "Obsidian is solid and opaque" $ do
    isSolid Obsidian `shouldBe` True
    isTransparent Obsidian `shouldBe` False

  it "Obsidian has hardness 50" $ do
    bpHardness (blockProperties Obsidian) `shouldBe` 50.0

  it "Ladder is transparent and not solid" $ do
    isTransparent Ladder `shouldBe` True
    isSolid Ladder `shouldBe` False

  it "OakDoorOpen is transparent and not solid" $ do
    isTransparent OakDoorOpen `shouldBe` True
    isSolid OakDoorOpen `shouldBe` False

  it "OakDoorClosed is solid" $ do
    isSolid OakDoorClosed `shouldBe` True

  it "OakFence is solid and transparent" $ do
    isSolid OakFence `shouldBe` True
    isTransparent OakFence `shouldBe` True

  it "WheatCrop is not solid and transparent" $ do
    isSolid WheatCrop `shouldBe` False
    isTransparent WheatCrop `shouldBe` True

  it "OakSapling is not solid and transparent" $ do
    isSolid OakSapling `shouldBe` False
    isTransparent OakSapling `shouldBe` True

  it "Farmland is solid and opaque" $ do
    isSolid Farmland `shouldBe` True
    isTransparent Farmland `shouldBe` False

  it "new block types roundtrip through Enum" $ do
    fromEnum Obsidian `shouldBe` 27
    fromEnum OakSapling `shouldBe` 35
    toEnum 27 `shouldBe` (Obsidian :: BlockType)
    toEnum 35 `shouldBe` (OakSapling :: BlockType)

-- =========================================================================
-- New item types
-- =========================================================================
newItemSpec :: Spec
newItemSpec = describe "New item types (WU-00)" $ do
  it "StickItem stacks to 64" $ do
    itemStackLimit StickItem `shouldBe` 64

  it "FoodItem stacks to 64" $ do
    itemStackLimit (FoodItem Bread) `shouldBe` 64

  it "MaterialItem stacks to 64" $ do
    itemStackLimit (MaterialItem Coal) `shouldBe` 64

  it "ArmorItem stacks to 1" $ do
    itemStackLimit (ArmorItem Helmet IronArmor 100) `shouldBe` 1

  it "StickItem is not a block item" $ do
    isBlockItem StickItem `shouldBe` False
    itemToBlock StickItem `shouldBe` Nothing

  it "FoodItem is not a block item" $ do
    isBlockItem (FoodItem Apple) `shouldBe` False

  it "CoalOre drops Coal material item" $ do
    blockDrops CoalOre `shouldBe` [(MaterialItem Coal, 1)]

  it "DiamondOre drops DiamondGem material item" $ do
    blockDrops DiamondOre `shouldBe` [(MaterialItem DiamondGem, 1)]

  it "Obsidian requires diamond harvest level" $ do
    blockRequiredHarvestLevel Obsidian `shouldBe` 4

  it "stick crafting recipe produces StickItem" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3) 0 0 p) 1 0 p
    tryCraft grid `shouldBe` CraftSuccess StickItem 4

  it "food hunger restore values are correct" $ do
    foodHungerRestore CookedPorkchop `shouldBe` 8
    foodHungerRestore Apple `shouldBe` 4
    foodHungerRestore Bread `shouldBe` 5
    foodHungerRestore RawBeef `shouldBe` 3

  it "armor defense points are correct" $ do
    armorDefensePoints Chestplate DiamondArmor `shouldBe` 8
    armorDefensePoints Helmet LeatherArmor `shouldBe` 1
    armorDefensePoints Boots IronArmor `shouldBe` 2

-- =========================================================================
-- Falling block gravity
-- =========================================================================

-- | Create a test world with a single chunk at (0,0) for gravity tests
makeTestWorld :: IO World
makeTestWorld = do
  world <- newWorld defaultGenConfig 4
  chunk <- newChunk (V2 0 0)
  atomically $ writeTVar (worldChunks world) (HM.singleton (V2 0 0) chunk)
  pure world

gravitySpec :: Spec
gravitySpec = describe "Falling block gravity" $ do
  it "Sand is gravity-affected" $
    isGravityAffected Sand `shouldBe` True

  it "Gravel is gravity-affected" $
    isGravityAffected Gravel `shouldBe` True

  it "Stone is not gravity-affected" $
    isGravityAffected Stone `shouldBe` False

  it "Air is not gravity-affected" $
    isGravityAffected Air `shouldBe` False

  it "triggerGravityAbove drops sand into air below" $ do
    world <- makeTestWorld
    -- Place stone at y=5, sand at y=7, air at y=6
    worldSetBlock world (V3 1 5 1) Stone
    worldSetBlock world (V3 1 7 1) Sand
    -- Trigger gravity from y=6 (already air)
    moved <- triggerGravityAbove world (V3 1 6 1)
    -- Sand at y=7 should have fallen to y=6
    btAt6 <- worldGetBlock world (V3 1 6 1)
    btAt7 <- worldGetBlock world (V3 1 7 1)
    btAt6 `shouldBe` Sand
    btAt7 `shouldBe` Air
    moved `shouldBe` True

  it "triggerGravityAbove cascades multiple sand blocks" $ do
    world <- makeTestWorld
    -- Stack: stone at y=3, air at y=4, sand at y=5, sand at y=6
    worldSetBlock world (V3 2 3 2) Stone
    worldSetBlock world (V3 2 5 2) Sand
    worldSetBlock world (V3 2 6 2) Sand
    -- Trigger gravity from y=4 (air gap)
    _ <- triggerGravityAbove world (V3 2 4 2)
    -- Sand should cascade: y=5 sand -> y=4, y=6 sand -> y=5
    btAt4 <- worldGetBlock world (V3 2 4 2)
    btAt5 <- worldGetBlock world (V3 2 5 2)
    btAt6 <- worldGetBlock world (V3 2 6 2)
    btAt4 `shouldBe` Sand
    btAt5 `shouldBe` Sand
    btAt6 `shouldBe` Air

  it "triggerGravityAbove does nothing when block above is not gravity-affected" $ do
    world <- makeTestWorld
    worldSetBlock world (V3 3 5 3) Stone
    moved <- triggerGravityAbove world (V3 3 4 3)
    btAt4 <- worldGetBlock world (V3 3 4 3)
    btAt5 <- worldGetBlock world (V3 3 5 3)
    btAt4 `shouldBe` Air
    btAt5 `shouldBe` Stone
    moved `shouldBe` False

  it "settleGravityBlock drops sand to lowest air" $ do
    world <- makeTestWorld
    -- Stone floor at y=2, air at y=3 and y=4, sand at y=5
    worldSetBlock world (V3 4 2 4) Stone
    worldSetBlock world (V3 4 5 4) Sand
    moved <- settleGravityBlock world (V3 4 5 4)
    moved `shouldBe` True
    btAt3 <- worldGetBlock world (V3 4 3 4)
    btAt5 <- worldGetBlock world (V3 4 5 4)
    btAt3 `shouldBe` Sand
    btAt5 `shouldBe` Air

  it "settleGravityBlock does not move block on solid ground" $ do
    world <- makeTestWorld
    worldSetBlock world (V3 5 3 5) Stone
    worldSetBlock world (V3 5 4 5) Sand
    moved <- settleGravityBlock world (V3 5 4 5)
    moved `shouldBe` False
    btAt4 <- worldGetBlock world (V3 5 4 5)
    btAt4 `shouldBe` Sand

  it "settleChunkGravity settles floating sand in a chunk" $ do
    world <- makeTestWorld
    -- Place stone floor and floating sand
    worldSetBlock world (V3 6 2 6) Stone
    worldSetBlock world (V3 6 8 6) Sand
    chunks <- atomically $ readTVar (worldChunks world)
    case HM.lookup (V2 0 0) chunks of
      Nothing -> expectationFailure "Chunk not found"
      Just chunk -> do
        moved <- settleChunkGravity world chunk
        moved `shouldBe` True
        btAt3 <- worldGetBlock world (V3 6 3 6)
        btAt8 <- worldGetBlock world (V3 6 8 6)
        btAt3 `shouldBe` Sand
        btAt8 `shouldBe` Air

-- Save/Load Binary roundtrip
-- =========================================================================
saveSpec :: Spec
saveSpec = describe "Game.Save Binary roundtrip" $ do
  it "BlockItem roundtrips through Binary" $ do
    let item = BlockItem Stone
    decode (encode item) `shouldBe` item

  it "BlockItem Dirt roundtrips through Binary" $ do
    let item = BlockItem Dirt
    decode (encode item) `shouldBe` item

  it "ToolItem roundtrips through Binary" $ do
    let item = ToolItem Pickaxe Diamond 1561
    decode (encode item) `shouldBe` item

  it "ToolItem Sword roundtrips through Binary" $ do
    let item = ToolItem Sword Wood 59
    decode (encode item) `shouldBe` item

  it "all BlockTypes roundtrip through Binary" $ do
    let allBlocks = [minBound .. maxBound] :: [BlockType]
    mapM_ (\bt -> decode (encode bt) `shouldBe` bt) allBlocks

  it "all ToolTypes roundtrip through Binary" $ do
    let allTools = [minBound .. maxBound] :: [ToolType]
    mapM_ (\tt -> decode (encode tt) `shouldBe` tt) allTools

  it "all ToolMaterials roundtrip through Binary" $ do
    let allMats = [minBound .. maxBound] :: [ToolMaterial]
    mapM_ (\tm -> decode (encode tm) `shouldBe` tm) allMats

  it "SaveData roundtrips through Binary" $ do
    let sd = SaveData
          { sdPlayerPos    = (1.5, 65.0, -3.2)
          , sdPlayerYaw    = 45.0
          , sdPlayerPitch  = -10.0
          , sdPlayerFlying = True
          , sdWorldSeed    = 12345
          , sdDayTime      = 0.75
          , sdDayCount     = 3
          , sdHealth       = 15
          , sdHunger       = 12
          , sdFallDist     = 2.3
          , sdInventory    = [(0, BlockItem Stone, 64), (1, ToolItem Pickaxe Diamond 1500, 1)]
          , sdSelectedSlot = 1
          }
    decode (encode sd) `shouldBe` sd

  it "SaveData with empty inventory roundtrips" $ do
    let sd = SaveData
          { sdPlayerPos    = (0, 80, 0)
          , sdPlayerYaw    = 0
          , sdPlayerPitch  = 0
          , sdPlayerFlying = False
          , sdWorldSeed    = 42
          , sdDayTime      = 0.25
          , sdDayCount     = 0
          , sdHealth       = 20
          , sdHunger       = 20
          , sdFallDist     = 0
          , sdInventory    = []
          , sdSelectedSlot = 0
          }
    decode (encode sd) `shouldBe` sd

  it "inventoryToSlotList and slotListToInventory roundtrip" $ do
    let inv0 = emptyInventory
        (inv1, _) = addItem inv0 (BlockItem Stone) 32
        (inv2, _) = addItem inv1 (ToolItem Pickaxe Wood 59) 1
        inv3 = selectHotbar inv2 1
        slots = inventoryToSlotList inv3
        restored = slotListToInventory slots (invSelected inv3)
    invSelected restored `shouldBe` invSelected inv3
    -- Check each slot matches
    mapM_ (\i -> getSlot restored i `shouldBe` getSlot inv3 i) [0 .. inventorySlots - 1]
-- Block Entity (chests)
-- =========================================================================
blockEntitySpec :: Spec
blockEntitySpec = describe "Game.BlockEntity" $ do
  it "new block entity map has no entries" $ do
    bem <- newBlockEntityMap
    exists <- hasBlockEntity bem (V3 0 64 0)
    exists `shouldBe` False

  it "setting a chest inventory makes it retrievable" $ do
    bem <- newBlockEntityMap
    let pos = V3 10 64 10
    setChestInventory bem pos emptyChestInventory
    mInv <- getChestInventory bem pos
    case mInv of
      Just inv -> V.length (invSlots inv) `shouldBe` chestSlots
      Nothing  -> expectationFailure "Expected chest inventory"

  it "hasBlockEntity returns True after set" $ do
    bem <- newBlockEntityMap
    let pos = V3 5 32 5
    setChestInventory bem pos emptyChestInventory
    exists <- hasBlockEntity bem pos
    exists `shouldBe` True

  it "getChestInventory returns Nothing for missing position" $ do
    bem <- newBlockEntityMap
    mInv <- getChestInventory bem (V3 99 99 99)
    mInv `shouldBe` Nothing

  it "removeBlockEntity returns data and clears it" $ do
    bem <- newBlockEntityMap
    let pos = V3 1 2 3
    setChestInventory bem pos emptyChestInventory
    mBe <- removeBlockEntity bem pos
    case mBe of
      Just (ChestData _) -> pure ()
      Nothing -> expectationFailure "Expected ChestData"
    exists <- hasBlockEntity bem pos
    exists `shouldBe` False

  it "chest slot get/set roundtrips" $ do
    let item = ItemStack (BlockItem Stone) 32
        inv = setChestSlot emptyChestInventory 13 (Just item)
    getChestSlot inv 13 `shouldBe` Just item
    getChestSlot inv 0 `shouldBe` Nothing

  it "chest has 27 slots" $ do
    chestSlots `shouldBe` 27
    V.length (invSlots emptyChestInventory) `shouldBe` 27

  it "multiple chests at different positions are independent" $ do
    bem <- newBlockEntityMap
    let pos1 = V3 0 64 0
        pos2 = V3 10 64 10
        item1 = ItemStack (BlockItem Stone) 16
        item2 = ItemStack (BlockItem Dirt) 8
        inv1 = setChestSlot emptyChestInventory 0 (Just item1)
        inv2 = setChestSlot emptyChestInventory 0 (Just item2)
    setChestInventory bem pos1 inv1
    setChestInventory bem pos2 inv2
    mInv1 <- getChestInventory bem pos1
    mInv2 <- getChestInventory bem pos2
    case (mInv1, mInv2) of
      (Just r1, Just r2) -> do
        getChestSlot r1 0 `shouldBe` Just item1
        getChestSlot r2 0 `shouldBe` Just item2
      _ -> expectationFailure "Expected both chest inventories"

  it "getFurnaceState returns Nothing for missing position" $ do
    bem <- newBlockEntityMap
    mFs <- getFurnaceState bem (V3 0 64 0)
    mFs `shouldBe` Nothing

  it "setFurnaceState/getFurnaceState roundtrip" $ do
    bem <- newBlockEntityMap
    let pos = V3 5 32 5
        fs = setFurnaceInput newFurnaceState (Just (ItemStack (BlockItem IronOre) 3))
    setFurnaceState bem pos fs
    mFs <- getFurnaceState bem pos
    case mFs of
      Just fs' -> getFurnaceInput fs' `shouldBe` Just (ItemStack (BlockItem IronOre) 3)
      Nothing  -> expectationFailure "Expected furnace state"

  it "hasBlockEntity returns True after setFurnaceState" $ do
    bem <- newBlockEntityMap
    let pos = V3 1 2 3
    setFurnaceState bem pos newFurnaceState
    exists <- hasBlockEntity bem pos
    exists `shouldBe` True

  it "removeBlockEntity returns FurnaceData and clears it" $ do
    bem <- newBlockEntityMap
    let pos = V3 10 20 30
    setFurnaceState bem pos newFurnaceState
    mBe <- removeBlockEntity bem pos
    case mBe of
      Just (FurnaceData _) -> pure ()
      _ -> expectationFailure "Expected FurnaceData"
    exists <- hasBlockEntity bem pos
    exists `shouldBe` False

  it "furnace and chest at different positions are independent" $ do
    bem <- newBlockEntityMap
    let chestPos = V3 0 64 0
        furnacePos = V3 10 64 10
        fs = setFurnaceInput newFurnaceState (Just (ItemStack (MaterialItem Coal) 1))
    setChestInventory bem chestPos emptyChestInventory
    setFurnaceState bem furnacePos fs
    mChest <- getChestInventory bem chestPos
    mFurnace <- getFurnaceState bem furnacePos
    case (mChest, mFurnace) of
      (Just _, Just fs') -> getFurnaceInput fs' `shouldBe` Just (ItemStack (MaterialItem Coal) 1)
      _ -> expectationFailure "Expected both entities"
    -- Cross-type lookups return Nothing
    mWrong1 <- getFurnaceState bem chestPos
    mWrong1 `shouldBe` Nothing
    mWrong2 <- getChestInventory bem furnacePos
    mWrong2 `shouldBe` Nothing

  it "allFurnaceEntities returns only furnaces" $ do
    bem <- newBlockEntityMap
    let fPos1 = V3 0 64 0
        fPos2 = V3 10 64 10
        cPos  = V3 20 64 20
    setFurnaceState bem fPos1 newFurnaceState
    setFurnaceState bem fPos2 (setFurnaceFuel newFurnaceState (Just (ItemStack (MaterialItem Coal) 5)))
    setChestInventory bem cPos emptyChestInventory
    furnaces <- allFurnaceEntities bem
    length furnaces `shouldBe` 2
-- Furnace
-- =========================================================================
furnaceSpec :: Spec
furnaceSpec = describe "Game.Furnace" $ do
  it "new furnace state is empty" $ do
    let fs = newFurnaceState
    getFurnaceInput fs `shouldBe` Nothing
    getFurnaceFuel fs `shouldBe` Nothing
    getFurnaceOutput fs `shouldBe` Nothing
    fsSmeltTime fs `shouldBe` 0
    fsFuelTime fs `shouldBe` 0

  it "has smelting recipes for ores" $ do
    length smeltRecipes `shouldSatisfy` (>= 5)
    -- Iron ore should have a recipe
    let hasIronRecipe = any (\r -> srInput r == BlockItem IronOre) smeltRecipes
    hasIronRecipe `shouldBe` True

  it "coal has fuel burn time" $ do
    fuelBurnTime (MaterialItem Coal) `shouldSatisfy` (> 0)

  it "oak planks have fuel burn time" $ do
    fuelBurnTime (BlockItem OakPlanks) `shouldSatisfy` (> 0)

  it "stone has no fuel burn time" $ do
    fuelBurnTime (BlockItem Stone) `shouldBe` 0

  it "tickFurnace with no input/fuel does nothing" $ do
    let fs = tickFurnace 1.0 newFurnaceState
    getFurnaceInput fs `shouldBe` Nothing
    getFurnaceOutput fs `shouldBe` Nothing

  it "tickFurnace consumes fuel and smelts input" $ do
    let fs0 = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem IronOre) 1)
          , fsFuel  = Just (ItemStack (MaterialItem Coal) 1)
          }
        -- Tick enough to consume fuel and complete smelting (10 seconds)
        fs1 = iterate (tickFurnace 1.0) fs0 !! 11
    -- Output should have an iron ingot
    getFurnaceOutput fs1 `shouldBe` Just (ItemStack (MaterialItem IronIngot) 1)
    -- Input should be consumed
    getFurnaceInput fs1 `shouldBe` Nothing

  it "tickFurnace does not smelt without fuel" $ do
    let fs0 = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem IronOre) 1)
          }
        fs1 = tickFurnace 5.0 fs0
    getFurnaceOutput fs1 `shouldBe` Nothing
    -- Input is unchanged
    getFurnaceInput fs1 `shouldBe` Just (ItemStack (BlockItem IronOre) 1)

  it "tickFurnace does not smelt invalid input" $ do
    let fs0 = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem Dirt) 1)
          , fsFuel  = Just (ItemStack (MaterialItem Coal) 1)
          }
        fs1 = tickFurnace 15.0 fs0
    getFurnaceOutput fs1 `shouldBe` Nothing

  it "setFurnaceInput/getFurnaceInput roundtrip" $ do
    let stack = Just (ItemStack (BlockItem IronOre) 3)
        fs = setFurnaceInput newFurnaceState stack
    getFurnaceInput fs `shouldBe` stack

  it "setFurnaceFuel/getFurnaceFuel roundtrip" $ do
    let stack = Just (ItemStack (MaterialItem Coal) 5)
        fs = setFurnaceFuel newFurnaceState stack
    getFurnaceFuel fs `shouldBe` stack

  it "MaterialItem has stack limit 64" $ do
    itemStackLimit (MaterialItem IronIngot) `shouldBe` 64
    itemStackLimit (MaterialItem Coal) `shouldBe` 64

  it "MaterialItem does not convert to block" $ do
    itemToBlock (MaterialItem IronIngot) `shouldBe` Nothing
    itemToBlock (MaterialItem GoldIngot) `shouldBe` Nothing

-- =========================================================================
-- Void damage
-- =========================================================================
voidDamageSpec :: Spec
voidDamageSpec = describe "Void damage" $ do
  it "damagePlayer reduces health" $ do
    let player = defaultPlayer (V3 0 80 0)
    plHealth (damagePlayer 4 player) `shouldBe` (maxHealth - 4)

  it "damagePlayer does not go below 0" $ do
    let player = (defaultPlayer (V3 0 80 0)) { plHealth = 2 }
    plHealth (damagePlayer 4 player) `shouldBe` 0

  it "player below Y=0 should be considered in void" $ do
    let player = (defaultPlayer (V3 0 (-5) 0)) { plFlying = False }
        V3 _ py _ = plPos player
    py `shouldSatisfy` (< 0)

  it "isPlayerDead detects zero health" $ do
    let player = (defaultPlayer (V3 0 80 0)) { plHealth = 0 }
    isPlayerDead player `shouldBe` True

  it "respawnPlayer restores full health" $ do
    let player = (defaultPlayer (V3 0 80 0)) { plHealth = 0 }
        respawned = respawnPlayer (V3 0 80 0) player
    plHealth respawned `shouldBe` maxHealth

-- Weather
-- =========================================================================
weatherSpec :: Spec
weatherSpec = describe "World.Weather" $ do
  it "newWeatherState starts clear" $ do
    wsType newWeatherState `shouldBe` Clear
    isRaining newWeatherState `shouldBe` False

  it "newWeatherState has zero intensity" $ do
    wsIntensity newWeatherState `shouldBe` 0.0

  it "newWeatherState has 300s timer" $ do
    wsTimer newWeatherState `shouldBe` 300.0

  it "weatherSkyMultiplier is 1.0 at zero intensity" $ do
    weatherSkyMultiplier newWeatherState `shouldBe` 1.0

  it "weatherAmbientMultiplier is 1.0 at zero intensity" $ do
    weatherAmbientMultiplier newWeatherState `shouldBe` 1.0

  it "weatherSkyMultiplier decreases with intensity" $ do
    let rainy = WeatherState Rain 100.0 1.0
    weatherSkyMultiplier rainy `shouldBe` 0.7

  it "weatherAmbientMultiplier decreases with intensity" $ do
    let rainy = WeatherState Rain 100.0 1.0
    weatherAmbientMultiplier rainy `shouldBe` 0.8

  it "updateWeather decrements timer" $ do
    ws <- updateWeather 1.0 newWeatherState
    wsTimer ws `shouldSatisfy` (< wsTimer newWeatherState)

  it "updateWeather transitions Clear to Rain when timer expires" $ do
    let almostDone = WeatherState Clear 0.5 0.0
    ws <- updateWeather 1.0 almostDone
    wsType ws `shouldBe` Rain
    wsTimer ws `shouldSatisfy` (>= 180.0)
    wsTimer ws `shouldSatisfy` (<= 300.0)

  it "updateWeather transitions Rain to Clear when timer expires" $ do
    let almostDone = WeatherState Rain 0.5 1.0
    ws <- updateWeather 1.0 almostDone
    wsType ws `shouldBe` Clear
    wsTimer ws `shouldSatisfy` (>= 300.0)
    wsTimer ws `shouldSatisfy` (<= 600.0)

  it "intensity ramps up during Rain" $ do
    let rainy = WeatherState Rain 100.0 0.0
    ws <- updateWeather 1.0 rainy
    wsIntensity ws `shouldSatisfy` (> 0.0)

  it "intensity ramps down during Clear" $ do
    let clearing = WeatherState Clear 100.0 1.0
    ws <- updateWeather 1.0 clearing
    wsIntensity ws `shouldSatisfy` (< 1.0)

-- Water physics and air supply
-- =========================================================================
waterPhysicsSpec :: Spec
waterPhysicsSpec = describe "Water physics and air supply" $ do
  let airQuery _ _ _ = pure False
      airHeightQuery :: BlockHeightQuery
      airHeightQuery _ _ _ = pure 0.0
      -- Water everywhere: all blocks return True for water query
      waterQuery _ _ _ = pure True
      -- Water only at head level (y=81, since head = feet_y + 1.62, so floor(80 + 1.62) = 81)
      headWaterQuery _ y _ = pure (y == 81)

  it "defaultPlayer has full air supply" $ do
    let player = defaultPlayer (V3 0 80 0)
    plAirSupply player `shouldBe` 15.0

  it "maxAirSupply is 15.0" $ do
    maxAirSupply `shouldBe` 15.0

  it "air supply decrements when head is submerged" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = False }
    -- headWaterQuery returns True for y=81 (floor(80 + 1.62) = 81)
    player1 <- updatePlayer 0.05 noInput airHeightQuery headWaterQuery airQuery player0
    plAirSupply player1 `shouldSatisfy` (< 15.0)

  it "air supply restores when head is not submerged" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = False, plAirSupply = 10.0 }
    player1 <- updatePlayer 0.05 noInput airHeightQuery airQuery airQuery player0
    plAirSupply player1 `shouldSatisfy` (> 10.0)

  it "air supply does not exceed maxAirSupply" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = False, plAirSupply = 14.99 }
    player1 <- updatePlayer 0.05 noInput airHeightQuery airQuery airQuery player0
    plAirSupply player1 `shouldSatisfy` (<= 15.0)

  it "respawnPlayer resets air supply" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plAirSupply = 5.0 }
        player1 = respawnPlayer (V3 0 80 0) player0
    plAirSupply player1 `shouldBe` 15.0

  it "flying mode resets air supply to max" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = True, plAirSupply = 5.0 }
    player1 <- updatePlayer 0.05 noInput airHeightQuery waterQuery airQuery player0
    plAirSupply player1 `shouldBe` 15.0

  it "water movement speed is reduced (swim upward at 2.0)" $ do
    -- When in water with jump pressed, vertical velocity should be ~2.0
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = False }
        jumpInput = noInput { piJump = True }
    player1 <- updatePlayer 0.05 jumpInput airHeightQuery waterQuery airQuery player0
    -- Player should have moved upward
    let V3 _ newY _ = plPos player1
    newY `shouldSatisfy` (>= 80.0)

-- =========================================================================
-- Painting entities
-- =========================================================================
paintingSpec :: Spec
paintingSpec = describe "Painting entities" $ do
  it "can spawn a painting entity with tag Painting" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 10.5 65.0 8.5) 1.0 "Painting"
    eid `shouldSatisfy` (> 0)
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> do
        entTag ent `shouldBe` "Painting"
        entPosition ent `shouldBe` V3 10.5 65.0 8.5
        entHealth ent `shouldBe` 1.0
        entAlive ent `shouldBe` True
      Nothing -> expectationFailure "Expected painting entity"

  it "painting entity can store yaw for wall direction" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Painting"
    updateEntity ew eid (\e -> e { entYaw = 90 })
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> entYaw ent `shouldBe` 90
      Nothing -> expectationFailure "Expected painting entity"

  it "painting entity can be destroyed" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Painting"
    destroyEntity ew eid
    mEnt <- getEntity ew eid
    mEnt `shouldBe` Nothing

  it "painting entities are included in allEntities" $ do
    ew <- newEntityWorld
    _ <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Painting"
    _ <- spawnEntity ew (V3 10.0 64.0 10.0) 1.0 "zombie"
    ents <- allEntities ew
    let paintings = filter (\e -> entTag e == "Painting") ents
    length paintings `shouldBe` 1

  it "entityCount includes paintings" $ do
    ew <- newEntityWorld
    _ <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Painting"
    count <- entityCount ew
    count `shouldBe` 1

-- =========================================================================
-- Redstone blocks
-- =========================================================================
redstoneBlockSpec :: Spec
redstoneBlockSpec = describe "Redstone blocks (Lever & RedstoneDust)" $ do
  -- Block properties
  it "Lever is solid and opaque" $ do
    isSolid Lever `shouldBe` True
    isTransparent Lever `shouldBe` False

  it "Lever has hardness 0.5" $ do
    bpHardness (blockProperties Lever) `shouldBe` 0.5

  it "RedstoneDust is not solid and is transparent" $ do
    isSolid RedstoneDust `shouldBe` False
    isTransparent RedstoneDust `shouldBe` True

  it "RedstoneDust has hardness 0.0" $ do
    bpHardness (blockProperties RedstoneDust) `shouldBe` 0.0

  it "Lever emits no light" $ do
    bpLightEmit (blockProperties Lever) `shouldBe` 0

  it "RedstoneDust emits no light" $ do
    bpLightEmit (blockProperties RedstoneDust) `shouldBe` 0

  -- Enum roundtrip
  it "new block types have correct enum values" $ do
    fromEnum Lever `shouldBe` 39
    fromEnum RedstoneDust `shouldBe` 40
    toEnum 39 `shouldBe` (Lever :: BlockType)
    toEnum 40 `shouldBe` (RedstoneDust :: BlockType)

  -- Texture coords
  it "Lever has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords Lever FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  it "RedstoneDust has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords RedstoneDust FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  -- Block drops
  it "Lever drops itself" $ do
    blockDrops Lever `shouldBe` [(BlockItem Lever, 1)]

  it "RedstoneDust drops itself" $ do
    blockDrops RedstoneDust `shouldBe` [(BlockItem RedstoneDust, 1)]

  -- Crafting recipes
  it "stick + cobblestone crafts a lever" $ do
    let s = Just StickItem
        c = Just (BlockItem Cobblestone)
        grid = setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3) 0 0 s) 1 0 c
    tryCraft grid `shouldBe` CraftSuccess (BlockItem Lever) 1

  it "cobblestone + coal crafts redstone dust" $ do
    let c = Just (BlockItem Cobblestone)
        coal = Just (MaterialItem Coal)
        grid = setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3) 0 0 c) 1 0 coal
    tryCraft grid `shouldBe` CraftSuccess (BlockItem RedstoneDust) 4

  -- Redstone state
  it "newRedstoneState has zero power everywhere" $ do
    rs <- newRedstoneState
    p <- getPower rs (V3 0 0 0)
    p `shouldBe` 0

  it "setPower and getPower roundtrip" $ do
    rs <- newRedstoneState
    setPower rs (V3 5 10 5) 15
    p <- getPower rs (V3 5 10 5)
    p `shouldBe` 15

  it "setPower to 0 clears power" $ do
    rs <- newRedstoneState
    setPower rs (V3 1 2 3) 10
    setPower rs (V3 1 2 3) 0
    p <- getPower rs (V3 1 2 3)
    p `shouldBe` 0

  it "isRedstoneConductor for common blocks" $ do
    isRedstoneConductor Stone `shouldBe` True
    isRedstoneConductor Cobblestone `shouldBe` True
    isRedstoneConductor Air `shouldBe` False
    isRedstoneConductor Glass `shouldBe` False

-- =========================================================================
-- Cactus block
-- =========================================================================
cactusBlockSpec :: Spec
cactusBlockSpec = describe "Cactus block" $ do
  it "Cactus is solid and transparent" $ do
    isSolid Cactus `shouldBe` True
    isTransparent Cactus `shouldBe` True

  it "Cactus has hardness 0.4" $ do
    bpHardness (blockProperties Cactus) `shouldBe` 0.4

  it "Cactus emits no light" $ do
    bpLightEmit (blockProperties Cactus) `shouldBe` 0

  it "Cactus has correct enum position" $ do
    toEnum (fromEnum Cactus) `shouldBe` (Cactus :: BlockType)

  it "Cactus has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords Cactus FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  it "Cactus drops itself" $ do
    blockDrops Cactus `shouldBe` [(BlockItem Cactus, 1)]

  it "Cactus requires no special harvest level" $ do
    blockRequiredHarvestLevel Cactus `shouldBe` 0

  it "Cactus has no preferred tool" $ do
    blockPreferredTool Cactus `shouldBe` Nothing

  it "Cactus roundtrips through Enum" $ do
    toEnum (fromEnum Cactus) `shouldBe` (Cactus :: BlockType)

  it "Cactus is not gravity-affected" $ do
    isGravityAffected Cactus `shouldBe` False

  it "Cactus is not a leaf block" $ do
    isLeafBlock Cactus `shouldBe` False

-- =========================================================================
-- Slab blocks
-- =========================================================================
slabBlockSpec :: Spec
slabBlockSpec = describe "Slab blocks (StoneSlab & OakSlab)" $ do
  -- Block properties
  it "StoneSlab is solid and transparent (half block needs top face rendered)" $ do
    isSolid StoneSlab `shouldBe` True
    isTransparent StoneSlab `shouldBe` True

  it "OakSlab is solid and transparent" $ do
    isSolid OakSlab `shouldBe` True
    isTransparent OakSlab `shouldBe` True

  it "StoneSlab has same hardness as Stone (1.5)" $ do
    bpHardness (blockProperties StoneSlab) `shouldBe` 1.5

  it "OakSlab has same hardness as OakPlanks (2.0)" $ do
    bpHardness (blockProperties OakSlab) `shouldBe` 2.0

  -- Collision height
  it "StoneSlab has 0.5 collision height" $ do
    blockCollisionHeight StoneSlab `shouldBe` 0.5

  it "OakSlab has 0.5 collision height" $ do
    blockCollisionHeight OakSlab `shouldBe` 0.5

  it "Stone has 1.0 collision height" $ do
    blockCollisionHeight Stone `shouldBe` 1.0

  it "Air has 1.0 collision height (but is not solid)" $ do
    blockCollisionHeight Air `shouldBe` 1.0

  -- Enum roundtrip
  it "slab block types roundtrip through Enum" $ do
    toEnum (fromEnum StoneSlab) `shouldBe` (StoneSlab :: BlockType)
    toEnum (fromEnum OakSlab) `shouldBe` (OakSlab :: BlockType)

  -- Texture coords
  it "StoneSlab has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords StoneSlab FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  it "OakSlab has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords OakSlab FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  -- Block drops
  it "StoneSlab drops itself" $ do
    blockDrops StoneSlab `shouldBe` [(BlockItem StoneSlab, 1)]

  it "OakSlab drops itself" $ do
    blockDrops OakSlab `shouldBe` [(BlockItem OakSlab, 1)]

  -- Preferred tools
  it "StoneSlab prefers pickaxe" $ do
    blockPreferredTool StoneSlab `shouldBe` Just Pickaxe

  it "OakSlab prefers axe" $ do
    blockPreferredTool OakSlab `shouldBe` Just Axe

  -- Crafting recipes
  it "3 stone in a row crafts 6 stone slabs" $ do
    let s = Just (BlockItem Stone)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
          0 0 s) 0 1 s) 0 2 s
    tryCraft grid `shouldBe` CraftSuccess (BlockItem StoneSlab) 6

  it "3 oak planks in a row crafts 6 oak slabs" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
          0 0 p) 0 1 p) 0 2 p
    tryCraft grid `shouldBe` CraftSuccess (BlockItem OakSlab) 6

  -- Binary roundtrip
  it "StoneSlab roundtrips through Binary" $ do
    let item = BlockItem StoneSlab
    decode (encode item) `shouldBe` item

  it "OakSlab roundtrips through Binary" $ do
    let item = BlockItem OakSlab
    decode (encode item) `shouldBe` item

-- =========================================================================
-- Dispenser block
-- =========================================================================
dispenserBlockSpec :: Spec
dispenserBlockSpec = describe "Dispenser block" $ do
  -- Block properties
  it "Dispenser is solid and opaque" $ do
    isSolid Dispenser `shouldBe` True
    isTransparent Dispenser `shouldBe` False

  it "Dispenser has hardness 3.5" $ do
    bpHardness (blockProperties Dispenser) `shouldBe` 3.5

  it "Dispenser emits no light" $ do
    bpLightEmit (blockProperties Dispenser) `shouldBe` 0

  it "Dispenser has full collision height" $ do
    blockCollisionHeight Dispenser `shouldBe` 1.0

  -- Enum roundtrip
  it "Dispenser roundtrips through Enum" $ do
    toEnum (fromEnum Dispenser) `shouldBe` (Dispenser :: BlockType)

  -- Texture coords
  it "Dispenser has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords Dispenser FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  it "Dispenser has different front face texture" $ do
    blockFaceTexCoords Dispenser FaceSouth `shouldNotBe` blockFaceTexCoords Dispenser FaceTop

  -- Block drops
  it "Dispenser drops itself" $ do
    blockDrops Dispenser `shouldBe` [(BlockItem Dispenser, 1)]

  -- Preferred tool
  it "Dispenser prefers pickaxe" $ do
    blockPreferredTool Dispenser `shouldBe` Just Pickaxe

  -- Crafting recipe
  it "7 cobblestone + redstone dust center crafts a dispenser" $ do
    let c = Just (BlockItem Cobblestone)
        r = Just (BlockItem RedstoneDust)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 c) 0 1 c) 0 2 c) 1 0 c) 1 1 r) 1 2 c) 2 0 c) 2 1 c) 2 2 c
    tryCraft grid `shouldBe` CraftSuccess (BlockItem Dispenser) 1

  -- Binary roundtrip
  it "Dispenser roundtrips through Binary" $ do
    let item = BlockItem Dispenser
    decode (encode item) `shouldBe` item

  -- Block entity (dispenser inventory)
  it "dispenser has 9 slots" $ do
    dispenserSlots `shouldBe` 9
    V.length (invSlots emptyDispenserInventory) `shouldBe` 9

  it "setting a dispenser inventory makes it retrievable" $ do
    bem <- newBlockEntityMap
    let pos = V3 10 64 10
    setDispenserInventory bem pos emptyDispenserInventory
    mInv <- getDispenserInventory bem pos
    case mInv of
      Just inv' -> V.length (invSlots inv') `shouldBe` dispenserSlots
      Nothing  -> expectationFailure "Expected dispenser inventory"

  it "getDispenserInventory returns Nothing for missing position" $ do
    bem <- newBlockEntityMap
    mInv <- getDispenserInventory bem (V3 99 99 99)
    mInv `shouldBe` Nothing

  it "dispenser slot get/set roundtrips" $ do
    let item = ItemStack (BlockItem Stone) 32
        inv' = setDispenserSlot emptyDispenserInventory 4 (Just item)
    getDispenserSlot inv' 4 `shouldBe` Just item
    getDispenserSlot inv' 0 `shouldBe` Nothing

  it "removeBlockEntity returns DispenserData and clears it" $ do
    bem <- newBlockEntityMap
    let pos = V3 1 2 3
    setDispenserInventory bem pos emptyDispenserInventory
    mBe <- removeBlockEntity bem pos
    case mBe of
      Just (DispenserData _) -> pure ()
      _ -> expectationFailure "Expected DispenserData"
    exists <- hasBlockEntity bem pos
    exists `shouldBe` False

  it "dispenser and chest at different positions are independent" $ do
    bem <- newBlockEntityMap
    let chestPos = V3 0 64 0
        dispPos = V3 10 64 10
        item1 = ItemStack (BlockItem Stone) 16
        item2 = ItemStack (BlockItem Dirt) 8
        inv1 = setChestSlot emptyChestInventory 0 (Just item1)
        inv2 = setDispenserSlot emptyDispenserInventory 0 (Just item2)
    setChestInventory bem chestPos inv1
    setDispenserInventory bem dispPos inv2
    mChest <- getChestInventory bem chestPos
    mDisp <- getDispenserInventory bem dispPos
    case (mChest, mDisp) of
      (Just r1, Just r2) -> do
        getChestSlot r1 0 `shouldBe` Just item1
        getDispenserSlot r2 0 `shouldBe` Just item2
      _ -> expectationFailure "Expected both inventories"

  -- Redstone conductor
  it "Dispenser is a redstone conductor" $ do
    isRedstoneConductor Dispenser `shouldBe` True
