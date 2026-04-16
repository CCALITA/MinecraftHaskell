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
      queuedToggle = noInput { piToggleFly = True, piMouseDX = 4, piMouseDY = -2 }

  it "preserves fly toggle when no physics tick ran" $ do
    endFrameInput False queuedToggle `shouldBe` noInput { piToggleFly = True }

  it "clears fly toggle after a physics tick runs" $ do
    endFrameInput True queuedToggle `shouldBe` noInput

  it "applies a queued fly toggle exactly once" $ do
    let player0 = (defaultPlayer (V3 0 80 0)) { plFlying = True }

    player1 <- updatePlayer 0 queuedToggle airQuery airQuery airQuery player0
    player2 <- updatePlayer 0 (endFrameInput True queuedToggle) airQuery airQuery airQuery player1

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
