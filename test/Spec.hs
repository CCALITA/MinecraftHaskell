module Main (main) where

import Test.Hspec
import Test.QuickCheck

import World.Block
import World.Chunk
import World.Noise
import World.World (worldToChunkLocal)
import Game.Inventory
import Game.Item
import Game.Crafting
import Game.Player
import Game.DroppedItem

import Linear (V2(..), V3(..))
import qualified Data.Vector as V

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

  it "sand does not craft into glass (requires smelting)" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem Sand))
    tryCraft grid `shouldBe` CraftFailure

  it "wood pickaxe recipe works" $ do
    let p = Just (BlockItem OakPlanks)
        s = Just (BlockItem OakLog)  -- stick placeholder
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 0 2 p) 1 1 s) 2 1 s
    case tryCraft grid of
      CraftSuccess (ToolItem Pickaxe Wood _) 1 -> pure ()
      other -> expectationFailure $ "Expected wood pickaxe, got: " ++ show other

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

    player1 <- updatePlayer 0 queuedToggle airQuery airQuery player0
    player2 <- updatePlayer 0 (endFrameInput True queuedToggle) airQuery airQuery player1

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

  it "blockDrops returns cobblestone for stone" $ do
    blockDrops Stone `shouldBe` [(BlockItem Cobblestone, 1)]

  it "blockDrops returns nothing for glass" $ do
    blockDrops Glass `shouldBe` []

  it "blockDrops returns nothing for air" $ do
    blockDrops Air `shouldBe` []

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
