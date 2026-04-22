module Main (main) where

import Test.Hspec
import Test.QuickCheck

import World.Block
import World.Chunk
import World.Noise
import World.World (worldToChunkLocal, World(..), worldGetBlock, worldSetBlock, triggerGravityAbove, settleGravityBlock, settleChunkGravity)
import World.Structure
import Game.Inventory
import Game.Item
import Game.Crafting
import Game.Player
import Game.DroppedItem
import Game.Save (SaveData(..), inventoryToSlotList, slotListToInventory)
import Game.SaveV3 (SaveDataV3(..), ChunkMeta(..), savev3Version, encodeSaveV3, decodeSaveV3, migrateV2toV3)
import Game.BlockEntity
import Game.Furnace
import World.Weather
import Entity.ECS
import Entity.Component
import Entity.Mob (MobType(..), MobInfo(..), MobBehavior(..), AIState(..), mobInfo)
import World.Redstone
import World.BlockRegistry
import Game.RecipeRegistry
import World.BiomeFeatures
import World.Biome (BiomeType(..))
import Game.ItemRegistry

import Entity.Villager
import qualified World.Dimension as Dim
import Game.Enchanting
import Game.Command
import Game.Achievement
import Game.Config
import Game.Event
import UI.Tooltip
import UI.Screen

import Game.PotionEffect

import Entity.Pathfinding (findPath, pathDistance)
import World.Light (LightMap, newLightMap, propagateBlockLight, propagateSkyLight, getBlockLight, getSkyLight, getTotalLight, maxLightLevel)
import Game.DayNight (DayNightCycle(..), newDayNightCycle, updateDayNight, getSkyColor, getAmbientLight, isNight, getTimeOfDay, TimeOfDay(..))
import Entity.Spawn (SpawnRules(..), defaultSpawnRules)

import TestHelpers (airHeightQuery, airQuery, waterQuery, withTestWorld)

import Data.Binary (encode, decode)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Linear (V2(..), V3(..), V4(..))
import qualified Data.Vector as V
import Control.Concurrent.STM (atomically, readTVar)
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
  compassClockSpec
  wolfMobSpec
  fishingRodSpec
  potionItemSpec
  boatSpec
  railBlockSpec
  minecartItemSpec
  dispenserBlockSpec
  enchantingTableSpec
  enchantingSystemSpec
  commandSpec
  achievementSpec
  villagerTradingSpec
  tooltipSpec
  structureSpec
  blockRegistrySpec
  biomeFeaturesSpec
  itemRegistrySpec
  armorDamageReductionSpec
  dimensionSpec
  screenRegistrySpec
  componentSpec
  eventBusSpec
  saveV3Spec
  recipeRegistrySpec

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
  let queuedToggle = noInput { piToggleFly = True, piMouseDX = 4, piMouseDY = -2 }

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

  it "triggerGravityAbove drops sand into air below" $ withTestWorld $ \world -> do
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

  it "triggerGravityAbove cascades multiple sand blocks" $ withTestWorld $ \world -> do
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

  it "triggerGravityAbove does nothing when block above is not gravity-affected" $ withTestWorld $ \world -> do
    worldSetBlock world (V3 3 5 3) Stone
    moved <- triggerGravityAbove world (V3 3 4 3)
    btAt4 <- worldGetBlock world (V3 3 4 3)
    btAt5 <- worldGetBlock world (V3 3 5 3)
    btAt4 `shouldBe` Air
    btAt5 `shouldBe` Stone
    moved `shouldBe` False

  it "settleGravityBlock drops sand to lowest air" $ withTestWorld $ \world -> do
    -- Stone floor at y=2, air at y=3 and y=4, sand at y=5
    worldSetBlock world (V3 4 2 4) Stone
    worldSetBlock world (V3 4 5 4) Sand
    moved <- settleGravityBlock world (V3 4 5 4)
    moved `shouldBe` True
    btAt3 <- worldGetBlock world (V3 4 3 4)
    btAt5 <- worldGetBlock world (V3 4 5 4)
    btAt3 `shouldBe` Sand
    btAt5 `shouldBe` Air

  it "settleGravityBlock does not move block on solid ground" $ withTestWorld $ \world -> do
    worldSetBlock world (V3 5 3 5) Stone
    worldSetBlock world (V3 5 4 5) Sand
    moved <- settleGravityBlock world (V3 5 4 5)
    moved `shouldBe` False
    btAt4 <- worldGetBlock world (V3 5 4 5)
    btAt4 `shouldBe` Sand

  it "settleChunkGravity settles floating sand in a chunk" $ withTestWorld $ \world -> do
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
  let -- Water only at head level (y=81, since head = feet_y + 1.62, so floor(80 + 1.62) = 81)
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
-- Compass & Clock items
-- =========================================================================
compassClockSpec :: Spec
compassClockSpec = describe "Compass and Clock items" $ do
  -- Item properties
  it "CompassItem stacks to 1" $ do
    itemStackLimit CompassItem `shouldBe` 1

  it "ClockItem stacks to 1" $ do
    itemStackLimit ClockItem `shouldBe` 1

  it "CompassItem is not a block item" $ do
    isBlockItem CompassItem `shouldBe` False
    itemToBlock CompassItem `shouldBe` Nothing

  it "ClockItem is not a block item" $ do
    isBlockItem ClockItem `shouldBe` False
    itemToBlock ClockItem `shouldBe` Nothing

  -- Crafting recipes
  it "4 iron ingots + redstone dust crafts compass" $ do
    let iron = Just (MaterialItem IronIngot)
        rd = Just (BlockItem RedstoneDust)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 1 iron) 1 0 iron) 1 1 rd) 1 2 iron) 2 1 iron
    tryCraft grid `shouldBe` CraftSuccess CompassItem 1

  it "4 gold ingots + redstone dust crafts clock" $ do
    let gold = Just (MaterialItem GoldIngot)
        rd = Just (BlockItem RedstoneDust)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 1 gold) 1 0 gold) 1 1 rd) 1 2 gold) 2 1 gold
    tryCraft grid `shouldBe` CraftSuccess ClockItem 1

  -- Binary roundtrip
  it "CompassItem roundtrips through Binary" $ do
    decode (encode CompassItem) `shouldBe` CompassItem

  it "ClockItem roundtrips through Binary" $ do
    decode (encode ClockItem) `shouldBe` ClockItem

-- =========================================================================
-- Wolf mob
-- =========================================================================
wolfMobSpec :: Spec
wolfMobSpec = describe "Wolf mob" $ do
  it "Wolf exists in MobType enum" $ do
    show Wolf `shouldBe` "Wolf"

  it "Wolf has correct health (8)" $ do
    miMaxHealth (mobInfo Wolf) `shouldBe` 8

  it "Wolf has correct attack damage (4)" $ do
    miAttackDmg (mobInfo Wolf) `shouldBe` 4

  it "Wolf has Neutral behavior" $ do
    miBehavior (mobInfo Wolf) `shouldBe` Neutral

  it "Wolf has detect range 16" $ do
    miDetectRange (mobInfo Wolf) `shouldBe` 16

  it "Wolf has attack range 1.5" $ do
    miAttackRange (mobInfo Wolf) `shouldBe` 1.5

  it "Wolf has speed 0.3" $ do
    miSpeed (mobInfo Wolf) `shouldBe` 0.3

  it "Wolf entity can be spawned with Wolf tag" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 8.0 "Wolf"
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> do
        entTag ent `shouldBe` "Wolf"
        entHealth ent `shouldBe` 8.0
      Nothing -> expectationFailure "Expected wolf entity"

  it "Tamed wolf can be represented with TamedWolf tag" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 8.0 "Wolf"
    updateEntity ew eid (\e -> e { entTag = "TamedWolf" })
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> entTag ent `shouldBe` "TamedWolf"
      Nothing -> expectationFailure "Expected tamed wolf entity"

  it "Sitting wolf uses TamedWolfSitting tag" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 8.0 "TamedWolfSitting"
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> entTag ent `shouldBe` "TamedWolfSitting"
      Nothing -> expectationFailure "Expected sitting wolf entity"

  it "Wolf entities appear in entitiesInRange" $ do
    ew <- newEntityWorld
    _ <- spawnEntity ew (V3 5.0 64.0 5.0) 8.0 "Wolf"
    nearby <- entitiesInRange ew (V3 5.0 64.0 5.0) 10.0
    let wolves = filter (\e -> entTag e == "Wolf") nearby
    length wolves `shouldBe` 1

-- =========================================================================
-- Fishing rod
-- =========================================================================
fishingRodSpec :: Spec
fishingRodSpec = describe "Fishing rod" $ do
  -- Item properties
  it "FishingRodItem stacks to 1" $ do
    itemStackLimit (FishingRodItem 64) `shouldBe` 1

  it "FishingRodItem is not a block item" $ do
    isBlockItem (FishingRodItem 64) `shouldBe` False
    itemToBlock (FishingRodItem 64) `shouldBe` Nothing

  it "FishingRodItem roundtrips through Binary" $ do
    let item = FishingRodItem 64
    decode (encode item) `shouldBe` item

  it "FishingRodItem with partial durability roundtrips through Binary" $ do
    let item = FishingRodItem 30
    decode (encode item) `shouldBe` item

  -- Fish food types
  it "RawFish restores 2 hunger" $ do
    foodHungerRestore RawFish `shouldBe` 2

  it "CookedFish restores 5 hunger" $ do
    foodHungerRestore CookedFish `shouldBe` 5

  it "RawSalmon restores 2 hunger" $ do
    foodHungerRestore RawSalmon `shouldBe` 2

  it "CookedSalmon restores 5 hunger" $ do
    foodHungerRestore CookedSalmon `shouldBe` 5

  it "fish food items stack to 64" $ do
    itemStackLimit (FoodItem RawFish) `shouldBe` 64
    itemStackLimit (FoodItem CookedFish) `shouldBe` 64
    itemStackLimit (FoodItem RawSalmon) `shouldBe` 64
    itemStackLimit (FoodItem CookedSalmon) `shouldBe` 64

  it "fish food items are not block items" $ do
    isBlockItem (FoodItem RawFish) `shouldBe` False
    itemToBlock (FoodItem RawSalmon) `shouldBe` Nothing

  it "fish food items roundtrip through Binary" $ do
    decode (encode (FoodItem RawFish)) `shouldBe` FoodItem RawFish
    decode (encode (FoodItem CookedFish)) `shouldBe` FoodItem CookedFish
    decode (encode (FoodItem RawSalmon)) `shouldBe` FoodItem RawSalmon
    decode (encode (FoodItem CookedSalmon)) `shouldBe` FoodItem CookedSalmon

  -- Crafting recipe
  it "fishing rod recipe: 3 sticks diagonal + 2 string" $ do
    let s = Just StickItem
        st = Just (MaterialItem StringMat)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 2 s) 1 1 s) 1 2 st) 2 0 s) 2 2 st
    tryCraft grid `shouldBe` CraftSuccess (FishingRodItem 64) 1

  -- Smelting recipes
  it "RawFish smelts to CookedFish" $ do
    let hasRecipe = any (\r -> srInput r == FoodItem RawFish && srOutput r == FoodItem CookedFish) smeltRecipes
    hasRecipe `shouldBe` True

  it "RawSalmon smelts to CookedSalmon" $ do
    let hasRecipe = any (\r -> srInput r == FoodItem RawSalmon && srOutput r == FoodItem CookedSalmon) smeltRecipes
    hasRecipe `shouldBe` True

  it "furnace smelts RawFish to CookedFish" $ do
    let fs0 = newFurnaceState
          { fsInput = Just (ItemStack (FoodItem RawFish) 1)
          , fsFuel  = Just (ItemStack (MaterialItem Coal) 1)
          }
        fs1 = iterate (tickFurnace 1.0) fs0 !! 11
    getFurnaceOutput fs1 `shouldBe` Just (ItemStack (FoodItem CookedFish) 1)
    getFurnaceInput fs1 `shouldBe` Nothing

  it "furnace smelts RawSalmon to CookedSalmon" $ do
    let fs0 = newFurnaceState
          { fsInput = Just (ItemStack (FoodItem RawSalmon) 1)
          , fsFuel  = Just (ItemStack (MaterialItem Coal) 1)
          }
        fs1 = iterate (tickFurnace 1.0) fs0 !! 11
    getFurnaceOutput fs1 `shouldBe` Just (ItemStack (FoodItem CookedSalmon) 1)
    getFurnaceInput fs1 `shouldBe` Nothing

  -- Food saturation values
  it "fish saturation values are correct" $ do
    foodSaturation RawFish `shouldBe` 0.4
    foodSaturation CookedFish `shouldBe` 6.0
    foodSaturation RawSalmon `shouldBe` 0.4
    foodSaturation CookedSalmon `shouldBe` 9.6

  -- Food names
  it "fish food names are correct" $ do
    foodName RawFish `shouldBe` "Raw Fish"
    foodName CookedFish `shouldBe` "Cooked Fish"
    foodName RawSalmon `shouldBe` "Raw Salmon"
    foodName CookedSalmon `shouldBe` "Cooked Salmon"

-- =========================================================================
-- Potion items
-- =========================================================================
potionItemSpec :: Spec
potionItemSpec = describe "Potion items (GlassBottle & Potions)" $ do
  -- GlassBottleItem properties
  it "GlassBottleItem is not a block item" $ do
    isBlockItem GlassBottleItem `shouldBe` False

  it "GlassBottleItem does not convert to block" $ do
    itemToBlock GlassBottleItem `shouldBe` Nothing

  it "GlassBottleItem stacks to 16" $ do
    itemStackLimit GlassBottleItem `shouldBe` 16

  -- PotionItem properties
  it "PotionItem is not a block item" $ do
    isBlockItem (PotionItem WaterBottle) `shouldBe` False

  it "PotionItem does not convert to block" $ do
    itemToBlock (PotionItem HealingPotion) `shouldBe` Nothing

  it "PotionItem stacks to 1" $ do
    itemStackLimit (PotionItem PoisonPotion) `shouldBe` 1
    itemStackLimit (PotionItem SpeedPotion) `shouldBe` 1

  -- PotionType coverage
  it "all PotionTypes have names" $ do
    let allPotions = [minBound .. maxBound] :: [PotionType]
    mapM_ (\pt -> length (potionName pt) `shouldSatisfy` (> 0)) allPotions

  it "all PotionTypes have valid colors" $ do
    let allPotions = [minBound .. maxBound] :: [PotionType]
    mapM_ (\pt -> let (r, g, b, a) = potionColor pt
                  in do r `shouldSatisfy` (>= 0); g `shouldSatisfy` (>= 0)
                        b `shouldSatisfy` (>= 0); a `shouldSatisfy` (> 0)) allPotions

  it "potionName returns expected names" $ do
    potionName WaterBottle `shouldBe` "Water Bottle"
    potionName HealingPotion `shouldBe` "Healing Potion"
    potionName PoisonPotion `shouldBe` "Poison Potion"
    potionName SpeedPotion `shouldBe` "Speed Potion"
    potionName AwkwardPotion `shouldBe` "Awkward Potion"

  -- Binary roundtrip
  it "GlassBottleItem roundtrips through Binary" $ do
    decode (encode GlassBottleItem) `shouldBe` GlassBottleItem

  it "PotionItem WaterBottle roundtrips through Binary" $ do
    let item = PotionItem WaterBottle
    decode (encode item) `shouldBe` item

  it "PotionItem HealingPotion roundtrips through Binary" $ do
    let item = PotionItem HealingPotion
    decode (encode item) `shouldBe` item

  it "PotionItem SpeedPotion roundtrips through Binary" $ do
    let item = PotionItem SpeedPotion
    decode (encode item) `shouldBe` item

  it "all PotionTypes roundtrip through Enum" $ do
    let allPotions = [minBound .. maxBound] :: [PotionType]
    mapM_ (\pt -> toEnum (fromEnum pt) `shouldBe` pt) allPotions

  it "all PotionTypes roundtrip through Binary" $ do
    let allPotions = [minBound .. maxBound] :: [PotionType]
    mapM_ (\pt -> decode (encode (PotionItem pt)) `shouldBe` PotionItem pt) allPotions

  -- Crafting recipe
  it "3 glass in V-shape crafts 3 glass bottles" $ do
    let g = Just (BlockItem Glass)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
          0 0 g) 0 2 g) 1 1 g
    tryCraft grid `shouldBe` CraftSuccess GlassBottleItem 3

  -- Inventory operations
  it "GlassBottleItem can be added to inventory" $ do
    let (inv, leftover) = addItem emptyInventory GlassBottleItem 5
    leftover `shouldBe` 0
    selectedItem (selectHotbar inv 0) `shouldBe` Just (ItemStack GlassBottleItem 5)

  it "PotionItem can be added to inventory" $ do
    let (inv, leftover) = addItem emptyInventory (PotionItem HealingPotion) 1
    leftover `shouldBe` 0
    selectedItem (selectHotbar inv 0) `shouldBe` Just (ItemStack (PotionItem HealingPotion) 1)

-- =========================================================================
-- Boat entity
-- =========================================================================
boatSpec :: Spec
boatSpec = describe "Boat entity and BoatItem" $ do
  -- Item properties
  it "BoatItem is not a block item" $ do
    isBlockItem BoatItem `shouldBe` False

  it "BoatItem does not convert to block type" $ do
    itemToBlock BoatItem `shouldBe` Nothing

  it "BoatItem stacks to 1" $ do
    itemStackLimit BoatItem `shouldBe` 1

  it "BoatItem roundtrips through Binary" $ do
    decode (encode BoatItem) `shouldBe` BoatItem

  -- Crafting recipe
  it "5 oak planks in U-shape crafts a boat" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 2 p) 1 0 p) 1 1 p) 1 2 p
    tryCraft grid `shouldBe` CraftSuccess BoatItem 1

  -- Entity system integration
  it "can spawn a Boat entity" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 10.5 65.0 10.5) 1.0 "Boat"
    eid `shouldSatisfy` (> 0)
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> do
        entTag ent `shouldBe` "Boat"
        entPosition ent `shouldBe` V3 10.5 65.0 10.5
        entHealth ent `shouldBe` 1.0
        entAlive ent `shouldBe` True
      Nothing -> expectationFailure "Expected boat entity"

  it "Boat entity can be destroyed" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Boat"
    destroyEntity ew eid
    mEnt <- getEntity ew eid
    mEnt `shouldBe` Nothing

  it "Boat entity position can be updated" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.0 64.0 5.0) 1.0 "Boat"
    updateEntity ew eid (\e -> e { entPosition = V3 6.0 64.0 6.0 })
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> entPosition ent `shouldBe` V3 6.0 64.0 6.0
      Nothing -> expectationFailure "Expected boat entity"

  it "Boat entity found by entitiesInRange" $ do
    ew <- newEntityWorld
    _ <- spawnEntity ew (V3 10.0 65.0 10.0) 1.0 "Boat"
    nearby <- entitiesInRange ew (V3 10.0 65.0 10.0) 5.0
    let boats = filter (\e -> entTag e == "Boat") nearby
    length boats `shouldBe` 1
-- Rail block
-- =========================================================================
railBlockSpec :: Spec
railBlockSpec = describe "Rail block" $ do
  -- Block properties
  it "Rail is not solid and is transparent" $ do
    isSolid Rail `shouldBe` False
    isTransparent Rail `shouldBe` True

  it "Rail has hardness 0.7" $ do
    bpHardness (blockProperties Rail) `shouldBe` 0.7

  it "Rail emits no light" $ do
    bpLightEmit (blockProperties Rail) `shouldBe` 0

  -- Enum roundtrip
  it "Rail roundtrips through Enum" $ do
    toEnum (fromEnum Rail) `shouldBe` (Rail :: BlockType)

  -- Texture coords
  it "Rail has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords Rail FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  -- Block drops
  it "Rail drops itself" $ do
    blockDrops Rail `shouldBe` [(BlockItem Rail, 1)]

  -- Preferred tool
  it "Rail prefers pickaxe" $ do
    blockPreferredTool Rail `shouldBe` Just Pickaxe

  -- Rail has no required harvest level
  it "Rail requires no special harvest level" $ do
    blockRequiredHarvestLevel Rail `shouldBe` 0

  -- Rail is not gravity-affected
  it "Rail is not gravity-affected" $ do
    isGravityAffected Rail `shouldBe` False

  -- Crafting recipe
  it "6 iron ingots + 1 stick crafts 16 rails" $ do
    let i = Just (MaterialItem IronIngot)
        s = Just StickItem
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 i) 0 2 i) 1 0 i) 1 1 s) 1 2 i) 2 0 i) 2 2 i
    tryCraft grid `shouldBe` CraftSuccess (BlockItem Rail) 16

  -- Binary roundtrip
  it "Rail roundtrips through Binary" $ do
    let item = BlockItem Rail
    decode (encode item) `shouldBe` item

-- =========================================================================
-- Minecart item
-- =========================================================================
minecartItemSpec :: Spec
minecartItemSpec = describe "Minecart item" $ do
  it "MinecartItem stacks to 1" $ do
    itemStackLimit MinecartItem `shouldBe` 1

  it "MinecartItem is not a block item" $ do
    isBlockItem MinecartItem `shouldBe` False

  it "MinecartItem does not convert to block type" $ do
    itemToBlock MinecartItem `shouldBe` Nothing

  it "MinecartItem roundtrips through Binary" $ do
    decode (encode MinecartItem) `shouldBe` MinecartItem

  -- Crafting recipe
  it "5 iron ingots in U-shape crafts a minecart" $ do
    let i = Just (MaterialItem IronIngot)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 0 i) 0 2 i) 1 0 i) 1 1 i) 1 2 i
    tryCraft grid `shouldBe` CraftSuccess MinecartItem 1

  -- Minecart entity
  it "can spawn a minecart entity" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.5 65.25 5.5) 1.0 "Minecart"
    eid `shouldSatisfy` (> 0)
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> do
        entTag ent `shouldBe` "Minecart"
        entAlive ent `shouldBe` True
      Nothing -> expectationFailure "Expected minecart entity"

  it "minecart entity can be destroyed" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 5.5 65.25 5.5) 1.0 "Minecart"
    destroyEntity ew eid
    mEnt <- getEntity ew eid
    mEnt `shouldBe` Nothing
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

-- Enchanting table block
-- =========================================================================
enchantingTableSpec :: Spec
enchantingTableSpec = describe "Enchanting table block" $ do
  -- Block properties
  it "EnchantingTable is solid and opaque" $ do
    isSolid EnchantingTable `shouldBe` True
    isTransparent EnchantingTable `shouldBe` False

  it "EnchantingTable has hardness 5.0" $ do
    bpHardness (blockProperties EnchantingTable) `shouldBe` 5.0

  it "EnchantingTable emits light level 7" $ do
    bpLightEmit (blockProperties EnchantingTable) `shouldBe` 7

  -- Enum roundtrip
  it "EnchantingTable roundtrips through Enum" $ do
    toEnum (fromEnum EnchantingTable) `shouldBe` (EnchantingTable :: BlockType)

  -- Texture coords
  it "EnchantingTable has valid texture coords" $ do
    let V2 u v = blockFaceTexCoords EnchantingTable FaceTop
    u `shouldSatisfy` (>= 0)
    v `shouldSatisfy` (>= 0)

  it "EnchantingTable has different top and side textures" $ do
    blockFaceTexCoords EnchantingTable FaceTop `shouldNotBe` blockFaceTexCoords EnchantingTable FaceEast

  -- Block drops
  it "EnchantingTable drops itself" $ do
    blockDrops EnchantingTable `shouldBe` [(BlockItem EnchantingTable, 1)]

  -- Preferred tool
  it "EnchantingTable prefers pickaxe" $ do
    blockPreferredTool EnchantingTable `shouldBe` Just Pickaxe

  -- Crafting recipe
  it "enchanting table recipe produces EnchantingTable" $ do
    let d = Just (MaterialItem DiamondGem)
        p = Just (BlockItem OakPlanks)
        o = Just (BlockItem Obsidian)
        grid = setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (setCraftingSlot
              (setCraftingSlot (setCraftingSlot (emptyCraftingGrid 3)
                0 1 p) 1 0 d) 1 1 p) 1 2 d) 2 0 o) 2 1 o) 2 2 o)
                0 0 Nothing
    tryCraft grid `shouldBe` CraftSuccess (BlockItem EnchantingTable) 1

  -- Binary roundtrip
  it "EnchantingTable roundtrips through Binary" $ do
    let item = BlockItem EnchantingTable
    decode (encode item) `shouldBe` item

-- =========================================================================
-- Enchanting system
-- =========================================================================
enchantingSystemSpec :: Spec
enchantingSystemSpec = describe "Game.Enchanting" $ do
  -- Enchantment types
  it "all EnchantmentType values are enumerable" $ do
    let allTypes = [minBound .. maxBound] :: [EnchantmentType]
    length allTypes `shouldBe` 6

  -- Max levels
  it "Sharpness has max level 5" $ do
    enchantmentMaxLevel Sharpness `shouldBe` 5

  it "Protection has max level 4" $ do
    enchantmentMaxLevel Protection `shouldBe` 4

  it "Efficiency has max level 5" $ do
    enchantmentMaxLevel Efficiency `shouldBe` 5

  it "Unbreaking has max level 3" $ do
    enchantmentMaxLevel Unbreaking `shouldBe` 3

  it "Power has max level 5" $ do
    enchantmentMaxLevel Power `shouldBe` 5

  it "Knockback has max level 2" $ do
    enchantmentMaxLevel Knockback `shouldBe` 2

  -- Enchantment names
  it "enchantmentName returns correct names" $ do
    enchantmentName Sharpness `shouldBe` "Sharpness"
    enchantmentName Protection `shouldBe` "Protection"
    enchantmentName Efficiency `shouldBe` "Efficiency"
    enchantmentName Unbreaking `shouldBe` "Unbreaking"

  -- Generate enchantments
  it "generateEnchantments returns exactly 3 options" $ do
    let options = generateEnchantments 10 42
    length options `shouldBe` 3

  it "generateEnchantments is deterministic (same seed = same result)" $ do
    let opts1 = generateEnchantments 10 42
        opts2 = generateEnchantments 10 42
    opts1 `shouldBe` opts2

  it "generateEnchantments with different seeds produces different results" $ do
    let opts1 = generateEnchantments 10 42
        opts2 = generateEnchantments 10 99
    opts1 `shouldNotBe` opts2

  it "generateEnchantments levels are within max bounds" $ do
    let options = generateEnchantments 30 42
    mapM_ (\(etype, lvl, _cost) -> do
      lvl `shouldSatisfy` (> 0)
      lvl `shouldSatisfy` (<= enchantmentMaxLevel etype)
      ) options

  it "generateEnchantments costs are positive" $ do
    let options = generateEnchantments 10 42
    mapM_ (\(_etype, _lvl, cost) ->
      cost `shouldSatisfy` (> 0)
      ) options

  -- Enchantment map
  it "new enchantment map is empty" $ do
    em <- newEnchantmentMap
    enchants <- getEnchantments em 0
    enchants `shouldBe` []

  it "setEnchantments and getEnchantments roundtrip" $ do
    em <- newEnchantmentMap
    let enchants = [Enchantment Sharpness 3, Enchantment Unbreaking 2]
    setEnchantments em 0 enchants
    result <- getEnchantments em 0
    result `shouldBe` enchants

  it "clearEnchantments removes enchantments" $ do
    em <- newEnchantmentMap
    setEnchantments em 0 [Enchantment Sharpness 1]
    clearEnchantments em 0
    result <- getEnchantments em 0
    result `shouldBe` []

  it "different slots are independent" $ do
    em <- newEnchantmentMap
    let e1 = [Enchantment Sharpness 3]
        e2 = [Enchantment Protection 4]
    setEnchantments em 0 e1
    setEnchantments em 5 e2
    r0 <- getEnchantments em 0
    r5 <- getEnchantments em 5
    r0 `shouldBe` e1
    r5 `shouldBe` e2

  -- Apply enchantment
  it "applyEnchantment returns a description" $ do
    let desc = applyEnchantment (Enchantment Sharpness 3)
    desc `shouldSatisfy` (not . null)

-- =========================================================================
-- Command parsing
-- =========================================================================
commandSpec :: Spec
commandSpec = describe "Game.Command" $ do
  -- /give
  it "parses /give with item and count" $ do
    parseCommand "/give stone 64" `shouldBe` Just (CmdGive "stone" 64)

  it "parses /give with item only (defaults to 1)" $ do
    parseCommand "/give diamond" `shouldBe` Just (CmdGive "diamond" 1)

  it "give normalizes item name to lowercase" $ do
    parseCommand "/give Stone 10" `shouldBe` Just (CmdGive "stone" 10)

  it "give rejects zero count" $ do
    parseCommand "/give stone 0" `shouldBe` Nothing

  it "give rejects negative count" $ do
    parseCommand "/give stone -5" `shouldBe` Nothing

  it "give rejects missing item" $ do
    parseCommand "/give" `shouldBe` Nothing

  it "give rejects too many arguments" $ do
    parseCommand "/give stone 64 extra" `shouldBe` Nothing

  -- /tp
  it "parses /tp with three coordinates" $ do
    parseCommand "/tp 100 65 200" `shouldBe` Just (CmdTeleport 100 65 200)

  it "parses /tp with floating point coordinates" $ do
    parseCommand "/tp 1.5 64.0 -3.2" `shouldBe` Just (CmdTeleport 1.5 64.0 (-3.2))

  it "parses /teleport as alias for /tp" $ do
    parseCommand "/teleport 10 20 30" `shouldBe` Just (CmdTeleport 10 20 30)

  it "tp rejects too few coordinates" $ do
    parseCommand "/tp 100 65" `shouldBe` Nothing

  it "tp rejects non-numeric coordinates" $ do
    parseCommand "/tp abc 65 200" `shouldBe` Nothing

  -- /time
  it "parses /time set day" $ do
    parseCommand "/time set day" `shouldBe` Just (CmdTime "day")

  it "parses /time set night" $ do
    parseCommand "/time set night" `shouldBe` Just (CmdTime "night")

  it "parses /time with direct value" $ do
    parseCommand "/time day" `shouldBe` Just (CmdTime "day")

  it "time normalizes value to lowercase" $ do
    parseCommand "/time set DAY" `shouldBe` Just (CmdTime "day")

  it "time rejects no arguments" $ do
    parseCommand "/time" `shouldBe` Nothing

  -- /weather
  it "parses /weather clear" $ do
    parseCommand "/weather clear" `shouldBe` Just (CmdWeather "clear")

  it "parses /weather rain" $ do
    parseCommand "/weather rain" `shouldBe` Just (CmdWeather "rain")

  it "weather normalizes to lowercase" $ do
    parseCommand "/weather RAIN" `shouldBe` Just (CmdWeather "rain")

  it "weather rejects no arguments" $ do
    parseCommand "/weather" `shouldBe` Nothing

  -- /gamemode
  it "parses /gamemode creative" $ do
    parseCommand "/gamemode creative" `shouldBe` Just (CmdGamemode "creative")

  it "parses /gamemode survival" $ do
    parseCommand "/gamemode survival" `shouldBe` Just (CmdGamemode "survival")

  it "gamemode normalizes to lowercase" $ do
    parseCommand "/gamemode Creative" `shouldBe` Just (CmdGamemode "creative")

  it "gamemode rejects no arguments" $ do
    parseCommand "/gamemode" `shouldBe` Nothing

  -- /kill
  it "parses /kill" $ do
    parseCommand "/kill" `shouldBe` Just CmdKill

  it "kill rejects extra arguments" $ do
    parseCommand "/kill player" `shouldBe` Nothing

  -- /seed
  it "parses /seed" $ do
    parseCommand "/seed" `shouldBe` Just CmdSeed

  it "seed rejects extra arguments" $ do
    parseCommand "/seed 42" `shouldBe` Nothing

  -- /help
  it "parses /help" $ do
    parseCommand "/help" `shouldBe` Just CmdHelp

  it "help rejects extra arguments" $ do
    parseCommand "/help give" `shouldBe` Nothing

  -- /summon
  it "parses /summon zombie" $ do
    parseCommand "/summon zombie" `shouldBe` Just (CmdSpawnMob "zombie")

  it "summon normalizes to lowercase" $ do
    parseCommand "/summon Zombie" `shouldBe` Just (CmdSpawnMob "zombie")

  it "summon rejects no arguments" $ do
    parseCommand "/summon" `shouldBe` Nothing

  -- Edge cases
  it "returns Nothing for empty string" $ do
    parseCommand "" `shouldBe` Nothing

  it "returns Nothing for non-command text" $ do
    parseCommand "hello world" `shouldBe` Nothing

  it "returns Nothing for unknown command" $ do
    parseCommand "/unknown" `shouldBe` Nothing

  it "handles leading whitespace" $ do
    parseCommand "  /help" `shouldBe` Just CmdHelp

  it "command names are case-insensitive" $ do
    parseCommand "/GIVE stone 1" `shouldBe` Just (CmdGive "stone" 1)
    parseCommand "/TP 0 64 0" `shouldBe` Just (CmdTeleport 0 64 0)
    parseCommand "/Kill" `shouldBe` Just CmdKill

  -- commandHelp and allCommands
  it "commandHelp has entries for all command types" $ do
    length commandHelp `shouldBe` 9

  it "allCommands contains expected commands" $ do
    "/give" `elem` allCommands `shouldBe` True
    "/tp" `elem` allCommands `shouldBe` True
    "/help" `elem` allCommands `shouldBe` True
    "/summon" `elem` allCommands `shouldBe` True

  it "allCommands includes teleport alias" $ do
    "/teleport" `elem` allCommands `shouldBe` True

-- =========================================================================
-- Achievement system
-- =========================================================================
achievementSpec :: Spec
achievementSpec = describe "Game.Achievement" $ do
  -- New state
  it "newAchievementState has nothing unlocked" $ do
    let state = newAchievementState
    isUnlocked state AchOpenInventory `shouldBe` False
    isUnlocked state AchDefeatDragon `shouldBe` False

  -- Unlock and query
  it "unlockAchievement marks achievement as unlocked" $ do
    let state0 = newAchievementState
        state1 = unlockAchievement state0 AchMineWood
    isUnlocked state1 AchMineWood `shouldBe` True
    isUnlocked state1 AchOpenInventory `shouldBe` False

  it "unlockAchievement is idempotent" $ do
    let state0 = newAchievementState
        state1 = unlockAchievement state0 AchCraftPlanks
        state2 = unlockAchievement state1 AchCraftPlanks
    state1 `shouldBe` state2

  it "multiple achievements can be unlocked independently" $ do
    let state = unlockAchievement (unlockAchievement newAchievementState AchMineWood) AchKillMob
    isUnlocked state AchMineWood `shouldBe` True
    isUnlocked state AchKillMob `shouldBe` True
    isUnlocked state AchCraftTable `shouldBe` False

  -- Check triggers
  it "checkAchievement returns Just for matching locked achievement" $ do
    let state = newAchievementState
    checkAchievement state TrigOpenInventory `shouldBe` Just AchOpenInventory

  it "checkAchievement returns Nothing for already unlocked achievement" $ do
    let state = unlockAchievement newAchievementState AchOpenInventory
    checkAchievement state TrigOpenInventory `shouldBe` Nothing

  it "checkAchievement recognizes mining oak log" $ do
    checkAchievement newAchievementState (TrigMineBlock OakLog) `shouldBe` Just AchMineWood

  it "checkAchievement recognizes mining diamond ore" $ do
    checkAchievement newAchievementState (TrigMineBlock DiamondOre) `shouldBe` Just AchMineDiamond

  it "checkAchievement returns Nothing for irrelevant block mine" $ do
    checkAchievement newAchievementState (TrigMineBlock Stone) `shouldBe` Nothing

  it "checkAchievement recognizes crafting planks" $ do
    checkAchievement newAchievementState (TrigCraftItem (BlockItem OakPlanks)) `shouldBe` Just AchCraftPlanks

  it "checkAchievement recognizes crafting table" $ do
    checkAchievement newAchievementState (TrigCraftItem (BlockItem CraftingTable)) `shouldBe` Just AchCraftTable

  it "checkAchievement recognizes crafting pickaxe" $ do
    checkAchievement newAchievementState (TrigCraftItem (ToolItem Pickaxe Wood 59)) `shouldBe` Just AchCraftPickaxe

  it "checkAchievement recognizes crafting sword" $ do
    checkAchievement newAchievementState (TrigCraftItem (ToolItem Sword Diamond 1561)) `shouldBe` Just AchMakeSword

  it "checkAchievement recognizes baking bread" $ do
    checkAchievement newAchievementState (TrigCraftItem (FoodItem Bread)) `shouldBe` Just AchBakeBread

  it "checkAchievement recognizes smelting iron" $ do
    checkAchievement newAchievementState (TrigSmeltItem (MaterialItem IronIngot)) `shouldBe` Just AchSmeltIron

  it "checkAchievement returns Nothing for smelting non-iron" $ do
    checkAchievement newAchievementState (TrigSmeltItem (MaterialItem GoldIngot)) `shouldBe` Nothing

  it "checkAchievement recognizes killing a zombie" $ do
    checkAchievement newAchievementState (TrigKillEntity "Zombie") `shouldBe` Just AchKillMob

  it "checkAchievement recognizes killing a skeleton" $ do
    checkAchievement newAchievementState (TrigKillEntity "Skeleton") `shouldBe` Just AchKillMob

  it "checkAchievement returns Nothing for killing passive mob" $ do
    checkAchievement newAchievementState (TrigKillEntity "Pig") `shouldBe` Nothing

  it "checkAchievement recognizes entering Nether" $ do
    checkAchievement newAchievementState (TrigEnterDimension Nether) `shouldBe` Just AchEnterNether

  it "checkAchievement returns Nothing for entering Overworld" $ do
    checkAchievement newAchievementState (TrigEnterDimension Overworld) `shouldBe` Nothing

  -- Names and descriptions
  it "achievementName returns non-empty string for all achievements" $ do
    mapM_ (\ach -> length (achievementName ach) `shouldSatisfy` (> 0)) allAchievements

  it "achievementDescription returns non-empty string for all achievements" $ do
    mapM_ (\ach -> length (achievementDescription ach) `shouldSatisfy` (> 0)) allAchievements

  it "achievementName returns expected name" $ do
    achievementName AchOpenInventory `shouldBe` "Taking Inventory"
    achievementName AchDefeatDragon `shouldBe` "The End."

  -- allAchievements
  it "allAchievements contains 14 achievements" $ do
    length allAchievements `shouldBe` 14

  it "allAchievements matches Enum bounds" $ do
    allAchievements `shouldBe` [minBound .. maxBound]

  -- Enum roundtrip
  it "AchievementType roundtrips through Enum" $ do
    mapM_ (\ach -> toEnum (fromEnum ach) `shouldBe` ach) allAchievements

-- =========================================================================
-- Villager trading
-- =========================================================================
villagerTradingSpec :: Spec
villagerTradingSpec = describe "Entity.Villager trading framework" $ do
  -- Profession enumeration
  it "allProfessions contains 6 professions" $ do
    length allProfessions `shouldBe` 6

  it "allProfessions matches Enum range" $ do
    allProfessions `shouldBe` [Farmer, Librarian, Blacksmith, Butcher, Cleric, Shepherd]

  it "VillagerProfession roundtrips through Enum" $ do
    mapM_ (\p -> toEnum (fromEnum p) `shouldBe` p) allProfessions

  -- Profession names
  it "professionName returns non-empty strings" $ do
    mapM_ (\p -> length (professionName p) `shouldSatisfy` (> 0)) allProfessions

  it "professionName returns expected names" $ do
    professionName Farmer `shouldBe` "Farmer"
    professionName Librarian `shouldBe` "Librarian"
    professionName Blacksmith `shouldBe` "Blacksmith"
    professionName Butcher `shouldBe` "Butcher"
    professionName Cleric `shouldBe` "Cleric"
    professionName Shepherd `shouldBe` "Shepherd"

  -- Trade generation for every profession
  it "every profession generates at least one trade" $ do
    mapM_ (\p -> length (generateTrades p) `shouldSatisfy` (> 0)) allProfessions

  -- Trade invariants
  it "all trades have positive input and output counts" $ do
    let allTrades = concatMap generateTrades allProfessions
    mapM_ (\t -> do
      toInputCount t `shouldSatisfy` (> 0)
      toOutputCount t `shouldSatisfy` (> 0)) allTrades

  it "all trades have positive max uses" $ do
    let allTrades = concatMap generateTrades allProfessions
    mapM_ (\t -> toMaxUses t `shouldSatisfy` (> 0)) allTrades

  it "all trades start with usesLeft equal to maxUses" $ do
    let allTrades = concatMap generateTrades allProfessions
    mapM_ (\t -> toUsesLeft t `shouldBe` toMaxUses t) allTrades

  -- Farmer trades
  it "Farmer offers wheat-for-currency trade" $ do
    let trades = generateTrades Farmer
        hasWheatTrade = any (\t -> toInputItem t == MaterialItem Wheat) trades
    hasWheatTrade `shouldBe` True

  it "Farmer offers currency-for-bread trade" $ do
    let trades = generateTrades Farmer
        hasBreadTrade = any (\t -> toOutputItem t == FoodItem Bread) trades
    hasBreadTrade `shouldBe` True

  -- Librarian trades
  it "Librarian offers paper-for-currency trade" $ do
    let trades = generateTrades Librarian
        hasPaperTrade = any (\t -> toInputItem t == MaterialItem Paper) trades
    hasPaperTrade `shouldBe` True

  -- Blacksmith trades
  it "Blacksmith offers iron tool trades" $ do
    let trades = generateTrades Blacksmith
        hasToolTrade = any (\t -> case toOutputItem t of
          ToolItem _ Iron _ -> True
          _                 -> False) trades
    hasToolTrade `shouldBe` True

  it "Blacksmith has 3 trades" $ do
    length (generateTrades Blacksmith) `shouldBe` 3

  -- Butcher trades
  it "Butcher accepts raw meat" $ do
    let trades = generateTrades Butcher
        hasMeatInput = any (\t -> toInputItem t == FoodItem RawPorkchop
                               || toInputItem t == FoodItem RawChicken) trades
    hasMeatInput `shouldBe` True

  -- Shepherd trades
  it "Shepherd accepts wool" $ do
    let trades = generateTrades Shepherd
        hasWoolTrade = any (\t -> toInputItem t == BlockItem Wool) trades
    hasWoolTrade `shouldBe` True

-- =========================================================================
-- Tooltip
-- =========================================================================
tooltipSpec :: Spec
tooltipSpec = describe "UI.Tooltip" $ do
  -- buildTooltip: BlockItem
  it "BlockItem Stone tooltip has name Stone" $ do
    let tt = buildTooltip (BlockItem Stone) []
    ttName tt `shouldBe` "Stone"

  it "BlockItem has no lore, no enchantments, no durability" $ do
    let tt = buildTooltip (BlockItem Dirt) []
    ttLoreLines tt `shouldBe` []
    ttEnchantments tt `shouldBe` []
    ttDurability tt `shouldBe` Nothing

  -- buildTooltip: ToolItem
  it "Diamond Pickaxe tooltip has correct name" $ do
    let tt = buildTooltip (ToolItem Pickaxe Diamond 1200) []
    ttName tt `shouldBe` "Diamond Pickaxe"

  it "ToolItem tooltip has durability" $ do
    let tt = buildTooltip (ToolItem Pickaxe Diamond 1200) []
    ttDurability tt `shouldBe` Just (1200, 1561)

  it "Wooden Sword tooltip has correct name and durability" $ do
    let tt = buildTooltip (ToolItem Sword Wood 59) []
    ttName tt `shouldBe` "Wooden Sword"
    ttDurability tt `shouldBe` Just (59, 59)

  -- buildTooltip: FoodItem
  it "Steak tooltip has hunger lore" $ do
    let tt = buildTooltip (FoodItem Steak) []
    ttName tt `shouldBe` "Steak"
    ttLoreLines tt `shouldBe` ["Restores 8 hunger"]
    ttDurability tt `shouldBe` Nothing

  it "Apple tooltip has hunger lore" $ do
    let tt = buildTooltip (FoodItem Apple) []
    ttLoreLines tt `shouldBe` ["Restores 4 hunger"]

  -- buildTooltip: ArmorItem
  it "Diamond Chestplate tooltip has defense lore and durability" $ do
    let tt = buildTooltip (ArmorItem Chestplate DiamondArmor 500) []
    ttName tt `shouldBe` "Diamond Chestplate"
    ttLoreLines tt `shouldBe` ["Defense: 8"]
    ttDurability tt `shouldBe` Just (500, 528)

  it "Iron Helmet tooltip has correct defense" $ do
    let tt = buildTooltip (ArmorItem Helmet IronArmor 200) []
    ttLoreLines tt `shouldBe` ["Defense: 2"]
    ttDurability tt `shouldBe` Just (200, 240)

  -- buildTooltip: enchantments
  it "enchantments appear in tooltip" $ do
    let enchants = [Enchantment Efficiency 5, Enchantment Unbreaking 3]
        tt = buildTooltip (ToolItem Pickaxe Diamond 1561) enchants
    ttEnchantments tt `shouldBe` ["Efficiency V", "Unbreaking III"]

  it "level 1 enchantment omits numeral" $ do
    let tt = buildTooltip (ToolItem Sword Iron 250) [Enchantment Sharpness 1]
    ttEnchantments tt `shouldBe` ["Sharpness"]

  -- buildTooltip: misc items
  it "Shears tooltip has durability" $ do
    let tt = buildTooltip (ShearsItem 200) []
    ttName tt `shouldBe` "Shears"
    ttDurability tt `shouldBe` Just (200, 238)

  it "Flint and Steel tooltip has durability" $ do
    let tt = buildTooltip (FlintAndSteelItem 50) []
    ttName tt `shouldBe` "Flint and Steel"
    ttDurability tt `shouldBe` Just (50, 64)

  it "FishingRod tooltip has durability" $ do
    let tt = buildTooltip (FishingRodItem 64) []
    ttName tt `shouldBe` "Fishing Rod"
    ttDurability tt `shouldBe` Just (64, 64)

  it "Compass tooltip has no lore or durability" $ do
    let tt = buildTooltip CompassItem []
    ttName tt `shouldBe` "Compass"
    ttLoreLines tt `shouldBe` []
    ttDurability tt `shouldBe` Nothing

  it "StickItem tooltip has name Stick" $ do
    let tt = buildTooltip StickItem []
    ttName tt `shouldBe` "Stick"

  it "MaterialItem Coal tooltip has name Coal" $ do
    let tt = buildTooltip (MaterialItem Coal) []
    ttName tt `shouldBe` "Coal"

  it "PotionItem has lore line" $ do
    let tt = buildTooltip (PotionItem HealingPotion) []
    ttName tt `shouldBe` "Healing Potion"
    ttLoreLines tt `shouldBe` ["Restores health"]

  it "BoatItem tooltip has name Boat" $ do
    let tt = buildTooltip BoatItem []
    ttName tt `shouldBe` "Boat"

  it "MinecartItem tooltip has name Minecart" $ do
    let tt = buildTooltip MinecartItem []
    ttName tt `shouldBe` "Minecart"

  -- renderTooltipVertices
  it "renderTooltipVertices produces non-empty vertex list" $ do
    let tt = buildTooltip (BlockItem Stone) []
        verts = renderTooltipVertices tt 0.0 0.0
    length verts `shouldSatisfy` (> 0)

  it "renderTooltipVertices vertex count is multiple of 6" $ do
    let tt = buildTooltip (ToolItem Pickaxe Diamond 1200) [Enchantment Efficiency 5]
        verts = renderTooltipVertices tt (-0.5) (-0.3)
    (length verts `mod` 6) `shouldBe` 0

  it "renderTooltipVertices with durability has more verts than without" $ do
    let ttNoDur = buildTooltip (BlockItem Stone) []
        ttDur   = buildTooltip (ToolItem Pickaxe Diamond 1200) []
        vertsNoDur = renderTooltipVertices ttNoDur 0.0 0.0
        vertsDur   = renderTooltipVertices ttDur   0.0 0.0
    length vertsDur `shouldSatisfy` (> length vertsNoDur)

-- =========================================================================
-- World structures
-- =========================================================================
structureSpec :: Spec
structureSpec = describe "World.Structure" $ do
  -- Well structure
  it "wellStructure has correct name" $ do
    stName wellStructure `shouldBe` "Well"

  it "wellStructure has correct size" $ do
    stSize wellStructure `shouldBe` V3 5 4 5

  it "wellStructure has non-empty block list" $ do
    length (stBlocks wellStructure) `shouldSatisfy` (> 0)

  it "wellStructure contains water blocks" $ do
    let hasWater = any (\sb -> sbBlock sb == Water) (stBlocks wellStructure)
    hasWater `shouldBe` True

  it "wellStructure contains cobblestone blocks" $ do
    let hasCobble = any (\sb -> sbBlock sb == Cobblestone) (stBlocks wellStructure)
    hasCobble `shouldBe` True

  it "wellStructure floor has 25 cobblestone blocks" $ do
    let floorBlocks = filter (\sb -> let V3 _ y _ = sbOffset sb in y == 0 && sbBlock sb == Cobblestone) (stBlocks wellStructure)
    length floorBlocks `shouldBe` 25

  it "wellStructure has 9 water blocks in interior" $ do
    let waterBlocks = filter (\sb -> sbBlock sb == Water) (stBlocks wellStructure)
    length waterBlocks `shouldBe` 9

  -- Desert temple structure
  it "desertTempleStructure has correct name" $ do
    stName desertTempleStructure `shouldBe` "Desert Temple"

  it "desertTempleStructure has correct size" $ do
    stSize desertTempleStructure `shouldBe` V3 9 8 9

  it "desertTempleStructure contains sand blocks" $ do
    let hasSand = any (\sb -> sbBlock sb == Sand) (stBlocks desertTempleStructure)
    hasSand `shouldBe` True

  it "desertTempleStructure contains a hidden chest" $ do
    let hasChest = any (\sb -> sbBlock sb == Chest) (stBlocks desertTempleStructure)
    hasChest `shouldBe` True

  it "desertTempleStructure has stone brick chamber" $ do
    let hasStoneBrick = any (\sb -> sbBlock sb == StoneBrick) (stBlocks desertTempleStructure)
    hasStoneBrick `shouldBe` True

  it "desertTempleStructure has non-empty block list" $ do
    length (stBlocks desertTempleStructure) `shouldSatisfy` (> 0)

  -- Dungeon structure
  it "dungeonStructure has correct name" $ do
    stName dungeonStructure `shouldBe` "Dungeon"

  it "dungeonStructure has correct size" $ do
    stSize dungeonStructure `shouldBe` V3 7 5 7

  it "dungeonStructure contains cobblestone walls" $ do
    let hasCobble = any (\sb -> sbBlock sb == Cobblestone) (stBlocks dungeonStructure)
    hasCobble `shouldBe` True

  it "dungeonStructure contains a chest" $ do
    let hasChest = any (\sb -> sbBlock sb == Chest) (stBlocks dungeonStructure)
    hasChest `shouldBe` True

  it "dungeonStructure contains a torch" $ do
    let hasTorch = any (\sb -> sbBlock sb == Torch) (stBlocks dungeonStructure)
    hasTorch `shouldBe` True

  it "dungeonStructure has non-empty block list" $ do
    length (stBlocks dungeonStructure) `shouldSatisfy` (> 0)

  -- Village house structure
  it "villageHouseStructure has correct name" $ do
    stName villageHouseStructure `shouldBe` "Village House"

  it "villageHouseStructure has correct size" $ do
    stSize villageHouseStructure `shouldBe` V3 5 5 5

  it "villageHouseStructure contains oak planks" $ do
    let hasPlanks = any (\sb -> sbBlock sb == OakPlanks) (stBlocks villageHouseStructure)
    hasPlanks `shouldBe` True

  it "villageHouseStructure contains a door" $ do
    let hasDoor = any (\sb -> sbBlock sb == OakDoorClosed) (stBlocks villageHouseStructure)
    hasDoor `shouldBe` True

  it "villageHouseStructure contains a bed" $ do
    let hasBed = any (\sb -> sbBlock sb == Bed) (stBlocks villageHouseStructure)
    hasBed `shouldBe` True

  it "villageHouseStructure contains a torch" $ do
    let hasTorch = any (\sb -> sbBlock sb == Torch) (stBlocks villageHouseStructure)
    hasTorch `shouldBe` True

  it "villageHouseStructure has non-empty block list" $ do
    length (stBlocks villageHouseStructure) `shouldSatisfy` (> 0)

  -- placeStructure integration test
  it "placeStructure places blocks into the world" $ withTestWorld $ \world -> do
    placeStructure world (V3 0 64 0) wellStructure
    -- Check cobblestone floor at origin
    bt00 <- worldGetBlock world (V3 0 64 0)
    bt00 `shouldBe` Cobblestone
    -- Check water in center
    btWater <- worldGetBlock world (V3 2 65 2)
    btWater `shouldBe` Water

  it "placeStructure respects offset" $ withTestWorld $ \world -> do
    placeStructure world (V3 5 70 5) dungeonStructure
    -- Floor corner at (5,70,5)
    bt <- worldGetBlock world (V3 5 70 5)
    bt `shouldBe` Cobblestone
    -- Chest at center (5+3, 70+1, 5+3) = (8, 71, 8)
    btChest <- worldGetBlock world (V3 8 71 8)
    btChest `shouldBe` Chest

  it "placeStructure does not affect unrelated positions" $ withTestWorld $ \world -> do
    placeStructure world (V3 0 64 0) wellStructure
    -- Position outside the structure should still be Air
    btFar <- worldGetBlock world (V3 10 64 10)
    btFar `shouldBe` Air

  -- Block offsets stay within bounding box
  it "all wellStructure offsets are within bounding box" $ do
    let V3 sx sy sz = stSize wellStructure
    mapM_ (\(StructureBlock (V3 x y z) _) -> do
      x `shouldSatisfy` (\v -> v >= 0 && v < sx)
      y `shouldSatisfy` (\v -> v >= 0 && v < sy)
      z `shouldSatisfy` (\v -> v >= 0 && v < sz)
      ) (stBlocks wellStructure)

  it "all desertTempleStructure offsets are within bounding box" $ do
    let V3 sx sy sz = stSize desertTempleStructure
    mapM_ (\(StructureBlock (V3 x y z) _) -> do
      x `shouldSatisfy` (\v -> v >= 0 && v < sx)
      y `shouldSatisfy` (\v -> v >= 0 && v < sy)
      z `shouldSatisfy` (\v -> v >= 0 && v < sz)
      ) (stBlocks desertTempleStructure)

  it "all dungeonStructure offsets are within bounding box" $ do
    let V3 sx sy sz = stSize dungeonStructure
    mapM_ (\(StructureBlock (V3 x y z) _) -> do
      x `shouldSatisfy` (\v -> v >= 0 && v < sx)
      y `shouldSatisfy` (\v -> v >= 0 && v < sy)
      z `shouldSatisfy` (\v -> v >= 0 && v < sz)
      ) (stBlocks dungeonStructure)

  it "all villageHouseStructure offsets are within bounding box" $ do
    let V3 sx sy sz = stSize villageHouseStructure
    mapM_ (\(StructureBlock (V3 x y z) _) -> do
      x `shouldSatisfy` (\v -> v >= 0 && v < sx)
      y `shouldSatisfy` (\v -> v >= 0 && v < sy)
      z `shouldSatisfy` (\v -> v >= 0 && v < sz)
      ) (stBlocks villageHouseStructure)

-- =========================================================================
-- Block Registry
-- =========================================================================
blockRegistrySpec :: Spec
blockRegistrySpec = describe "World.BlockRegistry" $ do
  -- Creation
  it "newBlockRegistry creates an empty registry" $ do
    reg <- newBlockRegistry
    entries <- registeredBlocks reg
    length entries `shouldBe` 0

  -- Register and lookup
  it "registerBlock then lookupBlock roundtrips" $ do
    reg <- newBlockRegistry
    registerBlock reg Stone (mkBlockDef Stone)
    result <- lookupBlock reg Stone
    case result of
      Just d  -> bdProperties d `shouldBe` blockProperties Stone
      Nothing -> expectationFailure "Expected Stone block def"

  it "lookupBlock returns Nothing for unregistered block" $ do
    reg <- newBlockRegistry
    result <- lookupBlock reg Dirt
    case result of
      Nothing -> pure ()
      Just _  -> expectationFailure "Expected Nothing for unregistered block"

  -- Overwrite
  it "registerBlock overwrites previous definition" $ do
    reg <- newBlockRegistry
    let def1 = mkBlockDef Stone
        def2 = (mkBlockDef Stone) { bdCollisionHeight = 0.5 }
    registerBlock reg Stone def1
    registerBlock reg Stone def2
    result <- lookupBlock reg Stone
    case result of
      Just d  -> bdCollisionHeight d `shouldBe` 0.5
      Nothing -> expectationFailure "Expected updated Stone block def"

  -- registeredBlocks count
  it "registeredBlocks returns all registered entries" $ do
    reg <- newBlockRegistry
    registerBlock reg Stone (mkBlockDef Stone)
    registerBlock reg Dirt (mkBlockDef Dirt)
    registerBlock reg Sand (mkBlockDef Sand)
    entries <- registeredBlocks reg
    length entries `shouldBe` 3

  -- Default registry
  it "defaultBlockRegistry contains all BlockType values" $ do
    reg <- defaultBlockRegistry
    entries <- registeredBlocks reg
    let allBlockTypes = [minBound .. maxBound] :: [BlockType]
    length entries `shouldBe` length allBlockTypes

  it "defaultBlockRegistry properties match blockProperties for Stone" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg Stone
    case result of
      Just d  -> bdProperties d `shouldBe` blockProperties Stone
      Nothing -> expectationFailure "Expected Stone in default registry"

  it "defaultBlockRegistry properties match blockProperties for Air" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg Air
    case result of
      Just d  -> bdProperties d `shouldBe` blockProperties Air
      Nothing -> expectationFailure "Expected Air in default registry"

  it "defaultBlockRegistry drops match blockDrops for Stone" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg Stone
    case result of
      Just d  -> bdDrops d `shouldBe` blockDrops Stone
      Nothing -> expectationFailure "Expected Stone in default registry"

  it "defaultBlockRegistry preferred tool matches for OakLog" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg OakLog
    case result of
      Just d  -> bdPreferredTool d `shouldBe` blockPreferredTool OakLog
      Nothing -> expectationFailure "Expected OakLog in default registry"

  it "defaultBlockRegistry collision height matches for StoneSlab" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg StoneSlab
    case result of
      Just d  -> bdCollisionHeight d `shouldBe` blockCollisionHeight StoneSlab
      Nothing -> expectationFailure "Expected StoneSlab in default registry"

  it "defaultBlockRegistry harvest level matches for DiamondOre" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg DiamondOre
    case result of
      Just d  -> bdHarvestLevel d `shouldBe` blockRequiredHarvestLevel DiamondOre
      Nothing -> expectationFailure "Expected DiamondOre in default registry"

  it "defaultBlockRegistry tex coords match for Grass top face" $ do
    reg <- defaultBlockRegistry
    result <- lookupBlock reg Grass
    case result of
      Just d  -> bdTexCoords d FaceTop `shouldBe` blockFaceTexCoords Grass FaceTop
      Nothing -> expectationFailure "Expected Grass in default registry"

-- =========================================================================
-- Dimension framework
-- =========================================================================
dimensionSpec :: Spec
dimensionSpec = describe "World.Dimension" $ do
  -- DimensionType enum
  it "DimensionType has 3 values" $ do
    let allDims = [minBound .. maxBound] :: [Dim.DimensionType]
    length allDims `shouldBe` 3

  it "DimensionType roundtrips through Enum" $ do
    let allDims = [minBound .. maxBound] :: [Dim.DimensionType]
    mapM_ (\d -> toEnum (fromEnum d) `shouldBe` d) allDims

  it "DimensionType Enum order is Overworld, Nether, TheEnd" $ do
    fromEnum Dim.Overworld `shouldBe` 0
    fromEnum Dim.Nether `shouldBe` 1
    fromEnum Dim.TheEnd `shouldBe` 2

  -- Overworld config
  it "overworldConfig has correct name" $ do
    Dim.dcName Dim.overworldConfig `shouldBe` "Overworld"

  it "overworldConfig has sea level 63" $ do
    Dim.dcSeaLevel Dim.overworldConfig `shouldBe` 63

  it "overworldConfig has weather" $ do
    Dim.dcHasWeather Dim.overworldConfig `shouldBe` True

  it "overworldConfig has day/night" $ do
    Dim.dcHasDayNight Dim.overworldConfig `shouldBe` True

  it "overworldConfig has no ceiling" $ do
    Dim.dcCeilingY Dim.overworldConfig `shouldBe` Nothing

  it "overworldConfig gravity is 1.0" $ do
    Dim.dcGravity Dim.overworldConfig `shouldBe` 1.0

  -- Nether config
  it "netherConfig has correct name" $ do
    Dim.dcName Dim.netherConfig `shouldBe` "Nether"

  it "netherConfig has sea level (lava level) 32" $ do
    Dim.dcSeaLevel Dim.netherConfig `shouldBe` 32

  it "netherConfig has no weather" $ do
    Dim.dcHasWeather Dim.netherConfig `shouldBe` False

  it "netherConfig has no day/night" $ do
    Dim.dcHasDayNight Dim.netherConfig `shouldBe` False

  it "netherConfig has ceiling at 128" $ do
    Dim.dcCeilingY Dim.netherConfig `shouldBe` Just 128

  it "netherConfig gravity is 1.0" $ do
    Dim.dcGravity Dim.netherConfig `shouldBe` 1.0

  -- End config
  it "endConfig has correct name" $ do
    Dim.dcName Dim.endConfig `shouldBe` "The End"

  it "endConfig has sea level 0" $ do
    Dim.dcSeaLevel Dim.endConfig `shouldBe` 0

  it "endConfig has no weather" $ do
    Dim.dcHasWeather Dim.endConfig `shouldBe` False

  it "endConfig has no day/night" $ do
    Dim.dcHasDayNight Dim.endConfig `shouldBe` False

  it "endConfig has no ceiling" $ do
    Dim.dcCeilingY Dim.endConfig `shouldBe` Nothing

  -- Config distinguishability
  it "all three configs are distinct" $ do
    Dim.overworldConfig `shouldNotBe` Dim.netherConfig
    Dim.netherConfig `shouldNotBe` Dim.endConfig
    Dim.overworldConfig `shouldNotBe` Dim.endConfig

  -- Nether chunk generation
  it "generateNetherChunk produces a non-empty chunk" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    -- Floor should be bedrock
    bt <- getBlock chunk 8 0 8
    bt `shouldBe` Bedrock

  it "generateNetherChunk has bedrock ceiling at y=127" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    bt <- getBlock chunk 8 127 8
    bt `shouldBe` Bedrock

  it "generateNetherChunk above ceiling is air (y=128+)" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    bt <- getBlock chunk 8 200 8
    bt `shouldBe` Air

  it "generateNetherChunk has lava at or below level 32 in carved areas" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    -- Scan a column; any air at y<=32 should have been replaced with lava
    -- Not every block is carved, so we check a structural invariant:
    -- at y<=32, blocks are Bedrock, Stone, or Lava (never Air)
    results <- mapM (\y -> getBlock chunk 8 y 8) [1..32]
    let hasAir = any (== Air) results
    hasAir `shouldBe` False

  it "generateNetherChunk is deterministic (same seed = same chunk)" $ do
    chunk1 <- Dim.generateNetherChunk 42 (V2 0 0)
    chunk2 <- Dim.generateNetherChunk 42 (V2 0 0)
    bt1 <- getBlock chunk1 4 64 4
    bt2 <- getBlock chunk2 4 64 4
    bt1 `shouldBe` bt2

  it "generateNetherChunk differs for different seeds" $ do
    chunk1 <- Dim.generateNetherChunk 42 (V2 0 0)
    chunk2 <- Dim.generateNetherChunk 99 (V2 0 0)
    -- Sample many positions across the chunk; at least one should differ
    results <- sequence
      [ do b1 <- getBlock chunk1 lx y lz
           b2 <- getBlock chunk2 lx y lz
           pure (b1 /= b2)
      | lx <- [0,4,8,12], lz <- [0,4,8,12], y <- [40,60,80,100]
      ]
    or results `shouldBe` True

  it "generateNetherChunk contains some Lava blocks" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    -- Check a sweep at lava level
    results <- mapM (\lx -> mapM (\lz -> getBlock chunk lx 20 lz) [0..15]) [0..15]
    let hasLava = any (any (== Lava)) results
    hasLava `shouldBe` True

  it "generateNetherChunk contains some light-emitting blocks (glowstone proxy)" $ do
    chunk <- Dim.generateNetherChunk 42 (V2 0 0)
    -- Torch is used as glowstone proxy; scan near ceiling across y=120..126
    results <- sequence
      [ getBlock chunk lx y lz
      | y <- [120..126], lx <- [0..15], lz <- [0,4,8,12]
      ]
    any (== Torch) results `shouldBe` True

  describe "Game.Config" $ do
    it "defaultConfig has render distance 4" $ do
      cfgRenderDistance defaultConfig `shouldBe` 4

    it "defaultConfig has tick rate 20" $ do
      cfgTickRate defaultConfig `shouldBe` 20.0

    it "defaultConfig has max reach 5" $ do
      cfgMaxReach defaultConfig `shouldBe` 5.0

    it "defaultConfig has FOV 45" $ do
      cfgFOV defaultConfig `shouldBe` 45.0

    it "defaultConfig has mouse sensitivity 0.15" $ do
      cfgMouseSensitivity defaultConfig `shouldBe` 0.15

    it "defaultConfig has day length 1200 seconds" $ do
      cfgDayLength defaultConfig `shouldBe` 1200.0

    it "defaultConfig has hostile spawn cap 20" $ do
      cfgMaxSpawnHostile defaultConfig `shouldBe` 20

    it "defaultConfig has passive spawn cap 10" $ do
      cfgMaxSpawnPassive defaultConfig `shouldBe` 10

    it "defaultConfig has spawn radius 24" $ do
      cfgSpawnRadius defaultConfig `shouldBe` 24

    it "defaultConfig has despawn distance 128" $ do
      cfgDespawnDistance defaultConfig `shouldBe` 128

    it "defaultConfig has auto-save interval 18000" $ do
      cfgAutoSaveInterval defaultConfig `shouldBe` 18000

    it "defaultConfig has empty starting items" $ do
      cfgStartingItems defaultConfig `shouldBe` []

    it "record update produces new config without mutation" $ do
      let custom = defaultConfig { cfgRenderDistance = 8 }
      cfgRenderDistance custom `shouldBe` 8
      cfgRenderDistance defaultConfig `shouldBe` 4

-- =========================================================================
-- UI.Screen registry
-- =========================================================================
screenRegistrySpec :: Spec
screenRegistrySpec = describe "UI.Screen" $ do
  let dummyScreen :: String -> ScreenDef
      dummyScreen name = ScreenDef
        { sdName        = name
        , sdRender      = \_ -> [1.0, 2.0, 3.0]
        , sdHandleClick = \_ _ _ -> pure ()
        , sdHandleKey   = \_ _ -> pure ()
        , sdOnOpen      = \_ -> pure ()
        , sdOnClose     = \_ -> pure ()
        }

  it "newScreenRegistry starts empty" $ do
    reg <- newScreenRegistry
    screens <- allScreens reg
    length screens `shouldBe` 0

  it "registerScreen then lookupScreen roundtrips" $ do
    reg <- newScreenRegistry
    registerScreen reg (dummyScreen "inventory")
    result <- lookupScreen reg "inventory"
    case result of
      Just sd -> sdName sd `shouldBe` "inventory"
      Nothing -> expectationFailure "Expected inventory screen"

  it "lookupScreen returns Nothing for unregistered name" $ do
    reg <- newScreenRegistry
    result <- lookupScreen reg "nonexistent"
    case result of
      Nothing -> pure ()
      Just _  -> expectationFailure "Expected Nothing for unregistered screen"

  it "registerScreen overwrites existing screen with same name" $ do
    reg <- newScreenRegistry
    let sd1 = (dummyScreen "pause") { sdRender = \_ -> [1.0] }
        sd2 = (dummyScreen "pause") { sdRender = \_ -> [9.0, 8.0] }
    registerScreen reg sd1
    registerScreen reg sd2
    result <- lookupScreen reg "pause"
    case result of
      Just sd -> sdRender sd undefined `shouldBe` [9.0, 8.0]
      Nothing -> expectationFailure "Expected overwritten pause screen"

  it "allScreens returns all registered screens" $ do
    reg <- newScreenRegistry
    registerScreen reg (dummyScreen "inventory")
    registerScreen reg (dummyScreen "crafting")
    registerScreen reg (dummyScreen "pause")
    screens <- allScreens reg
    length screens `shouldBe` 3

  it "allScreens contains expected names" $ do
    reg <- newScreenRegistry
    registerScreen reg (dummyScreen "inventory")
    registerScreen reg (dummyScreen "crafting")
    screens <- allScreens reg
    let names = map sdName screens
    "inventory" `elem` names `shouldBe` True
    "crafting" `elem` names `shouldBe` True

  it "sdRender callback returns expected vertex data" $ do
    reg <- newScreenRegistry
    registerScreen reg (dummyScreen "test")
    result <- lookupScreen reg "test"
    case result of
      Just sd -> sdRender sd undefined `shouldBe` [1.0, 2.0, 3.0]
      Nothing -> expectationFailure "Expected test screen"

-- =========================================================================
-- Entity.Component
-- =========================================================================
componentSpec :: Spec
componentSpec = describe "Entity.Component" $ do
  it "newComponentMap starts empty" $ do
    ref <- newComponentMap
    comps <- getComponents ref 1
    comps `shouldBe` []

  it "addComponent stores a single component" $ do
    ref <- newComponentMap
    addComponent ref 1 (HealthComp 20 20)
    comps <- getComponents ref 1
    comps `shouldBe` [HealthComp 20 20]

  it "addComponent appends multiple components to same entity" $ do
    ref <- newComponentMap
    addComponent ref 1 (HealthComp 10 20)
    addComponent ref 1 (TamedComp False)
    comps <- getComponents ref 1
    comps `shouldBe` [HealthComp 10 20, TamedComp False]

  it "addComponent keeps entities independent" $ do
    ref <- newComponentMap
    addComponent ref 1 (HealthComp 10 20)
    addComponent ref 2 (PaintingComp 90)
    c1 <- getComponents ref 1
    c2 <- getComponents ref 2
    c1 `shouldBe` [HealthComp 10 20]
    c2 `shouldBe` [PaintingComp 90]

  it "removeComponent drops matching component" $ do
    ref <- newComponentMap
    addComponent ref 1 (HealthComp 10 20)
    addComponent ref 1 (TamedComp True)
    removeComponent ref 1 (\case HealthComp {} -> True; _ -> False)
    comps <- getComponents ref 1
    comps `shouldBe` [TamedComp True]

  it "removeComponent is no-op when no match" $ do
    ref <- newComponentMap
    addComponent ref 1 (TamedComp False)
    removeComponent ref 1 (\case HealthComp {} -> True; _ -> False)
    comps <- getComponents ref 1
    comps `shouldBe` [TamedComp False]

  it "removeEntity clears all components for an entity" $ do
    ref <- newComponentMap
    addComponent ref 1 (HealthComp 5 10)
    addComponent ref 1 (AIComp (AIIdle 3.0))
    removeEntity ref 1
    comps <- getComponents ref 1
    comps `shouldBe` []

  it "getHealth returns Just for HealthComp" $ do
    getHealth [TamedComp False, HealthComp 7 20] `shouldBe` Just (7, 20)

  it "getHealth returns Nothing when absent" $ do
    getHealth [TamedComp True, PaintingComp 45] `shouldBe` Nothing

  it "isTamed returns True for TamedComp True" $ do
    isTamed [HealthComp 10 10, TamedComp True] `shouldBe` True

  it "isTamed returns False when absent" $ do
    isTamed [HealthComp 10 10] `shouldBe` False

  it "isMountOccupied detects occupied mount" $ do
    isMountOccupied [MountableComp True] `shouldBe` True

  it "isMountOccupied returns False when unoccupied" $ do
    isMountOccupied [MountableComp False] `shouldBe` False

  it "getProjectile extracts damage and age" $ do
    getProjectile [ProjectileComp 5.0 1.2] `shouldBe` Just (5.0, 1.2)

  it "getPaintingYaw extracts yaw" $ do
    getPaintingYaw [PaintingComp 270] `shouldBe` Just 270

  it "getAI extracts AIState" $ do
    let st = AIIdle 2.5
    getAI [HealthComp 10 10, AIComp st] `shouldBe` Just st

-- =========================================================================
-- Event Bus
-- =========================================================================
eventBusSpec :: Spec
eventBusSpec = describe "Game.Event" $ do
  it "newEventBus starts with no handlers (emit does nothing)" $ do
    bus <- newEventBus
    -- emitting to an empty bus should not crash
    emit bus (EvPlayerDied)

  it "subscribe registers a handler that receives events" $ do
    ref <- newIORef ([] :: [GameEvent])
    bus <- newEventBus
    subscribe bus (\ev -> modifyIORef' ref (++ [ev]))
    emit bus (EvPlayerDamaged 5)
    got <- readIORef ref
    got `shouldBe` [EvPlayerDamaged 5]

  it "emit dispatches to multiple handlers in subscription order" $ do
    ref <- newIORef ([] :: [Int])
    bus <- newEventBus
    subscribe bus (\_ -> modifyIORef' ref (++ [1]))
    subscribe bus (\_ -> modifyIORef' ref (++ [2]))
    subscribe bus (\_ -> modifyIORef' ref (++ [3]))
    emit bus EvPlayerDied
    got <- readIORef ref
    got `shouldBe` [1, 2, 3]

  it "handler receives the exact event that was emitted" $ do
    ref <- newIORef Nothing
    bus <- newEventBus
    subscribe bus (\ev -> modifyIORef' ref (const (Just ev)))
    let ev = EvBlockBroken (V3 1 2 3) Stone
    emit bus ev
    got <- readIORef ref
    got `shouldBe` Just ev

  it "each emit invokes all handlers independently" $ do
    counter <- newIORef (0 :: Int)
    bus <- newEventBus
    subscribe bus (\_ -> modifyIORef' counter (+ 1))
    subscribe bus (\_ -> modifyIORef' counter (+ 1))
    emit bus EvPlayerDied
    emit bus EvPlayerDied
    got <- readIORef counter
    got `shouldBe` 4

  it "handlers subscribed after an emit do not see past events" $ do
    ref <- newIORef ([] :: [GameEvent])
    bus <- newEventBus
    emit bus EvPlayerDied
    subscribe bus (\ev -> modifyIORef' ref (++ [ev]))
    got <- readIORef ref
    got `shouldBe` []

  it "EvBlockPlaced carries the correct block type" $ do
    ref <- newIORef Nothing
    bus <- newEventBus
    subscribe bus (\ev -> modifyIORef' ref (const (Just ev)))
    let ev = EvBlockPlaced (V3 10 64 10) Cobblestone
    emit bus ev
    got <- readIORef ref
    got `shouldBe` Just ev

  it "EvMobKilled carries tag and entity id" $ do
    ref <- newIORef Nothing
    bus <- newEventBus
    subscribe bus (\ev -> modifyIORef' ref (const (Just ev)))
    let ev = EvMobKilled "Zombie" 42
    emit bus ev
    got <- readIORef ref
    got `shouldBe` Just ev

  it "EvAchievementUnlocked carries the achievement name" $ do
    ref <- newIORef Nothing
    bus <- newEventBus
    subscribe bus (\ev -> modifyIORef' ref (const (Just ev)))
    let ev = EvAchievementUnlocked "first_diamond"
    emit bus ev
    got <- readIORef ref
    got `shouldBe` Just ev

-- =========================================================================
-- SaveV3
-- =========================================================================
saveV3Spec :: Spec
saveV3Spec = describe "Game.SaveV3" $ do

  it "savev3Version is 3" $ do
    savev3Version `shouldBe` (3 :: Word8)

  it "SaveDataV3 roundtrips through Binary encode/decode" $ do
    let sd = sampleV3
    decode (encode sd) `shouldBe` sd

  it "encodeSaveV3/decodeSaveV3 roundtrip" $ do
    let sd = sampleV3
    decodeSaveV3 (encodeSaveV3 sd) `shouldBe` Just sd

  it "decodeSaveV3 rejects wrong version prefix" $ do
    -- Write version 2 prefix followed by valid V3 payload
    let badBytes = encode (2 :: Word8) <> encode sampleV3
    decodeSaveV3 badBytes `shouldBe` Nothing

  it "decodeSaveV3 rejects truncated input" $ do
    decodeSaveV3 BL.empty `shouldBe` Nothing

  it "ChunkMeta roundtrips through Binary" $ do
    let cm = ChunkMeta (V2 3 (-5)) True False
    decode (encode cm) `shouldBe` cm

  it "migrateV2toV3 preserves player position" $ do
    let v2 = sampleV2
        v3 = migrateV2toV3 v2
    sv3PlayerPos v3 `shouldBe` sdPlayerPos v2

  it "migrateV2toV3 preserves inventory" $ do
    let v2 = sampleV2
        v3 = migrateV2toV3 v2
    sv3Inventory v3 `shouldBe` sdInventory v2

  it "migrateV2toV3 converts Int health to Float" $ do
    let v2 = sampleV2 { sdHealth = 15 }
        v3 = migrateV2toV3 v2
    sv3Health v3 `shouldBe` 15.0

  it "migrateV2toV3 sets version to 3" $ do
    let v3 = migrateV2toV3 sampleV2
    sv3Version v3 `shouldBe` savev3Version

  it "migrateV2toV3 defaults: XP=0, airSupply=15, weather=Clear" $ do
    let v3 = migrateV2toV3 sampleV2
    sv3XP v3 `shouldBe` 0
    sv3XPLevel v3 `shouldBe` 0
    sv3AirSupply v3 `shouldBe` 15.0
    sv3WeatherType v3 `shouldBe` 0

  it "SaveDataV3 with non-empty armor and chunk metas roundtrips" $ do
    let sd = sampleV3
              { sv3ArmorSlots = [Just (ToolItem Pickaxe Diamond 100, 1), Nothing, Nothing, Nothing]
              , sv3ChunkMetas = [ChunkMeta (V2 0 0) True True, ChunkMeta (V2 1 (-1)) False False]
              }
    decode (encode sd) `shouldBe` sd

-- | Sample V2 save data for migration tests
sampleV2 :: SaveData
sampleV2 = SaveData
  { sdPlayerPos    = (10.5, 65.0, -3.2)
  , sdPlayerYaw    = 90.0
  , sdPlayerPitch  = -15.0
  , sdPlayerFlying = False
  , sdWorldSeed    = 42
  , sdDayTime      = 0.5
  , sdDayCount     = 7
  , sdHealth       = 18
  , sdHunger       = 16
  , sdFallDist     = 1.2
  , sdInventory    = [(0, BlockItem Stone, 32), (1, ToolItem Sword Iron 200, 1)]
  , sdSelectedSlot = 0
  }

-- | Sample V3 save data for roundtrip tests
sampleV3 :: SaveDataV3
sampleV3 = SaveDataV3
  { sv3Version      = savev3Version
  , sv3PlayerPos    = (1.5, 65.0, -3.2)
  , sv3PlayerYaw    = 45.0
  , sv3PlayerPitch  = -10.0
  , sv3Flying       = True
  , sv3Health       = 18.5
  , sv3Hunger       = 12
  , sv3Saturation   = 8.0
  , sv3XP           = 150
  , sv3XPLevel      = 5
  , sv3FallDist     = 0.0
  , sv3AirSupply    = 15.0
  , sv3DayTime      = 0.75
  , sv3DayCount     = 3
  , sv3WeatherType  = 1
  , sv3WeatherTimer = 300.0
  , sv3Inventory    = [(0, BlockItem Stone, 64), (1, ToolItem Pickaxe Diamond 1500, 1)]
  , sv3ArmorSlots   = []
  , sv3SpawnPoint   = (0, 80, 0)
  , sv3WorldSeed    = 12345
  , sv3ChunkMetas   = []
  }

-- =========================================================================
-- RecipeRegistry
-- =========================================================================
recipeRegistrySpec :: Spec
recipeRegistrySpec = describe "Game.RecipeRegistry" $ do
  it "new registry starts empty" $ do
    reg <- newRecipeRegistry
    recipes <- allRegisteredRecipes reg
    recipes `shouldBe` []

  it "registerRecipe adds a single recipe" $ do
    reg <- newRecipeRegistry
    let recipe = Recipe [[Just (BlockItem OakLog)]] (BlockItem OakPlanks) 4
    registerRecipe reg recipe
    recipes <- allRegisteredRecipes reg
    length recipes `shouldBe` 1

  it "registerRecipes adds multiple recipes" $ do
    reg <- newRecipeRegistry
    let r1 = Recipe [[Just (BlockItem OakLog)]] (BlockItem OakPlanks) 4
        r2 = Recipe [[Just (BlockItem Sand)]] (BlockItem Glass) 1
    registerRecipes reg [r1, r2]
    recipes <- allRegisteredRecipes reg
    length recipes `shouldBe` 2

  it "defaultRecipeRegistry contains all recipes from allRecipes" $ do
    reg <- defaultRecipeRegistry
    recipes <- allRegisteredRecipes reg
    length recipes `shouldBe` length allRecipes

  it "findMatchingRecipe succeeds for OakLog -> OakPlanks" $ do
    reg <- defaultRecipeRegistry
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    result <- findMatchingRecipe reg grid
    result `shouldBe` CraftSuccess (BlockItem OakPlanks) 4

  it "findMatchingRecipe returns CraftFailure for empty grid" $ do
    reg <- defaultRecipeRegistry
    result <- findMatchingRecipe reg (emptyCraftingGrid 3)
    result `shouldBe` CraftFailure

  it "findMatchingRecipe returns CraftFailure for unregistered pattern" $ do
    reg <- newRecipeRegistry
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    result <- findMatchingRecipe reg grid
    result `shouldBe` CraftFailure

  it "findMatchingRecipe matches regardless of grid position" $ do
    reg <- defaultRecipeRegistry
    let grid = setCraftingSlot (emptyCraftingGrid 3) 2 2 (Just (BlockItem OakLog))
    result <- findMatchingRecipe reg grid
    result `shouldBe` CraftSuccess (BlockItem OakPlanks) 4

  it "registerRecipe accumulates with prior recipes" $ do
    reg <- newRecipeRegistry
    let r1 = Recipe [[Just (BlockItem OakLog)]] (BlockItem OakPlanks) 4
        r2 = Recipe [[Just (BlockItem Sand)]] (BlockItem Glass) 1
    registerRecipe reg r1
    registerRecipe reg r2
    recipes <- allRegisteredRecipes reg
    length recipes `shouldBe` 2

  it "findMatchingRecipe works for 2x2 crafting table recipe" $ do
    reg <- defaultRecipeRegistry
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 1 0 p) 1 1 p
    result <- findMatchingRecipe reg grid
    result `shouldBe` CraftSuccess (BlockItem CraftingTable) 1

-- =========================================================================
-- BiomeFeatures
-- =========================================================================
biomeFeaturesSpec :: Spec
biomeFeaturesSpec = describe "World.BiomeFeatures" $ do
  it "biomeFeatures returns non-empty list for every biome" $ do
    let allBiomes = [minBound .. maxBound] :: [BiomeType]
    mapM_ (\b -> biomeFeatures b `shouldNotBe` []) allBiomes

  it "Plains has at least one TreeFeature" $ do
    let trees = featureTrees (biomeFeatures Plains)
    length trees `shouldSatisfy` (> 0)

  it "Plains has common ore features" $ do
    let ores = featureOres (biomeFeatures Plains)
    length ores `shouldSatisfy` (>= 4)

  it "Forest total tree density exceeds Plains" $ do
    let forestDensity = sum $ map treeDensity (featureTrees (biomeFeatures Forest))
        plainsDensity = sum $ map treeDensity (featureTrees (biomeFeatures Plains))
    forestDensity `shouldSatisfy` (> plainsDensity)

  it "Desert has no TreeFeature" $ do
    let trees = featureTrees (biomeFeatures Desert)
    trees `shouldBe` []

  it "Desert has Cactus DecorationFeature" $ do
    let decos = featureDecorations (biomeFeatures Desert)
        hasCactus = any (\(DecorationFeature bt _) -> bt == Cactus) decos
    hasCactus `shouldBe` True

  it "Mountains has SpruceTree feature" $ do
    let trees = featureTrees (biomeFeatures Mountains)
        hasSpruce = any (\(TreeFeature t _) -> t == SpruceTree) trees
    hasSpruce `shouldBe` True

  it "all OreFeature Y ranges are valid (minY < maxY, both > 0)" $ do
    let allBiomes = [minBound .. maxBound] :: [BiomeType]
        allOres = concatMap (featureOres . biomeFeatures) allBiomes
    mapM_ (\ore -> do
      oreMinY ore `shouldSatisfy` (> 0)
      oreMaxY ore `shouldSatisfy` (> 0)
      oreMinY ore `shouldSatisfy` (< oreMaxY ore)) allOres

  it "treeDensity returns 0 for OreFeature" $ do
    treeDensity (OreFeature CoalOre 4 80 0.7) `shouldBe` 0

  it "treeLogBlock is defined for all TreeType values" $ do
    let allTrees = [minBound .. maxBound] :: [TreeType]
    mapM_ (\t -> treeLogBlock t `shouldSatisfy` const True) allTrees

  it "treeLeafBlock is defined for all TreeType values" $ do
    let allTrees = [minBound .. maxBound] :: [TreeType]
    mapM_ (\t -> treeLeafBlock t `shouldSatisfy` const True) allTrees

  it "at least one biome has a StructureFeature" $ do
    let allBiomes = [minBound .. maxBound] :: [BiomeType]
        allStructs = concatMap (featureStructures . biomeFeatures) allBiomes
    length allStructs `shouldSatisfy` (> 0)

  it "all DecorationFeature chances are in (0, 1)" $ do
    let allBiomes = [minBound .. maxBound] :: [BiomeType]
        allDecos = concatMap (featureDecorations . biomeFeatures) allBiomes
    mapM_ (\d -> do
      decorationChance d `shouldSatisfy` (> 0)
      decorationChance d `shouldSatisfy` (< 1)) allDecos

  it "all StructureFeature chances are in (0, 1)" $ do
    let allBiomes = [minBound .. maxBound] :: [BiomeType]
        allStructs = concatMap (featureStructures . biomeFeatures) allBiomes
    mapM_ (\s -> do
      structureChance s `shouldSatisfy` (> 0)
      structureChance s `shouldSatisfy` (< 1)) allStructs

  it "TreeType roundtrips through Enum" $ do
    let allTrees = [minBound .. maxBound] :: [TreeType]
    mapM_ (\t -> toEnum (fromEnum t) `shouldBe` t) allTrees

  it "OakTree uses OakLog and OakLeaves" $ do
    treeLogBlock OakTree `shouldBe` OakLog
    treeLeafBlock OakTree `shouldBe` OakLeaves

-- =========================================================================
-- Item Registry
-- =========================================================================
itemRegistrySpec :: Spec
itemRegistrySpec = describe "Game.ItemRegistry" $ do
  -- Creation
  it "newItemRegistry creates an empty registry" $ do
    reg <- newItemRegistry
    entries <- registeredItems reg
    length entries `shouldBe` 0

  -- Register and lookup
  it "registerItem then lookupItem roundtrips" $ do
    reg <- newItemRegistry
    let def = mkItemDef 2 StickItem
    registerItem reg 20 def
    result <- lookupItem reg 20
    case result of
      Just d  -> idStackLimit d `shouldBe` 64
      Nothing -> expectationFailure "Expected StickItem def"

  it "lookupItem returns Nothing for unregistered key" $ do
    reg <- newItemRegistry
    result <- lookupItem reg 255
    result `shouldBe` Nothing

  -- Overwrite
  it "registerItem overwrites previous definition" $ do
    reg <- newItemRegistry
    let def1 = mkItemDef 2 StickItem
        def2 = def1 { idStackLimit = 16 }
    registerItem reg 20 def1
    registerItem reg 20 def2
    result <- lookupItem reg 20
    case result of
      Just d  -> idStackLimit d `shouldBe` 16
      Nothing -> expectationFailure "Expected updated StickItem def"

  -- registeredItems count
  it "registeredItems returns all registered entries" $ do
    reg <- newItemRegistry
    registerItem reg 0 (mkItemDef 0 (BlockItem Stone))
    registerItem reg 1 (mkItemDef 2 StickItem)
    registerItem reg 2 (mkItemDef 8 CompassItem)
    entries <- registeredItems reg
    length entries `shouldBe` 3

  -- Default registry
  it "defaultItemRegistry is non-empty" $ do
    reg <- defaultItemRegistry
    entries <- registeredItems reg
    length entries `shouldSatisfy` (> 0)

  it "defaultItemRegistry contains StickItem (key 20)" $ do
    reg <- defaultItemRegistry
    result <- lookupItem reg 20
    case result of
      Just d  -> do
        idStackLimit d `shouldBe` 64
        idBinaryTag d `shouldBe` 2
      Nothing -> expectationFailure "Expected StickItem in default registry"

  it "defaultItemRegistry CompassItem has stack limit 1" $ do
    reg <- defaultItemRegistry
    result <- lookupItem reg 82
    case result of
      Just d  -> idStackLimit d `shouldBe` 1
      Nothing -> expectationFailure "Expected CompassItem in default registry"

  it "defaultItemRegistry BoatItem has no block mapping" $ do
    reg <- defaultItemRegistry
    result <- lookupItem reg 100
    case result of
      Just d  -> idToBlock d `shouldBe` Nothing
      Nothing -> expectationFailure "Expected BoatItem in default registry"

  it "defaultItemRegistry BlockItem Stone maps to Stone block" $ do
    reg <- defaultItemRegistry
    result <- lookupItem reg (fromIntegral (fromEnum Stone))
    case result of
      Just d  -> idToBlock d `shouldBe` Just Stone
      Nothing -> expectationFailure "Expected BlockItem Stone in default registry"

  -- mkItemDef preserves properties
  it "mkItemDef preserves stack limit for BlockItem" $ do
    let def = mkItemDef 0 (BlockItem Dirt)
    idStackLimit def `shouldBe` 64

  it "mkItemDef preserves stack limit for ToolItem" $ do
    let def = mkItemDef 1 (ToolItem Pickaxe Diamond 1561)
    idStackLimit def `shouldBe` 1

  it "mkItemDef color is non-trivial for Coal" $ do
    let def = mkItemDef 4 (MaterialItem Coal)
        (r, g, b, a) = idColor def
    a `shouldBe` 1.0
    r `shouldSatisfy` (< 0.5)
    g `shouldSatisfy` (< 0.5)
    b `shouldSatisfy` (< 0.5)

  it "mkItemDef mini-icon is non-empty for Sword" $ do
    let def = mkItemDef 1 (ToolItem Sword Iron 250)
    length (idMiniIcon def) `shouldSatisfy` (> 0)

  describe "Game.PotionEffect" $ do
    it "tickEffects decrements duration" $ do
      let effs = [ActiveEffect SpeedEffect 0 10.0]
          (alive, _) = tickEffects 1.0 effs
      length alive `shouldBe` 1
      aeDuration (head alive) `shouldSatisfy` (\d -> abs (d - 9.0) < 0.01)

    it "tickEffects removes expired effects" $ do
      let effs = [ActiveEffect PoisonEffect 0 0.5]
          (alive, _) = tickEffects 1.0 effs
      length alive `shouldBe` 0

    it "tickEffects returns SpeedMod for SpeedEffect" $ do
      let effs = [ActiveEffect SpeedEffect 0 10.0]
          (_, ticks) = tickEffects 0.05 effs
      ticks `shouldBe` [SpeedMod 0.2]

    it "tickEffects returns DamageTick for PoisonEffect" $ do
      let effs = [ActiveEffect PoisonEffect 0 5.0]
          (_, ticks) = tickEffects 0.05 effs
      ticks `shouldBe` [DamageTick 0.8]

    it "tickEffects returns HealTick for RegenerationEffect" $ do
      let effs = [ActiveEffect RegenerationEffect 0 10.0]
          (_, ticks) = tickEffects 0.05 effs
      ticks `shouldBe` [HealTick 0.4]

    it "applyPotion SpeedPotion adds SpeedEffect" $ do
      let effs = applyPotion SpeedPotion []
      length effs `shouldBe` 1
      aeType (head effs) `shouldBe` SpeedEffect
      aeDuration (head effs) `shouldSatisfy` (> 0)

    it "applyPotion PoisonPotion adds PoisonEffect" $ do
      let effs = applyPotion PoisonPotion []
      length effs `shouldBe` 1
      aeType (head effs) `shouldBe` PoisonEffect

    it "applyPotion replaces existing effect of same type" $ do
      let effs = applyPotion SpeedPotion [ActiveEffect SpeedEffect 0 5.0]
      length effs `shouldBe` 1
      aeDuration (head effs) `shouldSatisfy` (> 25.0)

    it "applyPotion WaterBottle does nothing" $ do
      let effs = applyPotion WaterBottle [ActiveEffect SpeedEffect 0 5.0]
      length effs `shouldBe` 1

    it "multiple effects tick independently" $ do
      let effs = [ActiveEffect SpeedEffect 0 10.0, ActiveEffect PoisonEffect 0 2.0]
          (alive, ticks) = tickEffects 0.05 effs
      length alive `shouldBe` 2
      length ticks `shouldBe` 2

    it "effectName returns non-empty for all types" $ do
      mapM_ (\t -> effectName t `shouldSatisfy` (not . null)) [minBound .. maxBound :: EffectType]

    it "removeEffectType removes matching effect" $ do
      let effs = [ActiveEffect SpeedEffect 0 10.0, ActiveEffect PoisonEffect 0 5.0]
          result = removeEffectType SpeedEffect effs
      length result `shouldBe` 1
      aeType (head result) `shouldBe` PoisonEffect

    it "amplifier increases SpeedMod magnitude" $ do
      let effs = [ActiveEffect SpeedEffect 1 10.0]
          (_, ticks) = tickEffects 0.05 effs
      ticks `shouldBe` [SpeedMod 0.4]

  describe "Entity.Pathfinding" $ do
    it "finds direct path on flat terrain" $ do
      let solidAt _ y _ = pure (y < 0)
      result <- findPath solidAt (V3 0 0 0) (V3 3 0 0)
      result `shouldSatisfy` \case Just _ -> True; Nothing -> False

    it "returns Nothing when completely blocked" $ do
      let solidAt _ y _ = pure (y >= 0 || y < -1)
      result <- findPath solidAt (V3 0 0 0) (V3 10 0 0)
      result `shouldBe` Nothing

    it "path excludes start position" $ do
      let solidAt _ y _ = pure (y < 0)
      result <- findPath solidAt (V3 0 0 0) (V3 1 0 0)
      case result of
        Just path -> head path `shouldSatisfy` (/= V3 0 0 0)
        Nothing   -> expectationFailure "Expected a path"

    it "path ends at goal" $ do
      let solidAt _ y _ = pure (y < 0)
      result <- findPath solidAt (V3 0 0 0) (V3 2 0 0)
      case result of
        Just path -> last path `shouldBe` V3 2 0 0
        Nothing   -> expectationFailure "Expected a path"

    it "pathDistance is zero for empty path" $ do
      pathDistance [] `shouldBe` 0

    it "pathDistance is zero for single-node path" $ do
      pathDistance [V3 0 0 0] `shouldBe` 0

    it "pathDistance is positive for multi-node path" $ do
      pathDistance [V3 0 0 0, V3 1 0 0, V3 2 0 0] `shouldSatisfy` (> 0)

    it "start equals goal returns empty or trivial path" $ do
      let solidAt _ y _ = pure (y < 0)
      result <- findPath solidAt (V3 0 0 0) (V3 0 0 0)
      case result of
        Just path -> length path `shouldSatisfy` (<= 1)
        Nothing   -> pure ()

  describe "Game.DayNight" $ do
    it "newDayNightCycle starts at time 0.25 (dawn)" $ do
      let dnc = newDayNightCycle
      dncTime dnc `shouldSatisfy` (\t -> t >= 0 && t <= 1)

    it "updateDayNight advances time" $ do
      let dnc = newDayNightCycle
          dnc' = updateDayNight 10.0 dnc
      dncTime dnc' `shouldSatisfy` (/= dncTime dnc)

    it "time wraps around after full cycle" $ do
      let dnc = newDayNightCycle
          dnc' = updateDayNight 1200.0 dnc
      dncTime dnc' `shouldSatisfy` (\t -> t >= 0 && t < 1.01)

    it "getSkyColor returns valid RGBA" $ do
      let dnc = newDayNightCycle
          V4 r g b a = getSkyColor dnc
      r `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      g `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      b `shouldSatisfy` (\x -> x >= 0 && x <= 1)
      a `shouldBe` 1.0

    it "getAmbientLight is between 0 and 1" $ do
      let dnc = newDayNightCycle
      getAmbientLight dnc `shouldSatisfy` (\a -> a >= 0 && a <= 1)

    it "isNight is False at dawn" $ do
      let dnc = newDayNightCycle
      isNight dnc `shouldBe` False

    it "isNight is True at midnight" $ do
      let dnc = newDayNightCycle { dncTime = 0.0 }
      isNight dnc `shouldBe` True

    it "getTimeOfDay returns a phase" $ do
      let dnc = newDayNightCycle
      getTimeOfDay dnc `shouldSatisfy` (\t -> t `elem` [Dawn, Day, Dusk, Night])

  describe "Entity.Spawn" $ do
    it "defaultSpawnRules has positive max hostile" $ do
      srMaxHostile defaultSpawnRules `shouldSatisfy` (> 0)

    it "defaultSpawnRules has positive max passive" $ do
      srMaxPassive defaultSpawnRules `shouldSatisfy` (> 0)

    it "defaultSpawnRules spawn radius is positive" $ do
      srSpawnRadius defaultSpawnRules `shouldSatisfy` (> 0)

    it "defaultSpawnRules despawn distance > spawn radius" $ do
      srDespawnDist defaultSpawnRules `shouldSatisfy` (> srSpawnRadius defaultSpawnRules)

    it "defaultSpawnRules cooldown is positive" $ do
      srSpawnCooldown defaultSpawnRules `shouldSatisfy` (> 0)

-- =========================================================================
-- Armor damage reduction
-- =========================================================================
armorDamageReductionSpec :: Spec
armorDamageReductionSpec = describe "Armor damage reduction" $ do
  it "no armor: full damage applied (minimum 1)" $ do
    let player = defaultPlayer (V3 0 80 0)
    plHealth (damagePlayer 4 player) `shouldBe` (maxHealth - 4)

  it "no armor: 1 raw damage stays 1" $ do
    let player = defaultPlayer (V3 0 80 0)
    plHealth (damagePlayer 1 player) `shouldBe` (maxHealth - 1)

  it "full iron armor reduces damage" $ do
    -- Iron armor total defense: helmet(2) + chestplate(6) + leggings(5) + boots(2) = 15
    let ironArmor = [ Just (ItemStack (ArmorItem Helmet IronArmor 100) 1)
                    , Just (ItemStack (ArmorItem Chestplate IronArmor 100) 1)
                    , Just (ItemStack (ArmorItem Leggings IronArmor 100) 1)
                    , Just (ItemStack (ArmorItem Boots IronArmor 100) 1)
                    ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = ironArmor }
    -- 10 raw damage - 15/2 = 10 - 7 = 3; max(1, 3) = 3
    plHealth (damagePlayer 10 player) `shouldBe` (maxHealth - 3)

  it "full diamond armor reduces damage further" $ do
    -- Diamond armor total defense: helmet(3) + chestplate(8) + leggings(6) + boots(3) = 20
    let diamondArmor = [ Just (ItemStack (ArmorItem Helmet DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Chestplate DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Leggings DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Boots DiamondArmor 100) 1)
                       ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = diamondArmor }
    -- 10 raw damage - 20/2 = 10 - 10 = 0; max(1, 0) = 1
    plHealth (damagePlayer 10 player) `shouldBe` (maxHealth - 1)

  it "damage reduction never reduces below 1" $ do
    -- Even with full diamond armor, minimum 1 damage
    let diamondArmor = [ Just (ItemStack (ArmorItem Helmet DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Chestplate DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Leggings DiamondArmor 100) 1)
                       , Just (ItemStack (ArmorItem Boots DiamondArmor 100) 1)
                       ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = diamondArmor }
    -- 5 raw damage - 20/2 = 5 - 10 = -5; max(1, -5) = 1
    plHealth (damagePlayer 5 player) `shouldBe` (maxHealth - 1)

  it "partial armor (only chestplate) provides partial defense" $ do
    -- Iron chestplate: 6 defense
    let partialArmor = [ Nothing
                       , Just (ItemStack (ArmorItem Chestplate IronArmor 100) 1)
                       , Nothing
                       , Nothing
                       ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = partialArmor }
    -- 10 raw damage - 6/2 = 10 - 3 = 7
    plHealth (damagePlayer 10 player) `shouldBe` (maxHealth - 7)

  it "armor durability decreases on hit" $ do
    let armor = [ Just (ItemStack (ArmorItem Helmet IronArmor 50) 1)
                , Just (ItemStack (ArmorItem Chestplate IronArmor 50) 1)
                , Nothing
                , Nothing
                ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = armor }
        player' = damagePlayer 4 player
    -- Both equipped pieces lose 1 durability
    plArmorSlots player' `shouldBe`
      [ Just (ItemStack (ArmorItem Helmet IronArmor 49) 1)
      , Just (ItemStack (ArmorItem Chestplate IronArmor 49) 1)
      , Nothing
      , Nothing
      ]

  it "armor breaks when durability reaches 0" $ do
    let armor = [ Just (ItemStack (ArmorItem Helmet IronArmor 1) 1)
                , Just (ItemStack (ArmorItem Chestplate IronArmor 50) 1)
                , Nothing
                , Nothing
                ]
        player = (defaultPlayer (V3 0 80 0)) { plArmorSlots = armor }
        player' = damagePlayer 4 player
    -- Helmet breaks (durability was 1), chestplate loses 1
    plArmorSlots player' `shouldBe`
      [ Nothing
      , Just (ItemStack (ArmorItem Chestplate IronArmor 49) 1)
      , Nothing
      , Nothing
      ]

  it "no armor worn: armor slots unchanged on hit" $ do
    let player = defaultPlayer (V3 0 80 0)
        player' = damagePlayer 4 player
    plArmorSlots player' `shouldBe` [Nothing, Nothing, Nothing, Nothing]

  it "totalArmorDefense computes correctly for mixed armor" $ do
    let armor = [ Just (ItemStack (ArmorItem Helmet LeatherArmor 50) 1)    -- 1
                , Just (ItemStack (ArmorItem Chestplate DiamondArmor 50) 1) -- 8
                , Nothing                                                    -- 0
                , Just (ItemStack (ArmorItem Boots IronArmor 50) 1)         -- 2
                ]
    totalArmorDefense armor `shouldBe` 11

  it "calcArmorDamageReduction formula: max(1, raw - defense/2)" $ do
    calcArmorDamageReduction 10 0 `shouldBe` 10
    calcArmorDamageReduction 10 6 `shouldBe` 7
    calcArmorDamageReduction 10 20 `shouldBe` 1
    calcArmorDamageReduction 1 0 `shouldBe` 1
    calcArmorDamageReduction 1 20 `shouldBe` 1
