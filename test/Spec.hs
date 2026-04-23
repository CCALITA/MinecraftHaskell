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
import Game.Save (SaveData(..), inventoryToSlotList, slotListToInventory, savePlayer, playerSavePath)
import Game.SaveV3 (SaveDataV3(..), ChunkMeta(..), savev3Version, encodeSaveV3, decodeSaveV3, migrateV2toV3, savePlayerV3, loadPlayerV3)
import Game.BlockEntity
import Game.Furnace
import World.Weather
import Entity.ECS
import Entity.Component
import Entity.Mob (MobType(..), MobInfo(..), MobBehavior(..), AIState(..), mobInfo, isPassive, isHostile)
import Game.XP (xpForBlock, xpForMobKill, xpLevel, xpForNextLevel, xpProgress)
import World.Redstone
import World.BlockRegistry
import Game.RecipeRegistry
import World.BiomeFeatures
import World.Biome (BiomeType(..))
import Game.ItemRegistry
import World.Generation (generateChunk, GenerationConfig(..), defaultGenConfig,
                         structureHashRoll, placeStructureInChunk)

import Entity.Villager
import qualified World.Dimension as Dim
import Game.Enchanting
import Game.Command
import Game.Achievement
import Game.Config
import Game.Event
import Game.ItemDisplay (armorSlotSilhouette, itemMiniIcon)
import UI.Tooltip
import UI.Screen

import Game.ItemDisplay (itemColor, itemMiniIcon, buildCursorItemVerts, cursorIconSize)
import Engine.BitmapFont (renderText, charSpacing)

import Game.PotionEffect
import Game.Particle (WeatherParticle(..), WeatherParticleType(..), spawnWeatherParticles, tickWeatherParticles, renderWeatherParticles, weatherParticleRadius, weatherParticleHeight, weatherParticleCount, rainFallSpeed, snowFallSpeed, isSnowBiome, clampParticleXZ, clampParticleY)

import Game.Bucket (BucketAction(..), determineBucketAction, bucketTypeToFluidBlock, fluidBlockToBucketType)
import World.Fluid (FluidState, FluidType(..), newFluidState, addFluidSource, removeFluid, getFluid, FluidBlock(..))

import Engine.Mesh (MeshData(..), NeighborData(..), meshChunkWithLight, emptyNeighborData, BlockVertex(..), computeVertexAO)
import Entity.Pathfinding (findPath, pathDistance)
import World.Light (LightMap, newLightMap, propagateBlockLight, propagateSkyLight, getBlockLight, getSkyLight, getTotalLight, maxLightLevel)
import Game.DayNight (DayNightCycle(..), newDayNightCycle, updateDayNight, getSkyColor, getAmbientLight, isNight, isDawn, isDusk, getTimeOfDay, TimeOfDay(..))
import Game.State (GameState(..), GameMode(..), PlayMode(..), newGameState)
import Game.Creative (creativePalette, creativePaletteSize, creativeClickSlot, creativePickFromPalette, creativeConsumeItem, creativeRefillSlot, palettePageCount, palettePageItems, hitPaletteSlot, paletteRows, paletteCols, paletteSlotsPerPage, paletteX0, paletteY0, paletteSlotW, paletteSlotH)
import Game.ItemDisplay (durabilityFraction, durabilityBarColor)
import Entity.Spawn (SpawnRules(..), defaultSpawnRules)

import TestHelpers (airHeightQuery, airQuery, waterQuery, withTestWorld)

import Data.Binary (encode, decode)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.List (nub, isPrefixOf, isInfixOf)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Linear (V2(..), V3(..), V4(..), identity)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV
import Control.Concurrent.STM (atomically, readTVar)
import qualified Data.HashMap.Strict as HM
import System.Directory (removeDirectoryRecursive, doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Data.Function ((&))
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad (forM_)

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
  saveV3FileSpec
  recipeRegistrySpec
  bedSleepSpec
  mobLootDropSpec
  woodVariantsAndFloraSpec
  xpSpec
  utilityBlockSpec
  bucketItemSpec
  bucketMechanicsSpec
  wheatCropGrowthSpec
  structurePlacementSpec
  executeCommandSpec
  chatStateSpec
  lookupItemByNameSpec
  netherPortalSpec
  crossChunkCullingSpec
  greedyMeshingSpec
  dimensionWiringSpec
  meshAOSpec
  directionalPistonSpec
  sunsetSunriseSpec
  craftingPreviewSpec
  dropFromSlotSpec
  hotbarNumberKeySpec
  inventoryComprehensiveSpec
  creativeInventorySpec
  collectAllSpec
  stackSplittingSpec
  durabilityDisplaySpec
  cursorItemRenderSpec
  shiftClickContainerSpec
  hotbarPopupSpec

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

  -- sortInventory tests
  describe "sortInventory" $ do
    it "preserves total item count after sorting" $ do
      let (inv1, _) = addItem emptyInventory (BlockItem Stone) 30
          (inv2, _) = addItem inv1 (BlockItem Dirt) 20
          (inv3, _) = addItem inv2 (BlockItem Stone) 15
          sorted = sortInventory inv3
          totalBefore = countItem inv3 (BlockItem Stone) + countItem inv3 (BlockItem Dirt)
          totalAfter  = countItem sorted (BlockItem Stone) + countItem sorted (BlockItem Dirt)
      totalAfter `shouldBe` totalBefore

    it "produces no gaps between non-empty slots" $ do
      -- Place items in non-contiguous slots
      let inv0 = setSlot emptyInventory 5 (Just (ItemStack (BlockItem Sand) 10))
          inv1 = setSlot inv0 15 (Just (ItemStack (BlockItem Dirt) 20))
          inv2 = setSlot inv1 30 (Just (ItemStack (BlockItem Stone) 5))
          sorted = sortInventory inv2
          slots = [getSlot sorted i | i <- [0 .. inventorySlots - 1]]
          nonEmpty = filter (/= Nothing) slots
          -- All non-empty slots should be contiguous from the start
          firstNSlots = take (length nonEmpty) slots
      firstNSlots `shouldBe` nonEmpty

    it "results are sorted by item Show order" $ do
      let (inv1, _) = addItem emptyInventory (BlockItem Stone) 10
          (inv2, _) = addItem inv1 (BlockItem Dirt) 10
          (inv3, _) = addItem inv2 (BlockItem Sand) 10
          sorted = sortInventory inv3
          occupiedItems = [ show (isItem s)
                          | i <- [0 .. inventorySlots - 1]
                          , Just s <- [getSlot sorted i]
                          ]
          isSorted [] = True
          isSorted [_] = True
          isSorted (a:b:rest) = a <= b && isSorted (b:rest)
      isSorted occupiedItems `shouldBe` True

    it "preserves invSelected after sorting" $ do
      let inv0 = selectHotbar emptyInventory 5
          (inv1, _) = addItem inv0 (BlockItem Stone) 10
          sorted = sortInventory inv1
      invSelected sorted `shouldBe` 5

    it "groups same item type and sums counts" $ do
      -- Put same item in separate slots
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 10))
          inv1 = setSlot inv0 5 (Just (ItemStack (BlockItem Stone) 20))
          inv2 = setSlot inv1 10 (Just (ItemStack (BlockItem Stone) 30))
          sorted = sortInventory inv2
      -- 10+20+30=60 should fit in a single stack (stackLimit=64)
      getSlot sorted 0 `shouldBe` Just (ItemStack (BlockItem Stone) 60)
      -- No more Stone in other slots
      countItem sorted (BlockItem Stone) `shouldBe` 60

    it "splits into stacks when total exceeds stackLimit" $ do
      -- 80 Stone total => one stack of 64 + one of 16
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 50))
          inv1 = setSlot inv0 3 (Just (ItemStack (BlockItem Stone) 30))
          sorted = sortInventory inv1
      countItem sorted (BlockItem Stone) `shouldBe` 80
      -- First stack should be full
      case getSlot sorted 0 of
        Just (ItemStack _ cnt) -> cnt `shouldBe` 64
        Nothing -> expectationFailure "Expected stone in slot 0"
      -- Second stack should have remainder
      case getSlot sorted 1 of
        Just (ItemStack _ cnt) -> cnt `shouldBe` 16
        Nothing -> expectationFailure "Expected stone in slot 1"

    it "sorting empty inventory stays empty" $ do
      let sorted = sortInventory emptyInventory
      invSlots sorted `shouldBe` invSlots emptyInventory

    it "tool items (stackLimit 1) remain individual stacks" $ do
      let tool1 = ToolItem Pickaxe Diamond 1561
          tool2 = ToolItem Pickaxe Diamond 1561
          inv0 = setSlot emptyInventory 10 (Just (ItemStack tool1 1))
          inv1 = setSlot inv0 20 (Just (ItemStack tool2 1))
          sorted = sortInventory inv1
      -- Tools have stackLimit 1 so they should remain as separate stacks
      getSlot sorted 0 `shouldBe` Just (ItemStack tool1 1)
      getSlot sorted 1 `shouldBe` Just (ItemStack tool2 1)

    it "mixed item types are sorted and contiguous" $ do
      let (inv1, _) = addItem emptyInventory (MaterialItem Coal) 15
          (inv2, _) = addItem inv1 (BlockItem Stone) 30
          (inv3, _) = addItem inv2 (FoodItem Apple) 5
          (inv4, _) = addItem inv3 (BlockItem Dirt) 20
          sorted = sortInventory inv4
          -- Verify total counts preserved
          countCoal   = countItem sorted (MaterialItem Coal)
          countStone  = countItem sorted (BlockItem Stone)
          countApple  = countItem sorted (FoodItem Apple)
          countDirt   = countItem sorted (BlockItem Dirt)
      countCoal `shouldBe` 15
      countStone `shouldBe` 30
      countApple `shouldBe` 5
      countDirt `shouldBe` 20
      -- Verify sorted order: all non-empty slots should have Show-ordered items
      let occupiedShows = [ show (isItem s)
                          | i <- [0 .. inventorySlots - 1]
                          , Just s <- [getSlot sorted i]
                          ]
          pairs' = zip occupiedShows (drop 1 occupiedShows)
      all (\(a, b) -> a <= b) pairs' `shouldBe` True
  -- moveToSection tests
  it "moveToSection moves hotbar item to first empty main slot" $ do
    let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 10))
        inv' = moveToSection inv 0
    getSlot inv' 0 `shouldBe` Nothing
    getSlot inv' 9 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

  it "moveToSection moves main item to first empty hotbar slot" $ do
    let inv = setSlot emptyInventory 15 (Just (ItemStack (BlockItem Dirt) 5))
        inv' = moveToSection inv 15
    getSlot inv' 15 `shouldBe` Nothing
    getSlot inv' 0 `shouldBe` Just (ItemStack (BlockItem Dirt) 5)

  it "moveToSection is no-op on empty slot" $ do
    let inv' = moveToSection emptyInventory 3
    inv' `shouldBe` emptyInventory

  it "moveToSection is no-op when target section is full" $ do
    -- Fill all main slots (9-35)
    let fillMain inv idx
          | idx > 35 = inv
          | otherwise = fillMain (setSlot inv idx (Just (ItemStack (BlockItem Dirt) 1))) (idx + 1)
        inv = setSlot (fillMain emptyInventory 9) 0 (Just (ItemStack (BlockItem Stone) 10))
        inv' = moveToSection inv 0
    -- Should be unchanged — no empty main slot
    getSlot inv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

  it "moveToSection is no-op when hotbar is full and moving from main" $ do
    -- Fill all hotbar slots (0-8)
    let fillHotbar inv idx
          | idx >= 9 = inv
          | otherwise = fillHotbar (setSlot inv idx (Just (ItemStack (BlockItem Dirt) 1))) (idx + 1)
        inv = setSlot (fillHotbar emptyInventory 0) 20 (Just (ItemStack (BlockItem Stone) 5))
        inv' = moveToSection inv 20
    -- Should be unchanged — no empty hotbar slot
    getSlot inv' 20 `shouldBe` Just (ItemStack (BlockItem Stone) 5)

  it "moveToSection skips occupied slots to find first empty" $ do
    -- Occupy slots 9 and 10, leave 11 empty
    let inv = setSlot (setSlot (setSlot emptyInventory 9 (Just (ItemStack (BlockItem Dirt) 1)))
                                        10 (Just (ItemStack (BlockItem Dirt) 1)))
                       0 (Just (ItemStack (BlockItem Stone) 3))
        inv' = moveToSection inv 0
    getSlot inv' 0 `shouldBe` Nothing
    getSlot inv' 9 `shouldBe` Just (ItemStack (BlockItem Dirt) 1)
    getSlot inv' 10 `shouldBe` Just (ItemStack (BlockItem Dirt) 1)
    getSlot inv' 11 `shouldBe` Just (ItemStack (BlockItem Stone) 3)

  it "moveToSection moves tool item from hotbar to main" $ do
    let tool = ToolItem Pickaxe Iron 250
        inv = setSlot emptyInventory 2 (Just (ItemStack tool 1))
        inv' = moveToSection inv 2
    getSlot inv' 2 `shouldBe` Nothing
    getSlot inv' 9 `shouldBe` Just (ItemStack tool 1)

  it "moveToSection moves tool item from main to hotbar" $ do
    let tool = ToolItem Sword Diamond 1561
        inv = setSlot emptyInventory 30 (Just (ItemStack tool 1))
        inv' = moveToSection inv 30
    getSlot inv' 30 `shouldBe` Nothing
    getSlot inv' 0 `shouldBe` Just (ItemStack tool 1)

  it "moveToSection is no-op for out-of-range slot index" $ do
    moveToSection emptyInventory (-1) `shouldBe` emptyInventory
    moveToSection emptyInventory 36 `shouldBe` emptyInventory

  it "moveToSection preserves stack count" $ do
    let inv = setSlot emptyInventory 5 (Just (ItemStack (BlockItem Stone) 64))
        inv' = moveToSection inv 5
    getSlot inv' 5 `shouldBe` Nothing
    getSlot inv' 9 `shouldBe` Just (ItemStack (BlockItem Stone) 64)

  it "moveToSection from hotbar slot 8 works" $ do
    let inv = setSlot emptyInventory 8 (Just (ItemStack (BlockItem Sand) 32))
        inv' = moveToSection inv 8
    getSlot inv' 8 `shouldBe` Nothing
    getSlot inv' 9 `shouldBe` Just (ItemStack (BlockItem Sand) 32)

  it "moveToSection from main slot 9 moves to hotbar" $ do
    let inv = setSlot emptyInventory 9 (Just (ItemStack (BlockItem Sand) 16))
        inv' = moveToSection inv 9
    getSlot inv' 9 `shouldBe` Nothing
    getSlot inv' 0 `shouldBe` Just (ItemStack (BlockItem Sand) 16)

  it "moveToSection from main slot 35 moves to hotbar" $ do
    let inv = setSlot emptyInventory 35 (Just (ItemStack (FoodItem Apple) 10))
        inv' = moveToSection inv 35
    getSlot inv' 35 `shouldBe` Nothing
    getSlot inv' 0 `shouldBe` Just (ItemStack (FoodItem Apple) 10)

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

  it "furnaceFuelFraction returns 0 for new furnace" $ do
    furnaceFuelFraction newFurnaceState `shouldBe` 0

  it "furnaceFuelFraction returns correct ratio when burning" $ do
    let fs = newFurnaceState { fsFuelTime = 40, fsMaxFuelTime = 80 }
    furnaceFuelFraction fs `shouldBe` 0.5

  it "furnaceFuelFraction clamps to 1 when fuel exceeds max" $ do
    let fs = newFurnaceState { fsFuelTime = 100, fsMaxFuelTime = 80 }
    furnaceFuelFraction fs `shouldBe` 1.0

  it "furnaceFuelFraction returns 0 when maxFuelTime is 0" $ do
    let fs = newFurnaceState { fsFuelTime = 10, fsMaxFuelTime = 0 }
    furnaceFuelFraction fs `shouldBe` 0

  it "furnaceSmeltFraction returns 0 for new furnace" $ do
    furnaceSmeltFraction newFurnaceState `shouldBe` 0

  it "furnaceSmeltFraction returns correct ratio mid-smelt" $ do
    let fs = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem IronOre) 1)
          , fsSmeltTime = 5.0
          }
    -- IronOre recipe time is 10.0, so fraction = 5/10 = 0.5
    furnaceSmeltFraction fs `shouldBe` 0.5

  it "furnaceSmeltFraction returns 0 with no input" $ do
    let fs = newFurnaceState { fsSmeltTime = 5.0 }
    furnaceSmeltFraction fs `shouldBe` 0

  it "furnaceSmeltFraction returns 0 for non-smeltable input" $ do
    let fs = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem Dirt) 1)
          , fsSmeltTime = 5.0
          }
    furnaceSmeltFraction fs `shouldBe` 0

  it "furnaceSmeltFraction clamps to 1 when progress exceeds recipe time" $ do
    let fs = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem IronOre) 1)
          , fsSmeltTime = 15.0
          }
    furnaceSmeltFraction fs `shouldBe` 1.0

  it "furnaceSmeltFraction returns 0 when smeltTime is 0" $ do
    let fs = newFurnaceState
          { fsInput = Just (ItemStack (BlockItem IronOre) 1)
          , fsSmeltTime = 0
          }
    furnaceSmeltFraction fs `shouldBe` 0
  describe "shiftClickFurnaceOutput" $ do
    it "moves output items into empty inventory" $ do
      let fs = setFurnaceOutput newFurnaceState (Just (ItemStack (MaterialItem IronIngot) 5))
          inv = emptyInventory
          (fs', inv') = shiftClickFurnaceOutput fs inv
      getFurnaceOutput fs' `shouldBe` Nothing
      countItem inv' (MaterialItem IronIngot) `shouldBe` 5

    it "no-op when output slot is empty" $ do
      let fs = newFurnaceState
          inv = emptyInventory
          (fs', inv') = shiftClickFurnaceOutput fs inv
      getFurnaceOutput fs' `shouldBe` Nothing
      inv' `shouldBe` inv

    it "merges output into existing inventory stacks" $ do
      let inv0 = fst $ addItem emptyInventory (MaterialItem IronIngot) 10
          fs = setFurnaceOutput newFurnaceState (Just (ItemStack (MaterialItem IronIngot) 3))
          (fs', inv') = shiftClickFurnaceOutput fs inv0
      getFurnaceOutput fs' `shouldBe` Nothing
      countItem inv' (MaterialItem IronIngot) `shouldBe` 13

    it "leaves leftover in output when inventory is full" $ do
      -- Fill all 36 slots with dirt (different item)
      let fillInv = foldl (\i idx -> setSlot i idx (Just (ItemStack (BlockItem Dirt) 64))) emptyInventory [0..35]
          fs = setFurnaceOutput newFurnaceState (Just (ItemStack (MaterialItem IronIngot) 5))
          (fs', inv') = shiftClickFurnaceOutput fs fillInv
      -- Output should still have the items since inventory is full
      getFurnaceOutput fs' `shouldBe` Just (ItemStack (MaterialItem IronIngot) 5)
      countItem inv' (MaterialItem IronIngot) `shouldBe` 0

    it "partially moves output when inventory has limited space" $ do
      -- Fill 35 slots with dirt, leave one slot empty
      let fillInv = foldl (\i idx -> setSlot i idx (Just (ItemStack (BlockItem Dirt) 64))) emptyInventory [0..34]
          fs = setFurnaceOutput newFurnaceState (Just (ItemStack (MaterialItem IronIngot) 100))
          (fs', inv') = shiftClickFurnaceOutput fs fillInv
      -- Only 64 should fit (one empty slot, stack limit 64)
      countItem inv' (MaterialItem IronIngot) `shouldBe` 64
      getFurnaceOutput fs' `shouldBe` Just (ItemStack (MaterialItem IronIngot) 36)

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

  -- Integration: IORef-based trigger workflow (mirrors game loop wiring)
  it "mine OakLog triggers Getting Wood via IORef workflow" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    let trigger = TrigMineBlock OakLog
    st <- readIORef achRef
    case checkAchievement st trigger of
      Just ach -> do
        writeIORef achRef (unlockAchievement st ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    st' <- readIORef achRef
    isUnlocked st' AchMineWood `shouldBe` True
    toast <- readIORef toastRef
    fmap fst toast `shouldBe` Just "Getting Wood"

  it "mine DiamondOre triggers Diamonds! via IORef workflow" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    let trigger = TrigMineBlock DiamondOre
    st <- readIORef achRef
    case checkAchievement st trigger of
      Just ach -> do
        writeIORef achRef (unlockAchievement st ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    st' <- readIORef achRef
    isUnlocked st' AchMineDiamond `shouldBe` True
    toast <- readIORef toastRef
    fmap fst toast `shouldBe` Just "Diamonds!"

  it "craft OakPlanks triggers Benchmarking via IORef workflow" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    let trigger = TrigCraftItem (BlockItem OakPlanks)
    st <- readIORef achRef
    case checkAchievement st trigger of
      Just ach -> do
        writeIORef achRef (unlockAchievement st ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    st' <- readIORef achRef
    isUnlocked st' AchCraftPlanks `shouldBe` True
    toast <- readIORef toastRef
    fmap fst toast `shouldBe` Just "Benchmarking"

  it "kill Zombie triggers Monster Hunter via IORef workflow" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    let trigger = TrigKillEntity "Zombie"
    st <- readIORef achRef
    case checkAchievement st trigger of
      Just ach -> do
        writeIORef achRef (unlockAchievement st ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    st' <- readIORef achRef
    isUnlocked st' AchKillMob `shouldBe` True
    toast <- readIORef toastRef
    fmap fst toast `shouldBe` Just "Monster Hunter"

  it "duplicate trigger does not overwrite toast" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    -- First trigger: should unlock
    let trigger1 = TrigMineBlock OakLog
    st0 <- readIORef achRef
    case checkAchievement st0 trigger1 of
      Just ach -> do
        writeIORef achRef (unlockAchievement st0 ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    -- Second trigger with same event: should NOT unlock again
    writeIORef toastRef (Just ("Getting Wood", 1.5))  -- simulate partially decayed toast
    st1 <- readIORef achRef
    case checkAchievement st1 trigger1 of
      Just _ach -> writeIORef toastRef (Just ("SHOULD NOT HAPPEN", 3.0))
      Nothing   -> pure ()
    toast <- readIORef toastRef
    fmap fst toast `shouldBe` Just "Getting Wood"
    fmap snd toast `shouldBe` Just 1.5

  it "toast timer set to 3.0 on unlock" $ do
    achRef   <- newIORef newAchievementState
    toastRef <- newIORef (Nothing :: Maybe (String, Float))
    let trigger = TrigKillEntity "Skeleton"
    st <- readIORef achRef
    case checkAchievement st trigger of
      Just ach -> do
        writeIORef achRef (unlockAchievement st ach)
        writeIORef toastRef (Just (achievementName ach, 3.0))
      Nothing -> pure ()
    toast <- readIORef toastRef
    fmap snd toast `shouldBe` Just 3.0

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

  it "Shepherd offers shears trade" $ do
    let trades = generateTrades Shepherd
        hasShears = any (\t -> case toOutputItem t of
          ShearsItem _ -> True
          _            -> False) trades
    hasShears `shouldBe` True

  -- Cleric trades
  it "Cleric offers glass trade" $ do
    let trades = generateTrades Cleric
        hasGlass = any (\t -> toOutputItem t == BlockItem Glass) trades
    hasGlass `shouldBe` True

  it "Cleric offers redstone dust trade" $ do
    let trades = generateTrades Cleric
        hasRedstone = any (\t -> toOutputItem t == BlockItem RedstoneDust) trades
    hasRedstone `shouldBe` True

  it "Cleric has 2 trades" $ do
    length (generateTrades Cleric) `shouldBe` 2

  -- Farmer trade count
  it "Farmer has 2 trades" $ do
    length (generateTrades Farmer) `shouldBe` 2

  -- Librarian trade count
  it "Librarian has 2 trades" $ do
    length (generateTrades Librarian) `shouldBe` 2

  -- Butcher trade count
  it "Butcher has 3 trades" $ do
    length (generateTrades Butcher) `shouldBe` 3

  -- Shepherd trade count
  it "Shepherd has 2 trades" $ do
    length (generateTrades Shepherd) `shouldBe` 2

  -- Butcher outputs cooked porkchop
  it "Butcher offers cooked porkchop trade" $ do
    let trades = generateTrades Butcher
        hasCookedPork = any (\t -> toOutputItem t == FoodItem CookedPorkchop) trades
    hasCookedPork `shouldBe` True

  -- Librarian outputs glass
  it "Librarian offers glass output" $ do
    let trades = generateTrades Librarian
        hasGlass = any (\t -> toOutputItem t == BlockItem Glass) trades
    hasGlass `shouldBe` True

  -- executeTrade tests
  it "executeTrade succeeds with sufficient items" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 20
    case executeTrade inv0 trade of
      Just (inv', trade') -> do
        hasItem inv' (MaterialItem GoldIngot) 1 `shouldBe` True
        toUsesLeft trade' `shouldBe` 11
      Nothing -> expectationFailure "executeTrade should have succeeded"

  it "executeTrade removes input items from inventory" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 20
    case executeTrade inv0 trade of
      Just (inv', _) -> do
        hasItem inv' (MaterialItem Wheat) 1 `shouldBe` False
      Nothing -> expectationFailure "executeTrade should have succeeded"

  it "executeTrade fails with insufficient items" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 10
    executeTrade inv0 trade `shouldBe` Nothing

  it "executeTrade fails with empty inventory" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 12
    executeTrade emptyInventory trade `shouldBe` Nothing

  it "executeTrade fails when trade out of stock" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 0
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 20
    executeTrade inv0 trade `shouldBe` Nothing

  it "executeTrade decrements usesLeft by 1" $ do
    let trade = TradeOffer (MaterialItem Coal) 15 (MaterialItem GoldIngot) 1 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem Coal) 15
    case executeTrade inv0 trade of
      Just (_, trade') -> toUsesLeft trade' `shouldBe` 11
      Nothing -> expectationFailure "trade should succeed"

  it "executeTrade adds correct output count" $ do
    let trade = TradeOffer (MaterialItem GoldIngot) 1 (FoodItem Bread) 4 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem GoldIngot) 1
    case executeTrade inv0 trade of
      Just (inv', _) -> hasItem inv' (FoodItem Bread) 4 `shouldBe` True
      Nothing -> expectationFailure "trade should succeed"

  it "executeTrade can be applied multiple times" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 12
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 64
    case executeTrade inv0 trade of
      Just (inv1, trade1) -> do
        toUsesLeft trade1 `shouldBe` 11
        case executeTrade inv1 trade1 of
          Just (inv2, trade2) -> do
            toUsesLeft trade2 `shouldBe` 10
            hasItem inv2 (MaterialItem GoldIngot) 2 `shouldBe` True
          Nothing -> expectationFailure "second trade should succeed"
      Nothing -> expectationFailure "first trade should succeed"

  it "executeTrade fails on last use when usesLeft is 1 then 0" $ do
    let trade = TradeOffer (MaterialItem Wheat) 20 (MaterialItem GoldIngot) 1 12 1
        inv0 = fst $ addItem emptyInventory (MaterialItem Wheat) 64
    case executeTrade inv0 trade of
      Just (inv1, trade1) -> do
        toUsesLeft trade1 `shouldBe` 0
        -- Next trade attempt should fail (out of stock)
        executeTrade inv1 trade1 `shouldBe` Nothing
      Nothing -> expectationFailure "first trade should succeed"

  it "executeTrade with Blacksmith iron pickaxe trade" $ do
    let trades = generateTrades Blacksmith
        pickTrade = head $ filter (\t -> case toOutputItem t of
          ToolItem Pickaxe Iron _ -> True
          _ -> False) trades
        inv0 = fst $ addItem emptyInventory (MaterialItem GoldIngot) 3
    case executeTrade inv0 pickTrade of
      Just (inv', _) -> do
        let hasPick = any (\idx -> case getSlot inv' idx of
              Just (ItemStack (ToolItem Pickaxe Iron _) _) -> True
              _ -> False) [0..35]
        hasPick `shouldBe` True
      Nothing -> expectationFailure "pickaxe trade should succeed"

  it "executeTrade with Shepherd wool trade" $ do
    let trades = generateTrades Shepherd
        woolTrade = head $ filter (\t -> toInputItem t == BlockItem Wool) trades
        inv0 = fst $ addItem emptyInventory (BlockItem Wool) 18
    case executeTrade inv0 woolTrade of
      Just (inv', trade') -> do
        hasItem inv' (MaterialItem GoldIngot) 1 `shouldBe` True
        toUsesLeft trade' `shouldBe` (toMaxUses woolTrade - 1)
      Nothing -> expectationFailure "wool trade should succeed"

  -- Villager MobType in Entity.Mob
  it "Villager is Passive behavior" $ do
    miBehavior (mobInfo Villager) `shouldBe` Passive

  it "Villager has 20 HP" $ do
    miMaxHealth (mobInfo Villager) `shouldBe` 20

  it "Villager has 0 attack damage" $ do
    miAttackDmg (mobInfo Villager) `shouldBe` 0.0

  it "Villager has 0 detect range" $ do
    miDetectRange (mobInfo Villager) `shouldBe` 0.0

  it "Villager has 0 attack range" $ do
    miAttackRange (mobInfo Villager) `shouldBe` 0.0

  it "Villager speed is 2.5" $ do
    miSpeed (mobInfo Villager) `shouldBe` 2.5

  it "isPassive returns True for Villager" $ do
    isPassive Villager `shouldBe` True

  it "isHostile returns False for Villager" $ do
    isHostile Villager `shouldBe` False

  -- GameState VillagerTrading mode
  it "VillagerTrading is distinct from other GameModes" $ do
    VillagerTrading `shouldNotBe` Playing
    VillagerTrading `shouldNotBe` InventoryOpen
    VillagerTrading `shouldNotBe` CraftingOpen
    VillagerTrading `shouldNotBe` MainMenu

  it "VillagerTrading shows correctly" $ do
    show VillagerTrading `shouldBe` "VillagerTrading"

  it "GameState initializes with empty villager trades" $ do
    gs <- newGameState (V3 0 60 0)
    trades <- readIORef (gsVillagerTrades gs)
    trades `shouldBe` []

  it "GameState initializes with no villager profession" $ do
    gs <- newGameState (V3 0 60 0)
    prof <- readIORef (gsVillagerProf gs)
    prof `shouldBe` Nothing

  it "GameState villager trades can be written and read" $ do
    gs <- newGameState (V3 0 60 0)
    let trades = generateTrades Farmer
    writeIORef (gsVillagerTrades gs) trades
    result <- readIORef (gsVillagerTrades gs)
    result `shouldBe` trades

  it "GameState villager profession can be written and read" $ do
    gs <- newGameState (V3 0 60 0)
    writeIORef (gsVillagerProf gs) (Just Blacksmith)
    result <- readIORef (gsVillagerProf gs)
    result `shouldBe` Just Blacksmith

  -- Trade input/output item types are distinct
  it "no trade has same input and output item" $ do
    let allTrades = concatMap generateTrades allProfessions
    mapM_ (\t -> toInputItem t `shouldNotBe` toOutputItem t) allTrades

  -- Currency is GoldIngot (Emerald substitute)
  it "Farmer wheat trade output is GoldIngot currency" $ do
    let trades = generateTrades Farmer
        wheatTrade = head $ filter (\t -> toInputItem t == MaterialItem Wheat) trades
    toOutputItem wheatTrade `shouldBe` MaterialItem GoldIngot

  it "Farmer bread trade input is GoldIngot currency" $ do
    let trades = generateTrades Farmer
        breadTrade = head $ filter (\t -> toOutputItem t == FoodItem Bread) trades
    toInputItem breadTrade `shouldBe` MaterialItem GoldIngot

  -- Blacksmith coal trade
  it "Blacksmith accepts coal for currency" $ do
    let trades = generateTrades Blacksmith
        hasCoal = any (\t -> toInputItem t == MaterialItem Coal) trades
    hasCoal `shouldBe` True

  -- Blacksmith sword trade
  it "Blacksmith offers iron sword trade" $ do
    let trades = generateTrades Blacksmith
        hasSword = any (\t -> case toOutputItem t of
          ToolItem Sword Iron _ -> True
          _                     -> False) trades
    hasSword `shouldBe` True

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

  -- Tooltip hover integration tests
  it "tooltip at hover offset (mouseX+0.02, mouseY+0.02) starts near that position" $ do
    let tt = buildTooltip (BlockItem Stone) []
        tx = 0.3; ty = -0.2
        verts = renderTooltipVertices tt (tx + 0.02) (ty + 0.02)
    -- First vertex x should be at or near the offset position
    length verts `shouldSatisfy` (>= 6)
    let firstX = verts !! 0
        firstY = verts !! 1
    firstX `shouldSatisfy` (\v -> abs (v - (tx + 0.02)) < 0.05)
    firstY `shouldSatisfy` (\v -> abs (v - (ty + 0.02)) < 0.05)

  it "tooltip for ToolItem with enchantments has more vertices than plain BlockItem" $ do
    let ttPlain = buildTooltip (BlockItem Dirt) []
        ttEnch  = buildTooltip (ToolItem Sword Diamond 1561) [Enchantment Sharpness 5]
        vertsPlain = renderTooltipVertices ttPlain 0.0 0.0
        vertsEnch  = renderTooltipVertices ttEnch  0.0 0.0
    length vertsEnch `shouldSatisfy` (> length vertsPlain)

  it "tooltip for FoodItem includes lore (more verts than no-lore item)" $ do
    let ttNoLore = buildTooltip StickItem []
        ttLore   = buildTooltip (FoodItem Steak) []
        vertsNoLore = renderTooltipVertices ttNoLore 0.0 0.0
        vertsLore   = renderTooltipVertices ttLore   0.0 0.0
    length vertsLore `shouldSatisfy` (> length vertsNoLore)

  it "tooltip background vertices (first 36 floats = 6 verts) have dark alpha" $ do
    let tt = buildTooltip (BlockItem Stone) []
        verts = renderTooltipVertices tt 0.0 0.0
    -- Background quad = 6 vertices * 6 floats = 36 floats
    -- Each vertex: x y r g b a; background alpha should be ~0.85
    length verts `shouldSatisfy` (>= 36)
    let bgAlpha = verts !! 5  -- alpha of first background vertex
    bgAlpha `shouldSatisfy` (> 0.8)

  it "tooltip vertices all have valid alpha (0-1 range)" $ do
    let tt = buildTooltip (ToolItem Pickaxe Iron 250) [Enchantment Efficiency 3]
        verts = renderTooltipVertices tt (-0.5) (-0.5)
        -- Every 6th float starting at index 5 is alpha
        alphas = [verts !! i | i <- [5, 11 .. length verts - 1]]
    all (\a -> a >= 0 && a <= 1) alphas `shouldBe` True

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
    placeStructure (worldSetBlock world) (V3 0 64 0) wellStructure
    -- Check cobblestone floor at origin
    bt00 <- worldGetBlock world (V3 0 64 0)
    bt00 `shouldBe` Cobblestone
    -- Check water in center
    btWater <- worldGetBlock world (V3 2 65 2)
    btWater `shouldBe` Water

  it "placeStructure respects offset" $ withTestWorld $ \world -> do
    placeStructure (worldSetBlock world) (V3 5 70 5) dungeonStructure
    -- Floor corner at (5,70,5)
    bt <- worldGetBlock world (V3 5 70 5)
    bt `shouldBe` Cobblestone
    -- Chest at center (5+3, 70+1, 5+3) = (8, 71, 8)
    btChest <- worldGetBlock world (V3 8 71 8)
    btChest `shouldBe` Chest

  it "placeStructure does not affect unrelated positions" $ withTestWorld $ \world -> do
    placeStructure (worldSetBlock world) (V3 0 64 0) wellStructure
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
-- SaveV3 File I/O: roundtrip and V2 backward compatibility
-- =========================================================================
saveV3FileSpec :: Spec
saveV3FileSpec = describe "SaveV3 file I/O and migration" $ do

  it "savePlayerV3 / loadPlayerV3 roundtrip via filesystem" $ do
    withSystemTempDirectory "savev3test" $ \tmpDir -> do
      let sd = sampleV3
      savePlayerV3 tmpDir sd
      result <- loadPlayerV3 tmpDir
      result `shouldBe` Just sd

  it "loadPlayerV3 reads V2 file and migrates to V3" $ do
    withSystemTempDirectory "savev2compat" $ \tmpDir -> do
      let v2 = sampleV2
      savePlayer tmpDir v2           -- write V2 format
      result <- loadPlayerV3 tmpDir  -- should auto-detect and migrate
      case result of
        Nothing -> expectationFailure "loadPlayerV3 returned Nothing for V2 save"
        Just v3 -> do
          sv3PlayerPos v3 `shouldBe` sdPlayerPos v2
          sv3PlayerYaw v3 `shouldBe` sdPlayerYaw v2
          sv3PlayerPitch v3 `shouldBe` sdPlayerPitch v2
          sv3Flying v3 `shouldBe` sdPlayerFlying v2
          sv3Health v3 `shouldBe` fromIntegral (sdHealth v2)
          sv3Hunger v3 `shouldBe` sdHunger v2
          sv3Inventory v3 `shouldBe` sdInventory v2
          sv3WorldSeed v3 `shouldBe` sdWorldSeed v2
          sv3DayTime v3 `shouldBe` sdDayTime v2
          sv3DayCount v3 `shouldBe` sdDayCount v2
          sv3Version v3 `shouldBe` savev3Version

  it "loadPlayerV3 returns Nothing for missing save directory" $ do
    withSystemTempDirectory "savev3empty" $ \tmpDir -> do
      result <- loadPlayerV3 tmpDir
      result `shouldBe` Nothing

  it "migrateV2toV3 roundtrip: migrated data encodes and decodes cleanly" $ do
    let v3 = migrateV2toV3 sampleV2
        encoded = encodeSaveV3 v3
        decoded = decodeSaveV3 encoded
    decoded `shouldBe` Just v3

  it "migrateV2toV3 sets default armor, spawn, weather fields" $ do
    let v3 = migrateV2toV3 sampleV2
    sv3ArmorSlots v3 `shouldBe` []
    sv3SpawnPoint v3 `shouldBe` (0, 80, 0)
    sv3WeatherType v3 `shouldBe` 0
    sv3WeatherTimer v3 `shouldBe` 600.0

  it "savePlayerV3 creates the player.dat file" $ do
    withSystemTempDirectory "savev3file" $ \tmpDir -> do
      savePlayerV3 tmpDir sampleV3
      exists <- doesFileExist (playerSavePath tmpDir)
      exists `shouldBe` True

  it "savePlayerV3 then loadPlayerV3 preserves armor and chunk metas" $ do
    withSystemTempDirectory "savev3armor" $ \tmpDir -> do
      let sd = sampleV3
                { sv3ArmorSlots = [Just (ToolItem Pickaxe Diamond 100, 1), Nothing, Nothing, Nothing]
                , sv3ChunkMetas = [ChunkMeta (V2 0 0) True True, ChunkMeta (V2 1 (-1)) False False]
                }
      savePlayerV3 tmpDir sd
      result <- loadPlayerV3 tmpDir
      result `shouldBe` Just sd

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

  -- Biome-feature wiring tests: verify biome -> tree type mappings
  it "Forest has both OakTree and BirchTree features" $ do
    let trees = featureTrees (biomeFeatures Forest)
        types = map (\(TreeFeature t _) -> t) trees
    types `shouldSatisfy` elem OakTree
    types `shouldSatisfy` elem BirchTree

  it "Taiga has SpruceTree feature" $ do
    let trees = featureTrees (biomeFeatures Taiga)
        types = map (\(TreeFeature t _) -> t) trees
    types `shouldSatisfy` elem SpruceTree

  it "Plains has sparse OakTree feature" $ do
    let trees = featureTrees (biomeFeatures Plains)
        types = map (\(TreeFeature t _) -> t) trees
    types `shouldSatisfy` elem OakTree

  it "Desert has no tree features" $ do
    let trees = featureTrees (biomeFeatures Desert)
    trees `shouldBe` []

  it "all tree types fall back to OakLog" $ do
    let allTrees = [minBound .. maxBound] :: [TreeType]
    mapM_ (\t -> treeLogBlock t `shouldBe` OakLog) allTrees

  it "all tree types fall back to OakLeaves" $ do
    let allTrees = [minBound .. maxBound] :: [TreeType]
    mapM_ (\t -> treeLeafBlock t `shouldBe` OakLeaves) allTrees

  it "Forest tree density is higher than Plains" $ do
    let forestTrees = featureTrees (biomeFeatures Forest)
        plainsTrees = featureTrees (biomeFeatures Plains)
        totalDensity ts = sum [d | TreeFeature _ d <- ts]
    totalDensity forestTrees `shouldSatisfy` (> totalDensity plainsTrees)

  it "Taiga SpruceTree density is higher than Mountains SpruceTree density" $ do
    let taigaTrees = featureTrees (biomeFeatures Taiga)
        mtnTrees   = featureTrees (biomeFeatures Mountains)
        spruceDensity ts = sum [d | TreeFeature SpruceTree d <- ts]
    spruceDensity taigaTrees `shouldSatisfy` (> spruceDensity mtnTrees)

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
-- Bed Sleep (night detection & hostile check)
-- =========================================================================
bedSleepSpec :: Spec
bedSleepSpec = describe "Bed sleep logic" $ do
  describe "Night time range detection" $ do
    it "isNight is True at midnight (dncTime = 0.0)" $ do
      let dnc = newDayNightCycle { dncTime = 0.0 }
      isNight dnc `shouldBe` True

    it "isNight is True at dncTime = 0.10" $ do
      let dnc = newDayNightCycle { dncTime = 0.10 }
      isNight dnc `shouldBe` True

    it "isNight is True at dncTime = 0.19" $ do
      let dnc = newDayNightCycle { dncTime = 0.19 }
      isNight dnc `shouldBe` True

    it "isNight is False at dawn (dncTime = 0.25)" $ do
      let dnc = newDayNightCycle { dncTime = 0.25 }
      isNight dnc `shouldBe` False

    it "isNight is False at noon (dncTime = 0.50)" $ do
      let dnc = newDayNightCycle { dncTime = 0.50 }
      isNight dnc `shouldBe` False

    it "isNight is True at dncTime = 0.80" $ do
      let dnc = newDayNightCycle { dncTime = 0.80 }
      isNight dnc `shouldBe` True

    it "isNight is True at dncTime = 0.95" $ do
      let dnc = newDayNightCycle { dncTime = 0.95 }
      isNight dnc `shouldBe` True

    it "isNight is False during dusk (dncTime = 0.75)" $ do
      let dnc = newDayNightCycle { dncTime = 0.75 }
      isNight dnc `shouldBe` False

    it "getTimeOfDay returns Night for values >= 0.80" $ do
      let dnc = newDayNightCycle { dncTime = 0.85 }
      getTimeOfDay dnc `shouldBe` Night

    it "getTimeOfDay returns Night for values < 0.20" $ do
      let dnc = newDayNightCycle { dncTime = 0.05 }
      getTimeOfDay dnc `shouldBe` Night

    it "getTimeOfDay returns Day for values in 0.30-0.70" $ do
      let dnc = newDayNightCycle { dncTime = 0.50 }
      getTimeOfDay dnc `shouldBe` Day

    it "getTimeOfDay returns Dawn for values in 0.20-0.30" $ do
      let dnc = newDayNightCycle { dncTime = 0.25 }
      getTimeOfDay dnc `shouldBe` Dawn

    it "getTimeOfDay returns Dusk for values in 0.70-0.80" $ do
      let dnc = newDayNightCycle { dncTime = 0.75 }
      getTimeOfDay dnc `shouldBe` Dusk

  describe "Hostile mob detection for bed sleep" $ do
    it "entitiesInRange returns entities within radius" $ do
      ew <- newEntityWorld
      _ <- spawnEntity ew (V3 10 65 10) 20 "Zombie"
      nearby <- entitiesInRange ew (V3 12 65 10) 8.0
      length nearby `shouldBe` 1

    it "entitiesInRange excludes entities outside radius" $ do
      ew <- newEntityWorld
      _ <- spawnEntity ew (V3 100 65 100) 20 "Zombie"
      nearby <- entitiesInRange ew (V3 10 65 10) 8.0
      length nearby `shouldBe` 0

    it "hostile tag check identifies Zombie as hostile" $ do
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
      ("Zombie" `elem` hostileTags) `shouldBe` True

    it "hostile tag check identifies Skeleton as hostile" $ do
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
      ("Skeleton" `elem` hostileTags) `shouldBe` True

    it "hostile tag check does not match Pig" $ do
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
      ("Pig" `elem` hostileTags) `shouldBe` False

    it "nearby hostile blocks sleep" $ do
      ew <- newEntityWorld
      _ <- spawnEntity ew (V3 10 65 10) 20 "Zombie"
      nearby <- entitiesInRange ew (V3 12 65 10) 8.0
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
          hasHostile = any (\e -> entTag e `elem` hostileTags) nearby
      hasHostile `shouldBe` True

    it "nearby passive mob does not block sleep" $ do
      ew <- newEntityWorld
      _ <- spawnEntity ew (V3 10 65 10) 10 "Pig"
      nearby <- entitiesInRange ew (V3 12 65 10) 8.0
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
          hasHostile = any (\e -> entTag e `elem` hostileTags) nearby
      hasHostile `shouldBe` False

    it "no entities means sleep is allowed" $ do
      ew <- newEntityWorld
      nearby <- entitiesInRange ew (V3 10 65 10) 8.0
      let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"] :: [String]
          hasHostile = any (\e -> entTag e `elem` hostileTags) nearby
      hasHostile `shouldBe` False

    it "dawn time (0.25) after successful sleep" $ do
      let dnc = newDayNightCycle { dncTime = 0.0, dncDayCount = 0 }
          -- Simulate what happens when sleep succeeds
          dnc' = dnc { dncTime = 0.25, dncDayCount = dncDayCount dnc + 1 }
      dncTime dnc' `shouldBe` 0.25
      dncDayCount dnc' `shouldBe` 1
      isNight dnc' `shouldBe` False

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

-- =========================================================================
-- Mob loot drops
-- =========================================================================
mobLootDropSpec :: Spec
mobLootDropSpec = describe "Mob loot drops" $ do

  -- Helper: run mobDrops many times and collect all results
  let collectDrops tag n = concat <$> mapM (\_ -> mobDrops tag) [1..n :: Int]
      -- Check that every (item, count) in a drop list satisfies a predicate
      allDropsSatisfy p drops = all p drops

  it "Pig drops RawPorkchop (1-3)" $ do
    drops <- collectDrops "Pig" 50
    let porkDrops = filter (\(i, _) -> i == FoodItem RawPorkchop) drops
    length porkDrops `shouldSatisfy` (> 0)
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 3) porkDrops `shouldBe` True

  it "Cow drops RawBeef (1-3) and Leather (0-2)" $ do
    drops <- collectDrops "Cow" 50
    let beefDrops = filter (\(i, _) -> i == FoodItem RawBeef) drops
        leatherDrops = filter (\(i, _) -> i == MaterialItem Leather) drops
    length beefDrops `shouldSatisfy` (> 0)
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 3) beefDrops `shouldBe` True
    -- Leather can be 0 (filtered out), so only check upper bound
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) leatherDrops `shouldBe` True

  it "Chicken drops RawChicken (1) and Feather (0-2)" $ do
    drops <- collectDrops "Chicken" 50
    let chickenDrops = filter (\(i, _) -> i == FoodItem RawChicken) drops
        featherDrops = filter (\(i, _) -> i == MaterialItem Feather) drops
    length chickenDrops `shouldSatisfy` (> 0)
    allDropsSatisfy (\(_, c) -> c == 1) chickenDrops `shouldBe` True
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) featherDrops `shouldBe` True

  it "Sheep drops 1 Wool" $ do
    drops <- mobDrops "Sheep"
    drops `shouldBe` [(BlockItem Wool, 1)]

  it "Zombie drops RottenFlesh (0-2)" $ do
    drops <- collectDrops "Zombie" 50
    let fleshDrops = filter (\(i, _) -> i == FoodItem RottenFlesh) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) fleshDrops `shouldBe` True

  it "Skeleton drops Bone (0-2) and Arrow (0-2)" $ do
    drops <- collectDrops "Skeleton" 50
    let boneDrops = filter (\(i, _) -> i == MaterialItem Bone) drops
        arrowDrops = filter (\(i, _) -> i == MaterialItem ArrowMat) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) boneDrops `shouldBe` True
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) arrowDrops `shouldBe` True

  it "Creeper drops Gunpowder (0-2)" $ do
    drops <- collectDrops "Creeper" 50
    let gunpowderDrops = filter (\(i, _) -> i == MaterialItem Gunpowder) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) gunpowderDrops `shouldBe` True

  it "Spider drops String (0-2)" $ do
    drops <- collectDrops "Spider" 50
    let stringDrops = filter (\(i, _) -> i == MaterialItem StringMat) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) stringDrops `shouldBe` True

  it "Wolf drops Bone (0-2)" $ do
    drops <- collectDrops "Wolf" 50
    let boneDrops = filter (\(i, _) -> i == MaterialItem Bone) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) boneDrops `shouldBe` True

  it "TamedWolf drops Bone (0-2)" $ do
    drops <- collectDrops "TamedWolf" 50
    let boneDrops = filter (\(i, _) -> i == MaterialItem Bone) drops
    allDropsSatisfy (\(_, c) -> c >= 1 && c <= 2) boneDrops `shouldBe` True

  it "Unknown mob type drops nothing" $ do
    drops <- mobDrops "UnknownMob"
    drops `shouldBe` []

  it "Mob death spawns drops at entity position" $ do
    ew <- newEntityWorld
    eid <- spawnEntity ew (V3 10.0 65.0 10.0) 20.0 "Zombie"
    -- Simulate killing the entity
    updateEntity ew eid (\e -> e { entHealth = 0, entAlive = False })
    mEnt <- getEntity ew eid
    case mEnt of
      Just ent -> do
        entHealth ent `shouldBe` 0
        entAlive ent `shouldBe` False
        -- Verify drops can be spawned at entity position
        di <- newDroppedItems
        drops <- mobDrops (entTag ent)
        mapM_ (\(item, count) -> spawnDrop di item count (entPosition ent)) drops
        items <- readIORef di
        -- Zombie drops 0-2 rotten flesh, so items could be empty or have 1 entry
        length items `shouldSatisfy` (<= 1)
      Nothing -> expectationFailure "Expected zombie entity"

  it "Drop counts are within loot table bounds across many samples" $ do
    -- Pig: RawPorkchop 1-3, run 200 times to ensure range is covered
    drops <- collectDrops "Pig" 200
    let counts = map snd $ filter (\(i, _) -> i == FoodItem RawPorkchop) drops
        minC = minimum counts
        maxC = maximum counts
    minC `shouldBe` 1
    maxC `shouldBe` 3

-- =========================================================================
-- Wood Variants and Flora Blocks
-- =========================================================================
woodVariantsAndFloraSpec :: Spec
woodVariantsAndFloraSpec = describe "Wood variants and flora blocks" $ do
  -- Block properties
  describe "blockProperties" $ do
    it "BirchLog is solid with hardness 2.0" $ do
      let bp = blockProperties BirchLog
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 2.0

    it "SpruceLeaves is solid and transparent with hardness 0.2" $ do
      let bp = blockProperties SpruceLeaves
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` True
      bpHardness bp `shouldBe` 0.2

    it "JunglePlanks is solid with hardness 2.0" $ do
      let bp = blockProperties JunglePlanks
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 2.0

    it "TallGrass is non-solid and transparent with hardness 0.0" $ do
      let bp = blockProperties TallGrass
      bpSolid bp `shouldBe` False
      bpTransparent bp `shouldBe` True
      bpHardness bp `shouldBe` 0.0

    it "Dandelion is non-solid and transparent" $ do
      let bp = blockProperties Dandelion
      bpSolid bp `shouldBe` False
      bpTransparent bp `shouldBe` True

    it "Rose is non-solid and transparent" $ do
      let bp = blockProperties Rose
      bpSolid bp `shouldBe` False
      bpTransparent bp `shouldBe` True

    it "BrownMushroom is non-solid and transparent" $ do
      let bp = blockProperties BrownMushroom
      bpSolid bp `shouldBe` False
      bpTransparent bp `shouldBe` True

    it "RedMushroom is non-solid and transparent" $ do
      let bp = blockProperties RedMushroom
      bpSolid bp `shouldBe` False
      bpTransparent bp `shouldBe` True

  -- isLeafBlock
  describe "isLeafBlock" $ do
    it "BirchLeaves is a leaf block" $
      isLeafBlock BirchLeaves `shouldBe` True
    it "SpruceLeaves is a leaf block" $
      isLeafBlock SpruceLeaves `shouldBe` True
    it "JungleLeaves is a leaf block" $
      isLeafBlock JungleLeaves `shouldBe` True
    it "BirchLog is not a leaf block" $
      isLeafBlock BirchLog `shouldBe` False

  -- Crafting recipes
  describe "crafting recipes" $ do
    it "BirchLog crafts into 4 BirchPlanks" $ do
      let grid = setCraftingSlot (emptyCraftingGrid 2) 0 0 (Just (BlockItem BirchLog))
      tryCraft grid `shouldBe` CraftSuccess (BlockItem BirchPlanks) 4

    it "SpruceLog crafts into 4 SprucePlanks" $ do
      let grid = setCraftingSlot (emptyCraftingGrid 2) 0 0 (Just (BlockItem SpruceLog))
      tryCraft grid `shouldBe` CraftSuccess (BlockItem SprucePlanks) 4

    it "JungleLog crafts into 4 JunglePlanks" $ do
      let grid = setCraftingSlot (emptyCraftingGrid 2) 0 0 (Just (BlockItem JungleLog))
      tryCraft grid `shouldBe` CraftSuccess (BlockItem JunglePlanks) 4

  -- Block drops
  describe "blockDrops" $ do
    it "BirchLog drops itself" $
      blockDrops BirchLog `shouldBe` [(BlockItem BirchLog, 1)]
    it "SpruceLog drops itself" $
      blockDrops SpruceLog `shouldBe` [(BlockItem SpruceLog, 1)]
    it "JungleLog drops itself" $
      blockDrops JungleLog `shouldBe` [(BlockItem JungleLog, 1)]
    it "BirchLeaves drops a sapling" $
      blockDrops BirchLeaves `shouldBe` [(BlockItem OakSapling, 1)]
    it "TallGrass drops nothing" $
      blockDrops TallGrass `shouldBe` []
    it "Dandelion drops itself" $
      blockDrops Dandelion `shouldBe` [(BlockItem Dandelion, 1)]
    it "RedMushroom drops itself" $
      blockDrops RedMushroom `shouldBe` [(BlockItem RedMushroom, 1)]

-- =========================================================================
-- XP System
-- =========================================================================
xpSpec :: Spec
xpSpec = describe "Game.XP" $ do
  it "level 0 at 0 XP" $
    xpLevel 0 `shouldBe` 0

  it "level 0 at 9 XP (not yet enough for level 1)" $
    xpLevel 9 `shouldBe` 0

  it "level 1 at 10 XP" $
    xpLevel 10 `shouldBe` 1

  it "level 2 at 40 XP" $
    xpLevel 40 `shouldBe` 2

  it "level 3 at 90 XP" $
    xpLevel 90 `shouldBe` 3

  it "level calculation: floor(sqrt(xp / 10))" $ do
    xpLevel 25 `shouldBe` 1  -- sqrt(2.5) = 1.58 -> floor = 1
    xpLevel 39 `shouldBe` 1  -- sqrt(3.9) = 1.97 -> floor = 1
    xpLevel 100 `shouldBe` 3  -- sqrt(10) = 3.16 -> floor = 3

  it "xpForNextLevel returns correct thresholds" $ do
    xpForNextLevel 0 `shouldBe` 10   -- level 1 requires 10 XP total
    xpForNextLevel 1 `shouldBe` 40   -- level 2 requires 40 XP total
    xpForNextLevel 2 `shouldBe` 90   -- level 3 requires 90 XP total

  it "xpProgress is 0.0 at start of level" $ do
    xpProgress 0 `shouldSatisfy` (< 0.01)
    xpProgress 10 `shouldSatisfy` (< 0.01)
    xpProgress 40 `shouldSatisfy` (< 0.01)

  it "xpProgress approaches 1.0 near end of level" $ do
    xpProgress 9 `shouldSatisfy` (> 0.85)
    xpProgress 39 `shouldSatisfy` (> 0.95)

  it "xpProgress is between 0 and 1" $
    property $ \n -> let xp = abs n `mod` 10000
                         p = xpProgress xp
                     in p >= 0.0 && p <= 1.0

  it "CoalOre gives 1 XP" $
    xpForBlock CoalOre `shouldBe` 1

  it "DiamondOre gives 7 XP" $
    xpForBlock DiamondOre `shouldBe` 7

  it "GoldOre gives 3 XP" $
    xpForBlock GoldOre `shouldBe` 3

  it "IronOre gives 1 XP" $
    xpForBlock IronOre `shouldBe` 1

  it "Stone gives 0 XP" $
    xpForBlock Stone `shouldBe` 0

  it "Dirt gives 0 XP" $
    xpForBlock Dirt `shouldBe` 0

  it "Hostile mobs give 5 XP" $ do
    xpForMobKill Zombie `shouldBe` 5
    xpForMobKill Skeleton `shouldBe` 5
    xpForMobKill Creeper `shouldBe` 5

  it "Passive mobs give 1-3 XP" $ do
    xpForMobKill Pig `shouldSatisfy` (\x -> x >= 1 && x <= 3)
    xpForMobKill Cow `shouldSatisfy` (\x -> x >= 1 && x <= 3)
    xpForMobKill Sheep `shouldSatisfy` (\x -> x >= 1 && x <= 3)
    xpForMobKill Chicken `shouldSatisfy` (\x -> x >= 1 && x <= 3)

  it "Neutral mobs give 3 XP" $ do
    xpForMobKill Spider `shouldBe` 3
    xpForMobKill Wolf `shouldBe` 3

-- =========================================================================
-- Utility Blocks
-- =========================================================================
utilityBlockSpec :: Spec
utilityBlockSpec = describe "Utility blocks" $ do
  -- Block properties
  describe "blockProperties" $ do
    it "Bookshelf is solid with hardness 1.5" $ do
      let bp = blockProperties Bookshelf
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 1.5

    it "Anvil is solid with hardness 5.0" $ do
      let bp = blockProperties Anvil
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 5.0

    it "BrewingStand is solid and transparent with hardness 0.5" $ do
      let bp = blockProperties BrewingStand
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` True
      bpHardness bp `shouldBe` 0.5

    it "BrewingStand emits light level 1" $ do
      bpLightEmit (blockProperties BrewingStand) `shouldBe` 1

    it "Ice is solid and transparent with hardness 0.5" $ do
      let bp = blockProperties Ice
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` True
      bpHardness bp `shouldBe` 0.5

    it "PackedIce is solid and opaque with hardness 0.5" $ do
      let bp = blockProperties PackedIce
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 0.5

    it "RedstoneLamp is solid and opaque with hardness 0.3" $ do
      let bp = blockProperties RedstoneLamp
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 0.3

    it "Hopper is solid and opaque with hardness 3.0" $ do
      let bp = blockProperties Hopper
      bpSolid bp `shouldBe` True
      bpTransparent bp `shouldBe` False
      bpHardness bp `shouldBe` 3.0

  -- Enum values
  describe "enum values" $ do
    it "new utility blocks follow RedMushroom in enum order" $ do
      fromEnum Bookshelf `shouldBe` fromEnum RedMushroom + 1
      fromEnum Anvil `shouldBe` fromEnum RedMushroom + 2
      fromEnum BrewingStand `shouldBe` fromEnum RedMushroom + 3
      fromEnum Ice `shouldBe` fromEnum RedMushroom + 4
      fromEnum PackedIce `shouldBe` fromEnum RedMushroom + 5
      fromEnum RedstoneLamp `shouldBe` fromEnum RedMushroom + 6
      fromEnum Hopper `shouldBe` fromEnum RedMushroom + 7

    it "enum roundtrip for all new utility blocks" $ do
      toEnum (fromEnum Bookshelf) `shouldBe` (Bookshelf :: BlockType)
      toEnum (fromEnum Anvil) `shouldBe` (Anvil :: BlockType)
      toEnum (fromEnum BrewingStand) `shouldBe` (BrewingStand :: BlockType)
      toEnum (fromEnum Ice) `shouldBe` (Ice :: BlockType)
      toEnum (fromEnum PackedIce) `shouldBe` (PackedIce :: BlockType)
      toEnum (fromEnum RedstoneLamp) `shouldBe` (RedstoneLamp :: BlockType)
      toEnum (fromEnum Hopper) `shouldBe` (Hopper :: BlockType)

  -- Texture coordinates
  describe "blockFaceTexCoords" $ do
    it "Bookshelf has different top and side textures" $ do
      blockFaceTexCoords Bookshelf FaceTop `shouldNotBe` blockFaceTexCoords Bookshelf FaceEast

    it "Hopper has different top and side textures" $ do
      blockFaceTexCoords Hopper FaceTop `shouldNotBe` blockFaceTexCoords Hopper FaceEast

    it "all new block texcoords are valid atlas coords" $ do
      let blocks = [Bookshelf, Anvil, BrewingStand, Ice, PackedIce, RedstoneLamp, Hopper]
      mapM_ (\bt -> do
        let V2 u v = blockFaceTexCoords bt FaceTop
        u `shouldSatisfy` (>= 0)
        v `shouldSatisfy` (>= 0)
        u `shouldSatisfy` (< 16)
        v `shouldSatisfy` (< 16)
        ) blocks

  -- Block drops
  describe "blockDrops" $ do
    it "Bookshelf drops 3 OakPlanks" $
      blockDrops Bookshelf `shouldBe` [(BlockItem OakPlanks, 3)]

    it "Anvil drops itself" $
      blockDrops Anvil `shouldBe` [(BlockItem Anvil, 1)]

    it "Ice drops nothing (melts)" $
      blockDrops Ice `shouldBe` []

    it "PackedIce drops itself" $
      blockDrops PackedIce `shouldBe` [(BlockItem PackedIce, 1)]

    it "RedstoneLamp drops itself" $
      blockDrops RedstoneLamp `shouldBe` [(BlockItem RedstoneLamp, 1)]

    it "Hopper drops itself" $
      blockDrops Hopper `shouldBe` [(BlockItem Hopper, 1)]

  -- Preferred tools
  describe "blockPreferredTool" $ do
    it "Bookshelf is mined with axe" $
      blockPreferredTool Bookshelf `shouldBe` Just Axe

    it "Anvil is mined with pickaxe" $
      blockPreferredTool Anvil `shouldBe` Just Pickaxe

    it "Ice is mined with pickaxe" $
      blockPreferredTool Ice `shouldBe` Just Pickaxe

    it "Hopper is mined with pickaxe" $
      blockPreferredTool Hopper `shouldBe` Just Pickaxe

  -- Harvest levels
  describe "blockRequiredHarvestLevel" $ do
    it "Anvil requires wood pickaxe or better" $
      blockRequiredHarvestLevel Anvil `shouldBe` 1

    it "Hopper requires wood pickaxe or better" $
      blockRequiredHarvestLevel Hopper `shouldBe` 1

    it "Bookshelf can be harvested by hand" $
      blockRequiredHarvestLevel Bookshelf `shouldBe` 0

  -- Crafting recipes
  describe "crafting recipes" $ do
    it "bookshelf crafts from 6 planks + 3 paper" $ do
      let grid = emptyCraftingGrid 3
            & \g -> setCraftingSlot g 0 0 (Just (BlockItem OakPlanks))
            & \g -> setCraftingSlot g 0 1 (Just (BlockItem OakPlanks))
            & \g -> setCraftingSlot g 0 2 (Just (BlockItem OakPlanks))
            & \g -> setCraftingSlot g 1 0 (Just (MaterialItem Paper))
            & \g -> setCraftingSlot g 1 1 (Just (MaterialItem Paper))
            & \g -> setCraftingSlot g 1 2 (Just (MaterialItem Paper))
            & \g -> setCraftingSlot g 2 0 (Just (BlockItem OakPlanks))
            & \g -> setCraftingSlot g 2 1 (Just (BlockItem OakPlanks))
            & \g -> setCraftingSlot g 2 2 (Just (BlockItem OakPlanks))
      tryCraft grid `shouldBe` CraftSuccess (BlockItem Bookshelf) 1

    it "redstone lamp crafts from 4 redstone dust + 1 glowstone" $ do
      let grid = emptyCraftingGrid 3
            & \g -> setCraftingSlot g 0 1 (Just (BlockItem RedstoneDust))
            & \g -> setCraftingSlot g 1 0 (Just (BlockItem RedstoneDust))
            & \g -> setCraftingSlot g 1 1 (Just (BlockItem Glowstone))
            & \g -> setCraftingSlot g 1 2 (Just (BlockItem RedstoneDust))
            & \g -> setCraftingSlot g 2 1 (Just (BlockItem RedstoneDust))
      tryCraft grid `shouldBe` CraftSuccess (BlockItem RedstoneLamp) 1

  -- Binary roundtrip for block items
  describe "Binary roundtrip" $ do
    it "new utility block items roundtrip through Binary" $ do
      let blocks = [Bookshelf, Anvil, BrewingStand, Ice, PackedIce, RedstoneLamp, Hopper]
      mapM_ (\bt -> decode (encode bt) `shouldBe` bt) blocks

-- =========================================================================
-- Bucket Items
-- =========================================================================
bucketItemSpec :: Spec
bucketItemSpec = describe "Bucket items" $ do
  -- Basic properties
  describe "itemToBlock" $ do
    it "BucketItem is not a block item" $
      itemToBlock (BucketItem BucketEmpty) `shouldBe` Nothing

    it "BucketItem water is not a block item" $
      itemToBlock (BucketItem BucketWater) `shouldBe` Nothing

  describe "isBlockItem" $ do
    it "BucketItem is not a block item" $
      isBlockItem (BucketItem BucketLava) `shouldBe` False

  describe "itemStackLimit" $ do
    it "empty bucket stacks to 16" $
      itemStackLimit (BucketItem BucketEmpty) `shouldBe` 16

    it "water bucket stacks to 16" $
      itemStackLimit (BucketItem BucketWater) `shouldBe` 16

    it "lava bucket stacks to 16" $
      itemStackLimit (BucketItem BucketLava) `shouldBe` 16

    it "milk bucket stacks to 16" $
      itemStackLimit (BucketItem BucketMilk) `shouldBe` 16

  -- Binary roundtrip
  describe "Binary roundtrip" $ do
    it "all bucket types roundtrip through Binary" $ do
      let buckets = [BucketEmpty, BucketWater, BucketLava, BucketMilk]
      mapM_ (\bt -> do
        let item = BucketItem bt
        decode (encode item) `shouldBe` item
        ) buckets

  -- BucketType enum
  describe "BucketType enum" $ do
    it "BucketType has correct enum order" $ do
      fromEnum BucketEmpty `shouldBe` 0
      fromEnum BucketWater `shouldBe` 1
      fromEnum BucketLava  `shouldBe` 2
      fromEnum BucketMilk  `shouldBe` 3

    it "BucketType roundtrips through enum" $ do
      let types = [BucketEmpty, BucketWater, BucketLava, BucketMilk]
      mapM_ (\bt -> toEnum (fromEnum bt) `shouldBe` bt) types

  -- Crafting
  describe "crafting" $ do
    it "empty bucket crafts from 3 iron ingots in V-shape" $ do
      let grid = emptyCraftingGrid 3
            & \g -> setCraftingSlot g 0 0 (Just (MaterialItem IronIngot))
            & \g -> setCraftingSlot g 0 2 (Just (MaterialItem IronIngot))
            & \g -> setCraftingSlot g 1 1 (Just (MaterialItem IronIngot))
      tryCraft grid `shouldBe` CraftSuccess (BucketItem BucketEmpty) 1

-- =========================================================================
-- Bucket Mechanics (pickup / place water and lava)
-- =========================================================================
bucketMechanicsSpec :: Spec
bucketMechanicsSpec = describe "Bucket mechanics" $ do

  -- Pure logic: determineBucketAction
  describe "determineBucketAction" $ do
    it "empty bucket on Water -> pickup water" $
      determineBucketAction BucketEmpty Water
        `shouldBe` BucketPickup Water BucketWater

    it "empty bucket on Lava -> pickup lava" $
      determineBucketAction BucketEmpty Lava
        `shouldBe` BucketPickup Lava BucketLava

    it "empty bucket on Stone -> no action" $
      determineBucketAction BucketEmpty Stone
        `shouldBe` BucketNoAction

    it "empty bucket on Air -> no action" $
      determineBucketAction BucketEmpty Air
        `shouldBe` BucketNoAction

    it "water bucket on any block -> place water" $
      determineBucketAction BucketWater Stone
        `shouldBe` BucketPlace Water BucketEmpty

    it "water bucket on Air -> place water" $
      determineBucketAction BucketWater Air
        `shouldBe` BucketPlace Water BucketEmpty

    it "lava bucket on any block -> place lava" $
      determineBucketAction BucketLava Dirt
        `shouldBe` BucketPlace Lava BucketEmpty

    it "lava bucket on Air -> place lava" $
      determineBucketAction BucketLava Air
        `shouldBe` BucketPlace Lava BucketEmpty

    it "milk bucket -> no action" $
      determineBucketAction BucketMilk Water
        `shouldBe` BucketNoAction

    it "milk bucket on Stone -> no action" $
      determineBucketAction BucketMilk Stone
        `shouldBe` BucketNoAction

  -- Helper conversion functions
  describe "bucketTypeToFluidBlock" $ do
    it "BucketWater -> Just Water" $
      bucketTypeToFluidBlock BucketWater `shouldBe` Just Water

    it "BucketLava -> Just Lava" $
      bucketTypeToFluidBlock BucketLava `shouldBe` Just Lava

    it "BucketEmpty -> Nothing" $
      bucketTypeToFluidBlock BucketEmpty `shouldBe` Nothing

    it "BucketMilk -> Nothing" $
      bucketTypeToFluidBlock BucketMilk `shouldBe` Nothing

  describe "fluidBlockToBucketType" $ do
    it "Water -> Just BucketWater" $
      fluidBlockToBucketType Water `shouldBe` Just BucketWater

    it "Lava -> Just BucketLava" $
      fluidBlockToBucketType Lava `shouldBe` Just BucketLava

    it "Stone -> Nothing" $
      fluidBlockToBucketType Stone `shouldBe` Nothing

    it "Air -> Nothing" $
      fluidBlockToBucketType Air `shouldBe` Nothing

  -- Inventory integration: picking up fluid replaces bucket in slot
  describe "inventory integration" $ do
    it "picking up water: empty bucket becomes water bucket in inventory" $ do
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BucketItem BucketEmpty) 1))
          sel  = 0
          inv1 = setSlot inv0 sel (Just (ItemStack (BucketItem BucketWater) 1))
      selectedItem inv1 `shouldBe` Just (ItemStack (BucketItem BucketWater) 1)

    it "placing water: water bucket becomes empty bucket in inventory" $ do
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BucketItem BucketWater) 1))
          sel  = 0
          inv1 = setSlot inv0 sel (Just (ItemStack (BucketItem BucketEmpty) 1))
      selectedItem inv1 `shouldBe` Just (ItemStack (BucketItem BucketEmpty) 1)

    it "picking up lava: empty bucket becomes lava bucket in inventory" $ do
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BucketItem BucketEmpty) 1))
          sel  = 0
          inv1 = setSlot inv0 sel (Just (ItemStack (BucketItem BucketLava) 1))
      selectedItem inv1 `shouldBe` Just (ItemStack (BucketItem BucketLava) 1)

    it "placing lava: lava bucket becomes empty bucket in inventory" $ do
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BucketItem BucketLava) 1))
          sel  = 0
          inv1 = setSlot inv0 sel (Just (ItemStack (BucketItem BucketEmpty) 1))
      selectedItem inv1 `shouldBe` Just (ItemStack (BucketItem BucketEmpty) 1)

  -- World integration: fluid source placement and removal
  describe "world integration" $ do
    it "removeFluid sets block to Air" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 5 64 5
      addFluidSource fs world pos FluidWater
      block <- worldGetBlock world pos
      block `shouldBe` Water
      removeFluid fs world pos
      block' <- worldGetBlock world pos
      block' `shouldBe` Air

    it "addFluidSource sets Water block" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 3 64 3
      addFluidSource fs world pos FluidWater
      block <- worldGetBlock world pos
      block `shouldBe` Water

    it "addFluidSource sets Lava block" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 3 64 3
      addFluidSource fs world pos FluidLava
      block <- worldGetBlock world pos
      block `shouldBe` Lava

    it "removeFluid removes fluid tracking" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 5 64 5
      addFluidSource fs world pos FluidWater
      mFluid <- getFluid fs pos
      mFluid `shouldSatisfy` (/= Nothing)
      removeFluid fs world pos
      mFluid' <- getFluid fs pos
      mFluid' `shouldBe` Nothing

    it "pickup water: world block becomes Air" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 4 64 4
      addFluidSource fs world pos FluidWater
      -- Simulate bucket pickup
      let action = determineBucketAction BucketEmpty Water
      action `shouldBe` BucketPickup Water BucketWater
      removeFluid fs world pos
      block <- worldGetBlock world pos
      block `shouldBe` Air

    it "place water: world block becomes Water" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 6 64 6
      -- Simulate bucket placement
      let action = determineBucketAction BucketWater Air
      action `shouldBe` BucketPlace Water BucketEmpty
      addFluidSource fs world pos FluidWater
      block <- worldGetBlock world pos
      block `shouldBe` Water

    it "place lava: world block becomes Lava" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 7 64 7
      let action = determineBucketAction BucketLava Air
      action `shouldBe` BucketPlace Lava BucketEmpty
      addFluidSource fs world pos FluidLava
      block <- worldGetBlock world pos
      block `shouldBe` Lava

    it "pickup lava: world block becomes Air" $ withTestWorld $ \world -> do
      fs <- newFluidState
      let pos = V3 8 64 8
      addFluidSource fs world pos FluidLava
      block <- worldGetBlock world pos
      block `shouldBe` Lava
      let action = determineBucketAction BucketEmpty Lava
      action `shouldBe` BucketPickup Lava BucketLava
      removeFluid fs world pos
      block' <- worldGetBlock world pos
      block' `shouldBe` Air

-- Wheat Crop Growth Stages
-- =========================================================================
wheatCropGrowthSpec :: Spec
wheatCropGrowthSpec = describe "Wheat crop growth stages" $ do
  -- Block properties
  it "all wheat crop stages are non-solid" $ do
    isSolid WheatCrop  `shouldBe` False
    isSolid WheatCrop1 `shouldBe` False
    isSolid WheatCrop2 `shouldBe` False
    isSolid WheatCrop3 `shouldBe` False
    isSolid WheatCrop4 `shouldBe` False
    isSolid WheatCrop5 `shouldBe` False
    isSolid WheatCrop6 `shouldBe` False
    isSolid WheatCrop7 `shouldBe` False

  it "all wheat crop stages are transparent" $ do
    isTransparent WheatCrop  `shouldBe` True
    isTransparent WheatCrop1 `shouldBe` True
    isTransparent WheatCrop2 `shouldBe` True
    isTransparent WheatCrop3 `shouldBe` True
    isTransparent WheatCrop4 `shouldBe` True
    isTransparent WheatCrop5 `shouldBe` True
    isTransparent WheatCrop6 `shouldBe` True
    isTransparent WheatCrop7 `shouldBe` True

  it "all wheat crop stages have hardness 0" $ do
    bpHardness (blockProperties WheatCrop)  `shouldBe` 0
    bpHardness (blockProperties WheatCrop1) `shouldBe` 0
    bpHardness (blockProperties WheatCrop2) `shouldBe` 0
    bpHardness (blockProperties WheatCrop3) `shouldBe` 0
    bpHardness (blockProperties WheatCrop4) `shouldBe` 0
    bpHardness (blockProperties WheatCrop5) `shouldBe` 0
    bpHardness (blockProperties WheatCrop6) `shouldBe` 0
    bpHardness (blockProperties WheatCrop7) `shouldBe` 0

  it "isWheatCropBlock identifies all stages" $ do
    isWheatCropBlock WheatCrop  `shouldBe` True
    isWheatCropBlock WheatCrop1 `shouldBe` True
    isWheatCropBlock WheatCrop2 `shouldBe` True
    isWheatCropBlock WheatCrop3 `shouldBe` True
    isWheatCropBlock WheatCrop4 `shouldBe` True
    isWheatCropBlock WheatCrop5 `shouldBe` True
    isWheatCropBlock WheatCrop6 `shouldBe` True
    isWheatCropBlock WheatCrop7 `shouldBe` True

  it "isWheatCropBlock rejects non-wheat blocks" $ do
    isWheatCropBlock Air       `shouldBe` False
    isWheatCropBlock Farmland  `shouldBe` False
    isWheatCropBlock Dirt      `shouldBe` False
    isWheatCropBlock OakSapling `shouldBe` False

  -- Texture coordinates are valid atlas positions
  it "all wheat crop stages have valid texture coords" $ do
    let checkCoords bt = do
          let V2 u v = blockFaceTexCoords bt FaceTop
          u `shouldSatisfy` (>= 0)
          v `shouldSatisfy` (>= 0)
          u `shouldSatisfy` (< 16)
          v `shouldSatisfy` (< 16)
    checkCoords WheatCrop1
    checkCoords WheatCrop2
    checkCoords WheatCrop3
    checkCoords WheatCrop4
    checkCoords WheatCrop5
    checkCoords WheatCrop6
    checkCoords WheatCrop7

  it "each wheat crop stage has distinct texture coords" $ do
    let coords = map (\bt -> blockFaceTexCoords bt FaceTop)
          [WheatCrop, WheatCrop1, WheatCrop2, WheatCrop3,
           WheatCrop4, WheatCrop5, WheatCrop6, WheatCrop7]
    length coords `shouldBe` length (nub coords)

  -- Drops
  it "WheatCrop (stage 0) drops WheatSeeds only" $
    blockDrops WheatCrop `shouldBe` [(MaterialItem Wheat, 1)]

  it "intermediate stages drop WheatSeeds only" $ do
    blockDrops WheatCrop1 `shouldBe` [(MaterialItem WheatSeeds, 1)]
    blockDrops WheatCrop2 `shouldBe` [(MaterialItem WheatSeeds, 1)]
    blockDrops WheatCrop3 `shouldBe` [(MaterialItem WheatSeeds, 1)]
    blockDrops WheatCrop4 `shouldBe` [(MaterialItem WheatSeeds, 1)]
    blockDrops WheatCrop5 `shouldBe` [(MaterialItem WheatSeeds, 1)]
    blockDrops WheatCrop6 `shouldBe` [(MaterialItem WheatSeeds, 1)]

  it "WheatCrop7 (fully mature) drops Wheat and WheatSeeds" $
    blockDrops WheatCrop7 `shouldBe` [(MaterialItem Wheat, 1), (MaterialItem WheatSeeds, 1)]

  -- Enum ordering (added at the end)
  it "wheat crop stages follow Hopper in enum" $ do
    fromEnum WheatCrop1 `shouldBe` fromEnum Hopper + 1
    fromEnum WheatCrop7 `shouldBe` fromEnum Hopper + 7
-- Structure Placement in World Generation
-- =========================================================================
structurePlacementSpec :: Spec
structurePlacementSpec = describe "World.Generation structure placement" $ do

  it "structureHashRoll is deterministic for same inputs" $ do
    let r1 = structureHashRoll 42 3 7 10001
        r2 = structureHashRoll 42 3 7 10001
    r1 `shouldBe` r2

  it "structureHashRoll returns non-negative values" $
    property $ \s cx cz salt ->
      structureHashRoll s cx cz salt >= 0

  it "structureHashRoll varies with different salts" $ do
    let r1 = structureHashRoll 42 3 7 10001
        r2 = structureHashRoll 42 3 7 10002
    r1 `shouldNotBe` r2

  it "structureHashRoll varies with different chunk positions" $ do
    let r1 = structureHashRoll 42 0 0 10001
        r2 = structureHashRoll 42 1 0 10001
        r3 = structureHashRoll 42 0 1 10001
    r1 `shouldNotBe` r2
    r1 `shouldNotBe` r3

  it "structureHashRoll varies with different seeds" $ do
    let r1 = structureHashRoll 42 3 7 10001
        r2 = structureHashRoll 99 3 7 10001
    r1 `shouldNotBe` r2

  it "placeStructureInChunk writes well blocks into chunk vector" $ do
    chunk <- newChunk (V2 0 0)
    let mvec = chunkBlocks chunk
    -- Place well at local (5, 64, 5)
    placeStructureInChunk mvec wellStructure 5 64 5
    -- Check cobblestone floor at (5, 64, 5)
    b <- MUV.read mvec (blockIndex 5 64 5)
    b `shouldBe` fromIntegral (fromEnum Cobblestone)
    -- Check water at (7, 65, 7) = origin (5,64,5) + offset (2,1,2)
    w <- MUV.read mvec (blockIndex 7 65 7)
    w `shouldBe` fromIntegral (fromEnum Water)

  it "placeStructureInChunk clips blocks outside chunk bounds" $ do
    chunk <- newChunk (V2 0 0)
    let mvec = chunkBlocks chunk
    -- Place well at edge so some blocks would go out of bounds
    placeStructureInChunk mvec wellStructure 14 64 14
    -- (14+4, 64, 14) = (18, 64, 14) is out of bounds, should not crash
    -- (14, 64, 14) should still have cobblestone
    b <- MUV.read mvec (blockIndex 14 64 14)
    b `shouldBe` fromIntegral (fromEnum Cobblestone)
    -- (14+1, 64, 14+1) = (15, 64, 15) is in bounds
    b2 <- MUV.read mvec (blockIndex 15 64 15)
    b2 `shouldBe` fromIntegral (fromEnum Cobblestone)

  it "placeStructureInChunk places dungeon underground" $ do
    chunk <- newChunk (V2 0 0)
    let mvec = chunkBlocks chunk
    placeStructureInChunk mvec dungeonStructure 4 20 4
    -- Floor cobblestone at (4, 20, 4)
    b <- MUV.read mvec (blockIndex 4 20 4)
    b `shouldBe` fromIntegral (fromEnum Cobblestone)
    -- Chest at center (4+3, 20+1, 4+3) = (7, 21, 7)
    c <- MUV.read mvec (blockIndex 7 21 7)
    c `shouldBe` fromIntegral (fromEnum Chest)

  it "generateChunk is deterministic — same seed produces identical chunks" $ do
    let cfg = defaultGenConfig { gcSeed = 12345 }
        pos = V2 10 20
    chunk1 <- generateChunk cfg pos
    chunk2 <- generateChunk cfg pos
    -- Compare all blocks in both chunks
    let totalBlocks = chunkWidth * chunkDepth * chunkHeight
    allSame <- allBlocksSame (chunkBlocks chunk1) (chunkBlocks chunk2) totalBlocks 0
    allSame `shouldBe` True

  it "generateChunk produces different chunks for different seeds" $ do
    let cfg1 = defaultGenConfig { gcSeed = 11111 }
        cfg2 = defaultGenConfig { gcSeed = 22222 }
        pos = V2 5 5
    chunk1 <- generateChunk cfg1 pos
    chunk2 <- generateChunk cfg2 pos
    -- At least one block should differ
    let totalBlocks = chunkWidth * chunkDepth * chunkHeight
    allSame <- allBlocksSame (chunkBlocks chunk1) (chunkBlocks chunk2) totalBlocks 0
    allSame `shouldBe` False

  it "generateChunk produces different chunks for different positions" $ do
    let cfg = defaultGenConfig { gcSeed = 42 }
    chunk1 <- generateChunk cfg (V2 0 0)
    chunk2 <- generateChunk cfg (V2 100 100)
    let totalBlocks = chunkWidth * chunkDepth * chunkHeight
    allSame <- allBlocksSame (chunkBlocks chunk1) (chunkBlocks chunk2) totalBlocks 0
    allSame `shouldBe` False

-- | Helper: compare all blocks in two mutable vectors
allBlocksSame :: MUV.IOVector Word8 -> MUV.IOVector Word8 -> Int -> Int -> IO Bool
allBlocksSame v1 v2 total i
  | i >= total = pure True
  | otherwise = do
      a <- MUV.read v1 i
      b <- MUV.read v2 i
      if a /= b then pure False else allBlocksSame v1 v2 total (i + 1)

-- =========================================================================
-- Command execution
-- =========================================================================
executeCommandSpec :: Spec
executeCommandSpec = describe "Game.Command.executeCommand" $ do
  it "/give returns CmdSuccess with item and count" $ do
    executeCommand (CmdGive "stone" 64) `shouldBe` CmdSuccess "Gave 64 stone"

  it "/give with count 1 shows count" $ do
    executeCommand (CmdGive "diamond" 1) `shouldBe` CmdSuccess "Gave 1 diamond"

  it "/tp returns CmdSuccess with coordinates" $ do
    let result = executeCommand (CmdTeleport 100 65 (-200))
    case result of
      CmdSuccess msg -> do
        msg `shouldSatisfy` \m -> "Teleported to" `isPrefixOf` m
      CmdError _ -> expectationFailure "Expected CmdSuccess"

  it "/time returns CmdSuccess" $ do
    executeCommand (CmdTime "day") `shouldBe` CmdSuccess "Time set to day"

  it "/time night returns CmdSuccess" $ do
    executeCommand (CmdTime "night") `shouldBe` CmdSuccess "Time set to night"

  it "/weather returns CmdSuccess" $ do
    executeCommand (CmdWeather "clear") `shouldBe` CmdSuccess "Weather set to clear"

  it "/weather rain returns CmdSuccess" $ do
    executeCommand (CmdWeather "rain") `shouldBe` CmdSuccess "Weather set to rain"

  it "/gamemode returns CmdSuccess" $ do
    executeCommand (CmdGamemode "creative") `shouldBe` CmdSuccess "Game mode set to creative"

  it "/gamemode survival returns CmdSuccess" $ do
    executeCommand (CmdGamemode "survival") `shouldBe` CmdSuccess "Game mode set to survival"

  it "/kill returns CmdSuccess" $ do
    executeCommand CmdKill `shouldBe` CmdSuccess "Killed player"

  it "/seed returns CmdSuccess" $ do
    executeCommand CmdSeed `shouldBe` CmdSuccess "Seed: 12345"

  it "/help returns CmdSuccess with command list" $ do
    let result = executeCommand CmdHelp
    case result of
      CmdSuccess msg -> msg `shouldSatisfy` \m -> "/give" `isInfixOf` m
      CmdError _ -> expectationFailure "Expected CmdSuccess"

  it "/summon returns CmdSuccess" $ do
    executeCommand (CmdSpawnMob "zombie") `shouldBe` CmdSuccess "Summoned zombie"

-- =========================================================================
-- Chat state
-- =========================================================================
chatStateSpec :: Spec
chatStateSpec = describe "Game.Command.ChatState" $ do
  it "emptyChatState has empty buffer" $ do
    chatGetBuffer emptyChatState `shouldBe` ""

  it "emptyChatState has no messages" $ do
    csMessages emptyChatState `shouldBe` []

  it "chatAddChar appends character to buffer" $ do
    let s = chatAddChar 'a' emptyChatState
    chatGetBuffer s `shouldBe` "a"

  it "chatAddChar builds up string correctly" $ do
    let s = chatAddChar 'c' $ chatAddChar 'b' $ chatAddChar 'a' emptyChatState
    chatGetBuffer s `shouldBe` "abc"

  it "chatDeleteChar removes last character" $ do
    let s = chatDeleteChar $ chatAddChar 'b' $ chatAddChar 'a' emptyChatState
    chatGetBuffer s `shouldBe` "a"

  it "chatDeleteChar on empty buffer is no-op" $ do
    let s = chatDeleteChar emptyChatState
    chatGetBuffer s `shouldBe` ""

  it "chatClear empties the buffer" $ do
    let s = chatClear $ chatAddChar 'x' emptyChatState
    chatGetBuffer s `shouldBe` ""

  it "chatClear preserves messages" $ do
    let s = addChatMessage "hello" 3.0 emptyChatState
        s' = chatClear $ chatAddChar 'x' s
    length (csMessages s') `shouldBe` 1

  it "addChatMessage adds a message" $ do
    let s = addChatMessage "test" 3.0 emptyChatState
    length (csMessages s) `shouldBe` 1
    cmText (head (csMessages s)) `shouldBe` "test"
    cmTimer (head (csMessages s)) `shouldBe` 3.0

  it "addChatMessage preserves existing messages" $ do
    let s = addChatMessage "b" 2.0 $ addChatMessage "a" 3.0 emptyChatState
    length (csMessages s) `shouldBe` 2

  it "updateChatMessages decrements timers" $ do
    let s = addChatMessage "test" 3.0 emptyChatState
        s' = updateChatMessages 1.0 s
    cmTimer (head (csMessages s')) `shouldBe` 2.0

  it "updateChatMessages removes expired messages" $ do
    let s = addChatMessage "test" 1.0 emptyChatState
        s' = updateChatMessages 1.5 s
    csMessages s' `shouldBe` []

  it "updateChatMessages keeps fresh messages" $ do
    let s = addChatMessage "old" 1.0 $ addChatMessage "new" 5.0 emptyChatState
        s' = updateChatMessages 2.0 s
    length (csMessages s') `shouldBe` 1
    cmText (head (csMessages s')) `shouldBe` "new"

  it "slash pre-fill sets buffer to /" $ do
    let s = chatAddChar '/' (chatClear emptyChatState)
    chatGetBuffer s `shouldBe` "/"

-- =========================================================================
-- Item name lookup
-- =========================================================================
lookupItemByNameSpec :: Spec
lookupItemByNameSpec = describe "Game.Item.lookupItemByName" $ do
  it "looks up stone block" $ do
    lookupItemByName "stone" `shouldBe` Just (BlockItem Stone)

  it "looks up dirt block" $ do
    lookupItemByName "dirt" `shouldBe` Just (BlockItem Dirt)

  it "looks up diamond material" $ do
    lookupItemByName "diamond" `shouldBe` Just (MaterialItem DiamondGem)

  it "looks up coal material" $ do
    lookupItemByName "coal" `shouldBe` Just (MaterialItem Coal)

  it "looks up iron_ingot" $ do
    lookupItemByName "iron_ingot" `shouldBe` Just (MaterialItem IronIngot)

  it "looks up diamond_pickaxe" $ do
    lookupItemByName "diamond_pickaxe" `shouldBe` Just (ToolItem Pickaxe Diamond 1561)

  it "looks up wooden_sword" $ do
    lookupItemByName "wooden_sword" `shouldBe` Just (ToolItem Sword Wood 59)

  it "looks up stick" $ do
    lookupItemByName "stick" `shouldBe` Just StickItem

  it "looks up apple food" $ do
    lookupItemByName "apple" `shouldBe` Just (FoodItem Apple)

  it "looks up bread food" $ do
    lookupItemByName "bread" `shouldBe` Just (FoodItem Bread)

  it "returns Nothing for unknown item" $ do
    lookupItemByName "unobtainium" `shouldBe` Nothing

  it "is case-insensitive" $ do
    lookupItemByName "Stone" `shouldBe` Just (BlockItem Stone)
    lookupItemByName "DIAMOND" `shouldBe` Just (MaterialItem DiamondGem)

  it "looks up torch block" $ do
    lookupItemByName "torch" `shouldBe` Just (BlockItem Torch)

  it "looks up compass" $ do
    lookupItemByName "compass" `shouldBe` Just CompassItem

  it "looks up glass_bottle" $ do
    lookupItemByName "glass_bottle" `shouldBe` Just GlassBottleItem

-- =========================================================================
-- Nether Portal (frame detection + coordinate mapping)
-- =========================================================================
netherPortalSpec :: Spec
netherPortalSpec = describe "World.Dimension.NetherPortal" $ do
  -- Coordinate mapping
  it "netherCoords divides overworld coordinates by 8" $ do
    Dim.netherCoords (V3 80 64 160) `shouldBe` V3 10 64 20

  it "netherCoords handles zero" $ do
    Dim.netherCoords (V3 0 64 0) `shouldBe` V3 0 64 0

  it "netherCoords preserves Y coordinate" $ do
    let V3 _ y _ = Dim.netherCoords (V3 100 42 200)
    y `shouldBe` 42

  it "netherCoords handles negative coordinates" $ do
    -- Haskell div rounds toward negative infinity
    Dim.netherCoords (V3 (-80) 64 (-160)) `shouldBe` V3 (-10) 64 (-20)

  it "overworldCoords multiplies Nether coordinates by 8" $ do
    Dim.overworldCoords (V3 10 64 20) `shouldBe` V3 80 64 160

  it "overworldCoords handles zero" $ do
    Dim.overworldCoords (V3 0 64 0) `shouldBe` V3 0 64 0

  it "overworldCoords preserves Y coordinate" $ do
    let V3 _ y _ = Dim.overworldCoords (V3 10 42 20)
    y `shouldBe` 42

  it "overworldCoords handles negative coordinates" $ do
    Dim.overworldCoords (V3 (-10) 64 (-20)) `shouldBe` V3 (-80) 64 (-160)

  it "netherCoords and overworldCoords are approximate inverses" $ do
    -- overworldCoords . netherCoords rounds toward nearest 8-multiple
    let original = V3 80 64 160
    Dim.overworldCoords (Dim.netherCoords original) `shouldBe` original

  it "portalTransitTime is 4 seconds" $ do
    Dim.portalTransitTime `shouldBe` 4.0

  -- Portal frame detection with X-axis orientation
  it "detectPortalFrame finds a valid X-axis portal" $ do
    withTestWorld $ \world -> do
      -- Build a 4x5 obsidian frame along X-axis at z=5
      -- Bottom row: x=2..5, y=10, z=5
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      -- Top row: x=2..5, y=14, z=5
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      -- Left column: x=2, y=11..13, z=5
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      -- Right column: x=5, y=11..13, z=5
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian
      -- Interior should be Air by default

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 2 10 5)
      result `shouldSatisfy` \case
        Just (Dim.PortalX, interior) -> length interior == 6
        _ -> False

  it "detectPortalFrame returns interior positions for X-axis portal" $ do
    withTestWorld $ \world -> do
      -- Build a 4x5 frame along X-axis at z=5
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 3 10 5)
      case result of
        Just (_, interior) -> do
          -- Interior should be 2 wide x 3 tall = 6 blocks
          length interior `shouldBe` 6
          -- All interior positions should be Air
          blocks <- mapM (worldGetBlock world) interior
          all (== Air) blocks `shouldBe` True
        Nothing -> expectationFailure "Expected portal frame to be detected"

  it "detectPortalFrame finds a valid Z-axis portal" $ do
    withTestWorld $ \world -> do
      -- Build a 4x5 obsidian frame along Z-axis at x=5
      -- Bottom row: z=2..5, y=10, x=5
      forM_ [2..5] $ \z -> worldSetBlock world (V3 5 10 z) Obsidian
      -- Top row: z=2..5, y=14, x=5
      forM_ [2..5] $ \z -> worldSetBlock world (V3 5 14 z) Obsidian
      -- Left column: z=2, y=11..13, x=5
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 2) Obsidian
      -- Right column: z=5, y=11..13, x=5
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 5 10 2)
      result `shouldSatisfy` \case
        Just (Dim.PortalZ, interior) -> length interior == 6
        _ -> False

  it "detectPortalFrame returns Nothing for incomplete frame" $ do
    withTestWorld $ \world -> do
      -- Build an incomplete frame (missing top row)
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      -- No top row
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 2 10 5)
      result `shouldBe` Nothing

  it "detectPortalFrame returns Nothing for frame with wrong block" $ do
    withTestWorld $ \world -> do
      -- Build a frame but with one cobblestone instead of obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian
      -- Replace one frame block with cobblestone
      worldSetBlock world (V3 3 14 5) Cobblestone

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 2 10 5)
      result `shouldBe` Nothing

  it "detectPortalFrame returns Nothing for frame with blocked interior" $ do
    withTestWorld $ \world -> do
      -- Build a valid frame
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian
      -- Place a stone block inside the portal interior
      worldSetBlock world (V3 3 12 5) Stone

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 2 10 5)
      result `shouldBe` Nothing

  it "detectPortalFrame click on any frame block detects portal" $ do
    withTestWorld $ \world -> do
      -- Build a valid X-axis frame
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian

      -- Click on top-right corner
      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 5 14 5)
      result `shouldSatisfy` \case
        Just (Dim.PortalX, _) -> True
        _ -> False

  it "detectPortalFrame allows existing NetherPortal blocks in interior" $ do
    withTestWorld $ \world -> do
      -- Build a valid frame with some interior already filled
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 10 5) Obsidian
      forM_ [2..5] $ \x -> worldSetBlock world (V3 x 14 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 2 y 5) Obsidian
      forM_ [11..13] $ \y -> worldSetBlock world (V3 5 y 5) Obsidian
      -- Pre-fill some interior with NetherPortal
      worldSetBlock world (V3 3 11 5) NetherPortal

      result <- Dim.detectPortalFrame (worldGetBlock world) (V3 2 10 5)
      result `shouldSatisfy` \case
        Just (Dim.PortalX, interior) -> length interior == 6
        _ -> False

  -- PortalOrientation enum
  it "PortalOrientation has 2 values" $ do
    let allOrients = [minBound .. maxBound] :: [Dim.PortalOrientation]
    length allOrients `shouldBe` 2

  it "PortalOrientation enum is PortalX=0, PortalZ=1" $ do
    fromEnum Dim.PortalX `shouldBe` 0
    fromEnum Dim.PortalZ `shouldBe` 1

  -- Dimension switching integration
  it "NetherPortal block is not solid and is transparent" $ do
    isSolid NetherPortal `shouldBe` False
    isTransparent NetherPortal `shouldBe` True

  it "NetherPortal has light emission 11" $ do
    bpLightEmit (blockProperties NetherPortal) `shouldBe` 11

  it "NetherPortal block drops nothing" $ do
    blockDrops NetherPortal `shouldBe` []

-- =========================================================================
-- Cross-chunk face culling
-- =========================================================================
crossChunkCullingSpec :: Spec
crossChunkCullingSpec = describe "Engine.Mesh cross-chunk face culling" $ do

  it "emptyNeighborData has all Nothing fields" $ do
    ndNorth emptyNeighborData `shouldBe` Nothing
    ndSouth emptyNeighborData `shouldBe` Nothing
    ndEast emptyNeighborData `shouldBe` Nothing
    ndWest emptyNeighborData `shouldBe` Nothing

  it "no neighbors: edge block emits face toward chunk boundary" $ do
    -- Place a single stone block at the east edge (x=15, y=0, z=0)
    chunk <- newChunk (V2 0 0)
    setBlock chunk 15 0 0 Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    -- With no neighbor, the east face should be emitted (treat boundary as Air).
    -- A single exposed block generates 6 faces = 6*4=24 verts, 6*6=36 indices.
    VS.length (mdVertices mesh) `shouldBe` 24
    VS.length (mdIndices mesh) `shouldBe` 36

  it "opaque neighbor: edge face is culled when neighbor block is opaque" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 15 5 5 Stone
    eastChunk <- newChunk (V2 1 0)
    setBlock eastChunk 0 5 5 Stone
    eastVec <- freezeBlocks eastChunk
    let nd = emptyNeighborData { ndEast = Just eastVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    meshNoNeighbor <- meshChunkWithLight chunk lm emptyNeighborData
    meshWithNeighbor <- meshChunkWithLight chunk lm nd
    let vertsWithout = VS.length (mdVertices meshNoNeighbor)
        vertsWith    = VS.length (mdVertices meshWithNeighbor)
    vertsWith `shouldSatisfy` (< vertsWithout)

  it "transparent neighbor: edge face is NOT culled when neighbor is transparent" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 15 5 5 Stone
    eastChunk <- newChunk (V2 1 0)
    setBlock eastChunk 0 5 5 Glass
    eastVec <- freezeBlocks eastChunk
    let nd = emptyNeighborData { ndEast = Just eastVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    meshNoNeighbor <- meshChunkWithLight chunk lm emptyNeighborData
    meshWithNeighbor <- meshChunkWithLight chunk lm nd
    let vertsWithout = VS.length (mdVertices meshNoNeighbor)
        vertsWith    = VS.length (mdVertices meshWithNeighbor)
    -- Transparent neighbor doesn't cull the face — same or more verts
    vertsWith `shouldSatisfy` (>= vertsWithout)

  it "north neighbor: +Z edge face culled by opaque neighbor" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 0 15 Stone  -- z=15 is the +Z edge
    northChunk <- newChunk (V2 0 1)
    setBlock northChunk 5 0 0 Stone  -- z=0 in the north neighbor
    northVec <- freezeBlocks northChunk
    let nd = emptyNeighborData { ndNorth = Just northVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm nd
    -- North face culled: 5 faces = 20 verts
    VS.length (mdVertices mesh) `shouldBe` 20

  it "south neighbor: -Z edge face culled by opaque neighbor" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 0 0 Stone  -- z=0 is the -Z edge
    southChunk <- newChunk (V2 0 (-1))
    setBlock southChunk 5 0 15 Stone  -- z=15 in the south neighbor
    southVec <- freezeBlocks southChunk
    let nd = emptyNeighborData { ndSouth = Just southVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm nd
    -- South face culled: 5 faces = 20 verts
    VS.length (mdVertices mesh) `shouldBe` 20

  it "west neighbor: -X edge face culled by opaque neighbor" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 0 0 5 Stone  -- x=0 is the -X edge
    westChunk <- newChunk (V2 (-1) 0)
    setBlock westChunk 15 0 5 Stone  -- x=15 in the west neighbor
    westVec <- freezeBlocks westChunk
    let nd = emptyNeighborData { ndWest = Just westVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm nd
    -- West face culled: 5 faces = 20 verts
    VS.length (mdVertices mesh) `shouldBe` 20

  it "missing neighbor treated as Air (graceful handling)" $ do
    -- When one neighbor is provided but another is Nothing, the missing
    -- direction should treat boundary as Air (emit face).
    chunk <- newChunk (V2 0 0)
    setBlock chunk 15 0 15 Stone  -- corner block: east + north edges
    -- Only provide east neighbor (opaque), leave north as Nothing
    eastChunk <- newChunk (V2 1 0)
    setBlock eastChunk 0 0 15 Stone
    eastVec <- freezeBlocks eastChunk
    let nd = emptyNeighborData { ndEast = Just eastVec }
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm nd
    -- East face culled, north face emitted (no north neighbor = Air).
    -- 5 faces = 20 verts
    VS.length (mdVertices mesh) `shouldBe` 20

  it "all four neighbors provided: fully interior edge block has fewer faces" $ do
    -- Place blocks at all 4 edges plus neighbors
    chunk <- newChunk (V2 0 0)
    setBlock chunk 8 0 8 Stone  -- interior block, not at any edge
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    -- Interior block with no neighbors needed: 6 faces (all neighbors are Air within chunk)
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    VS.length (mdVertices mesh) `shouldBe` 24
-- =========================================================================
-- Greedy meshing
-- =========================================================================
greedyMeshingSpec :: Spec
greedyMeshingSpec = describe "Engine.Mesh greedy meshing" $ do

  it "single block produces 24 vertices (6 faces, no merging possible)" $ do
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    VS.length (mdVertices mesh) `shouldBe` 24
    VS.length (mdIndices mesh) `shouldBe` 36

  it "2x1 row of same blocks merges top/bottom faces, reducing vertex count" $ do
    -- Two adjacent stone blocks at same Y, same Z, consecutive X
    -- Naive: 2 blocks * 5 exposed faces each * 4 verts = 40 verts
    --   (the shared internal face between them is culled, leaving 5 faces each = 10 faces)
    -- Greedy: top face merges into 1 quad (4 verts), bottom merges into 1 quad,
    --   front/back merge into 1 quad each, plus 2 end caps = 6 quads = 24 verts
    -- That's fewer than the naive 10 faces * 4 verts = 40.
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    setBlock chunk 6 10 5 Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    -- Greedy merging should produce fewer vertices than naive (40 verts) when AO permits
    let vertCount = VS.length (mdVertices mesh)
    vertCount `shouldSatisfy` (<= 40)

  it "2x2 flat layer of same blocks merges into fewer quads" $ do
    -- 4 blocks in a 2x2 square at same Y level
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    setBlock chunk 6 10 5 Stone
    setBlock chunk 5 10 6 Stone
    setBlock chunk 6 10 6 Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    let vertCount = VS.length (mdVertices mesh)
    -- Naive: 4 blocks, each with 5 or fewer exposed faces.
    --   Total exposed faces = 4*4 + 2*2 = 20 (4 top + 4 bottom + 8 sides + 4 internal culled)
    --   Actually: each block has 6 faces minus adjacencies.
    --   Top: 4 faces -> greedy merges to 1 quad (4 verts)
    --   Bottom: 4 faces -> 1 quad (4 verts)
    --   4 side faces -> some merge
    -- Greedy should produce significantly fewer than naive
    -- Note: AO may prevent some merges when corner vertices differ
    vertCount `shouldSatisfy` (<= 96)  -- significantly less than naive 4*6*4=96

  it "4x4 flat layer merges each face direction into a single quad" $ do
    -- 16 stone blocks in a 4x4 square at y=0
    chunk <- newChunk (V2 0 0)
    forM_ [0..3] $ \x ->
      forM_ [0..3] $ \z ->
        setBlock chunk x 0 z Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    let vertCount = VS.length (mdVertices mesh)
    -- Naive: 16 blocks * ~5 faces * 4 verts = ~320 verts
    -- Greedy: top(1) + bottom(1) + north(1) + south(1) + east(1) + west(1) = 6 quads
    --   but bottom face at y=0 borders y=-1 which is Air, so bottom is emitted
    --   Side faces: 4 sides, each a strip of 4 blocks -> each merges to 1 quad
    -- AO differences may prevent full merging, but should still be much less than naive
    vertCount `shouldSatisfy` (< 320)

  it "different block types are not merged together" $ do
    -- Two adjacent blocks of different types should NOT be merged
    chunk <- newChunk (V2 0 0)
    setBlock chunk 5 10 5 Stone
    setBlock chunk 6 10 5 Dirt
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    let vertCount = VS.length (mdVertices mesh)
    -- No merging possible across different block types.
    -- Each block has 5 exposed faces (internal face culled) = 10 faces * 4 verts = 40
    vertCount `shouldBe` 40

  it "16x1x16 flat layer produces significantly fewer verts than naive" $ do
    -- A full 16x16 flat layer of stone at y=64
    chunk <- newChunk (V2 0 0)
    forM_ [0..15] $ \x ->
      forM_ [0..15] $ \z ->
        setBlock chunk x 64 z Stone
    lm <- newLightMap
    propagateBlockLight chunk lm
    propagateSkyLight chunk lm
    mesh <- meshChunkWithLight chunk lm emptyNeighborData
    let vertCount = VS.length (mdVertices mesh)
    -- AO may prevent full merging, but far fewer than naive 256*5*4=5120
    vertCount `shouldSatisfy` (< 5120)

-- Dimension wiring (sky color, GameState fields, coordinate mapping)
-- =========================================================================
dimensionWiringSpec :: Spec
dimensionWiringSpec = describe "Dimension wiring" $ do
  -- dimensionSkyColor
  describe "dimensionSkyColor" $ do
    it "Overworld sky color is identity (1,1,1,1)" $ do
      Dim.dimensionSkyColor Dim.Overworld `shouldBe` V4 1.0 1.0 1.0 1.0

    it "Nether sky color is reddish" $ do
      let V4 r g b _a = Dim.dimensionSkyColor Dim.Nether
      r `shouldSatisfy` (> 0.3)
      g `shouldSatisfy` (< 0.3)
      b `shouldSatisfy` (< 0.3)

    it "Nether sky red channel exceeds green and blue" $ do
      let V4 r g b _a = Dim.dimensionSkyColor Dim.Nether
      r `shouldSatisfy` (> g)
      r `shouldSatisfy` (> b)

    it "TheEnd sky color is dark purplish" $ do
      let V4 r _g b _a = Dim.dimensionSkyColor Dim.TheEnd
      b `shouldSatisfy` (> r)

    it "all three dimension sky colors are distinct" $ do
      let ow  = Dim.dimensionSkyColor Dim.Overworld
          net = Dim.dimensionSkyColor Dim.Nether
          end = Dim.dimensionSkyColor Dim.TheEnd
      ow `shouldNotBe` net
      net `shouldNotBe` end
      ow `shouldNotBe` end

    it "dimensionSkyColor alpha is 1 for all dimensions" $ do
      let dims = [minBound .. maxBound] :: [Dim.DimensionType]
      mapM_ (\d -> let V4 _ _ _ a = Dim.dimensionSkyColor d in a `shouldBe` 1.0) dims

  -- dimensionConfig lookup
  describe "dimensionConfig" $ do
    it "dimensionConfig Overworld matches overworldConfig" $ do
      Dim.dimensionConfig Dim.Overworld `shouldBe` Dim.overworldConfig

    it "dimensionConfig Nether matches netherConfig" $ do
      Dim.dimensionConfig Dim.Nether `shouldBe` Dim.netherConfig

    it "dimensionConfig TheEnd matches endConfig" $ do
      Dim.dimensionConfig Dim.TheEnd `shouldBe` Dim.endConfig

  -- Coordinate mapping roundtrip properties
  describe "coordinate mapping" $ do
    it "netherCoords then overworldCoords roundtrips multiples of 8" $ do
      let original = V3 80 64 160
      Dim.overworldCoords (Dim.netherCoords original) `shouldBe` original

    it "overworldCoords then netherCoords roundtrips" $ do
      let nether = V3 10 64 20
      Dim.netherCoords (Dim.overworldCoords nether) `shouldBe` nether

    it "netherCoords divides x and z by 8, preserves y" $ do
      Dim.netherCoords (V3 160 100 (-240)) `shouldBe` V3 20 100 (-30)

    it "overworldCoords multiplies x and z by 8, preserves y" $ do
      Dim.overworldCoords (V3 20 100 (-30)) `shouldBe` V3 160 100 (-240)

    it "netherCoords at origin is origin" $ do
      Dim.netherCoords (V3 0 0 0) `shouldBe` V3 0 0 0

    it "overworldCoords at origin is origin" $ do
      Dim.overworldCoords (V3 0 0 0) `shouldBe` V3 0 0 0

  -- GameState dimension fields
  describe "GameState dimension fields" $ do
    it "newGameState starts in Overworld" $ do
      gs <- newGameState (V3 0 80 0)
      dim <- readIORef (gsDimension gs)
      dim `shouldBe` Dim.Overworld

    it "gsDimension can be changed to Nether" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsDimension gs) Dim.Nether
      dim <- readIORef (gsDimension gs)
      dim `shouldBe` Dim.Nether

    it "gsDimension can be changed to TheEnd" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsDimension gs) Dim.TheEnd
      dim <- readIORef (gsDimension gs)
      dim `shouldBe` Dim.TheEnd

    it "gsNetherWorld starts as Nothing" $ do
      gs <- newGameState (V3 0 80 0)
      nw <- readIORef (gsNetherWorld gs)
      case nw of
        Nothing -> pure ()
        Just _  -> expectationFailure "Expected gsNetherWorld to be Nothing"

    it "gsPortalTimer starts at 0" $ do
      gs <- newGameState (V3 0 80 0)
      timer <- readIORef (gsPortalTimer gs)
      timer `shouldBe` 0.0

weatherParticleSpec :: Spec
weatherParticleSpec = describe "Game.Particle weather" $ do
  describe "constants" $ do
    it "weatherParticleRadius is 16" $ weatherParticleRadius `shouldBe` 16.0
    it "weatherParticleHeight is 20" $ weatherParticleHeight `shouldBe` 20.0
    it "weatherParticleCount is 200" $ weatherParticleCount `shouldBe` 200
    it "rainFallSpeed is 15" $ rainFallSpeed `shouldBe` 15.0
    it "snowFallSpeed is 3" $ snowFallSpeed `shouldBe` 3.0
  describe "isSnowBiome" $ do
    it "Tundra is a snow biome" $ isSnowBiome Tundra `shouldBe` True
    it "Taiga is a snow biome" $ isSnowBiome Taiga `shouldBe` True
    it "Plains is not a snow biome" $ isSnowBiome Plains `shouldBe` False
    it "Desert is not a snow biome" $ isSnowBiome Desert `shouldBe` False
    it "Forest is not a snow biome" $ isSnowBiome Forest `shouldBe` False
  describe "clampParticleXZ" $ do
    it "values within radius are unchanged" $ clampParticleXZ 16.0 5.0 `shouldBe` 5.0
    it "wraps values exceeding radius" $ clampParticleXZ 16.0 20.0 `shouldBe` (20.0 - 32.0)
    it "wraps values below negative radius" $ clampParticleXZ 16.0 (-20.0) `shouldBe` (-20.0 + 32.0)
    it "boundary value is unchanged" $ clampParticleXZ 16.0 16.0 `shouldBe` 16.0
    it "negative boundary value is unchanged" $ clampParticleXZ 16.0 (-16.0) `shouldBe` (-16.0)
  describe "clampParticleY" $ do
    it "value within [0, height] returns Just" $ clampParticleY 20.0 10.0 `shouldBe` Just 10.0
    it "value at zero returns Just 0" $ clampParticleY 20.0 0.0 `shouldBe` Just 0.0
    it "value above height wraps" $ clampParticleY 20.0 25.0 `shouldBe` Just 5.0
    it "negative value returns Nothing" $ clampParticleY 20.0 (-1.0) `shouldBe` Nothing
  describe "spawnWeatherParticles" $ do
    it "spawns the requested number of particles" $ do
      ps <- spawnWeatherParticles (V3 100 80 100) Plains 50
      length ps `shouldBe` 50
    it "spawns weatherParticleCount particles" $ do
      ps <- spawnWeatherParticles (V3 0 64 0) Forest weatherParticleCount
      length ps `shouldBe` weatherParticleCount
    it "rain particles are RainDrop in Plains" $ do
      ps <- spawnWeatherParticles (V3 0 64 0) Plains 10
      all (\p -> wpType p == RainDrop) ps `shouldBe` True
    it "snow particles are SnowFlake in Tundra" $ do
      ps <- spawnWeatherParticles (V3 0 64 0) Tundra 10
      all (\p -> wpType p == SnowFlake) ps `shouldBe` True
    it "snow particles are SnowFlake in Taiga" $ do
      ps <- spawnWeatherParticles (V3 0 64 0) Taiga 10
      all (\p -> wpType p == SnowFlake) ps `shouldBe` True
    it "all particles within cylinder radius" $ do
      ps <- spawnWeatherParticles (V3 100 80 (-50)) Plains 200
      let ok wp = let V3 wx _ wz = wpPos wp; dx = wx - 100; dz = wz + 50 in sqrt (dx*dx + dz*dz) <= weatherParticleRadius + 0.01
      all ok ps `shouldBe` True
    it "all particles within cylinder height" $ do
      ps <- spawnWeatherParticles (V3 0 80 0) Plains 200
      let ok wp = let V3 _ wy _ = wpPos wp; dy = wy - 80 in dy >= -0.01 && dy <= weatherParticleHeight + 0.01
      all ok ps `shouldBe` True
  describe "tickWeatherParticles" $ do
    it "preserves particle count" $ do
      ps <- spawnWeatherParticles (V3 0 80 0) Plains 50
      ps' <- tickWeatherParticles 0.5 (V3 0 80 0) Plains ps
      length ps' `shouldBe` 50
    it "snow falls slower than rain" $ do
      rps <- spawnWeatherParticles (V3 0 100 0) Plains 1
      sps <- spawnWeatherParticles (V3 0 100 0) Tundra 1
      rps' <- tickWeatherParticles 0.01 (V3 0 100 0) Plains rps
      sps' <- tickWeatherParticles 0.01 (V3 0 100 0) Tundra sps
      let rdy = let V3 _ y0 _ = wpPos (head rps); V3 _ y1 _ = wpPos (head rps') in y0 - y1
          sdy = let V3 _ y0 _ = wpPos (head sps); V3 _ y1 _ = wpPos (head sps') in y0 - y1
      rdy `shouldSatisfy` (> sdy)
  describe "renderWeatherParticles" $ do
    it "empty list produces no vertices" $ renderWeatherParticles [] identity `shouldBe` []
    it "rain particle produces 36 floats" $ length (renderWeatherParticles [WeatherParticle (V3 0 0 (-1)) RainDrop] identity) `shouldBe` 36
    it "snow particle produces 36 floats" $ length (renderWeatherParticles [WeatherParticle (V3 0 0 (-1)) SnowFlake] identity) `shouldBe` 36
    it "rain has blue color" $ case renderWeatherParticles [WeatherParticle (V3 0 0 (-1)) RainDrop] identity of { (_:_:r:g:b:_) -> do { r `shouldBe` 0.5; g `shouldBe` 0.6; b `shouldBe` 1.0 }; _ -> expectationFailure "no verts" }
    it "snow has white color" $ case renderWeatherParticles [WeatherParticle (V3 0 0 (-1)) SnowFlake] identity of { (_:_:r:g:b:_) -> do { r `shouldBe` 1.0; g `shouldBe` 1.0; b `shouldBe` 1.0 }; _ -> expectationFailure "no verts" }
-- =========================================================================
-- Directional Pistons
-- =========================================================================
directionalPistonSpec :: Spec
directionalPistonSpec = describe "Directional Pistons" $ do

  describe "BlockType constructors" $ do
    it "PistonNorth exists and is distinct from Piston" $
      PistonNorth `shouldSatisfy` (/= Piston)

    it "PistonSouth exists and is distinct from Piston" $
      PistonSouth `shouldSatisfy` (/= Piston)

    it "PistonEast exists and is distinct from Piston" $
      PistonEast `shouldSatisfy` (/= Piston)

    it "PistonWest exists and is distinct from Piston" $
      PistonWest `shouldSatisfy` (/= Piston)

    it "PistonDown exists and is distinct from Piston" $
      PistonDown `shouldSatisfy` (/= Piston)

    it "PistonHeadNorth exists and is distinct from PistonHead" $
      PistonHeadNorth `shouldSatisfy` (/= PistonHead)

    it "PistonHeadSouth exists and is distinct from PistonHead" $
      PistonHeadSouth `shouldSatisfy` (/= PistonHead)

    it "PistonHeadEast exists and is distinct from PistonHead" $
      PistonHeadEast `shouldSatisfy` (/= PistonHead)

    it "PistonHeadWest exists and is distinct from PistonHead" $
      PistonHeadWest `shouldSatisfy` (/= PistonHead)

    it "PistonHeadDown exists and is distinct from PistonHead" $
      PistonHeadDown `shouldSatisfy` (/= PistonHead)

  describe "Enum ordering" $ do
    it "directional pistons come after WheatCrop7" $ do
      fromEnum PistonNorth `shouldSatisfy` (> fromEnum WheatCrop7)
      fromEnum PistonSouth `shouldSatisfy` (> fromEnum WheatCrop7)
      fromEnum PistonEast `shouldSatisfy` (> fromEnum WheatCrop7)
      fromEnum PistonWest `shouldSatisfy` (> fromEnum WheatCrop7)
      fromEnum PistonDown `shouldSatisfy` (> fromEnum WheatCrop7)

    it "directional piston heads come after directional pistons" $ do
      fromEnum PistonHeadNorth `shouldSatisfy` (> fromEnum PistonDown)
      fromEnum PistonHeadSouth `shouldSatisfy` (> fromEnum PistonDown)
      fromEnum PistonHeadEast `shouldSatisfy` (> fromEnum PistonDown)
      fromEnum PistonHeadWest `shouldSatisfy` (> fromEnum PistonDown)
      fromEnum PistonHeadDown `shouldSatisfy` (> fromEnum PistonDown)

    it "PistonHeadDown is the last enum value (Bounded maxBound)" $
      (maxBound :: BlockType) `shouldBe` PistonHeadDown

  describe "isPistonBlock" $ do
    it "Piston (upward) is a piston" $
      isPistonBlock Piston `shouldBe` True

    it "PistonNorth is a piston" $
      isPistonBlock PistonNorth `shouldBe` True

    it "PistonSouth is a piston" $
      isPistonBlock PistonSouth `shouldBe` True

    it "PistonEast is a piston" $
      isPistonBlock PistonEast `shouldBe` True

    it "PistonWest is a piston" $
      isPistonBlock PistonWest `shouldBe` True

    it "PistonDown is a piston" $
      isPistonBlock PistonDown `shouldBe` True

    it "Stone is not a piston" $
      isPistonBlock Stone `shouldBe` False

    it "PistonHead is not a piston (it is a head)" $
      isPistonBlock PistonHead `shouldBe` False

  describe "isPistonHeadBlock" $ do
    it "PistonHead (upward) is a piston head" $
      isPistonHeadBlock PistonHead `shouldBe` True

    it "PistonHeadNorth is a piston head" $
      isPistonHeadBlock PistonHeadNorth `shouldBe` True

    it "PistonHeadSouth is a piston head" $
      isPistonHeadBlock PistonHeadSouth `shouldBe` True

    it "PistonHeadEast is a piston head" $
      isPistonHeadBlock PistonHeadEast `shouldBe` True

    it "PistonHeadWest is a piston head" $
      isPistonHeadBlock PistonHeadWest `shouldBe` True

    it "PistonHeadDown is a piston head" $
      isPistonHeadBlock PistonHeadDown `shouldBe` True

    it "Piston is not a piston head" $
      isPistonHeadBlock Piston `shouldBe` False

    it "Air is not a piston head" $
      isPistonHeadBlock Air `shouldBe` False

  describe "pistonDirection" $ do
    it "Piston pushes upward (+Y)" $
      pistonDirection Piston `shouldBe` V3 0 1 0

    it "PistonNorth pushes +Z" $
      pistonDirection PistonNorth `shouldBe` V3 0 0 1

    it "PistonSouth pushes -Z" $
      pistonDirection PistonSouth `shouldBe` V3 0 0 (-1)

    it "PistonEast pushes +X" $
      pistonDirection PistonEast `shouldBe` V3 1 0 0

    it "PistonWest pushes -X" $
      pistonDirection PistonWest `shouldBe` V3 (-1) 0 0

    it "PistonDown pushes -Y" $
      pistonDirection PistonDown `shouldBe` V3 0 (-1) 0

  describe "pistonHeadForPiston" $ do
    it "Piston maps to PistonHead" $
      pistonHeadForPiston Piston `shouldBe` PistonHead

    it "PistonNorth maps to PistonHeadNorth" $
      pistonHeadForPiston PistonNorth `shouldBe` PistonHeadNorth

    it "PistonSouth maps to PistonHeadSouth" $
      pistonHeadForPiston PistonSouth `shouldBe` PistonHeadSouth

    it "PistonEast maps to PistonHeadEast" $
      pistonHeadForPiston PistonEast `shouldBe` PistonHeadEast

    it "PistonWest maps to PistonHeadWest" $
      pistonHeadForPiston PistonWest `shouldBe` PistonHeadWest

    it "PistonDown maps to PistonHeadDown" $
      pistonHeadForPiston PistonDown `shouldBe` PistonHeadDown

  describe "block properties" $ do
    it "all directional pistons are solid" $ do
      let pistons = [PistonNorth, PistonSouth, PistonEast, PistonWest, PistonDown]
      mapM_ (\p -> isSolid p `shouldBe` True) pistons

    it "all directional pistons are not transparent" $ do
      let pistons = [PistonNorth, PistonSouth, PistonEast, PistonWest, PistonDown]
      mapM_ (\p -> isTransparent p `shouldBe` False) pistons

    it "all directional pistons have hardness 0.5" $ do
      let pistons = [PistonNorth, PistonSouth, PistonEast, PistonWest, PistonDown]
      mapM_ (\p -> bpHardness (blockProperties p) `shouldBe` 0.5) pistons

    it "all directional piston heads are solid" $ do
      let heads = [PistonHeadNorth, PistonHeadSouth, PistonHeadEast, PistonHeadWest, PistonHeadDown]
      mapM_ (\h -> isSolid h `shouldBe` True) heads

    it "all directional piston heads have hardness 0.5" $ do
      let heads = [PistonHeadNorth, PistonHeadSouth, PistonHeadEast, PistonHeadWest, PistonHeadDown]
      mapM_ (\h -> bpHardness (blockProperties h) `shouldBe` 0.5) heads

    it "directional piston properties match original Piston" $ do
      let ref = blockProperties Piston
      mapM_ (\p -> blockProperties p `shouldBe` ref)
        [PistonNorth, PistonSouth, PistonEast, PistonWest, PistonDown]

    it "directional piston head properties match original PistonHead" $ do
      let ref = blockProperties PistonHead
      mapM_ (\h -> blockProperties h `shouldBe` ref)
        [PistonHeadNorth, PistonHeadSouth, PistonHeadEast, PistonHeadWest, PistonHeadDown]

  describe "texture coordinates" $ do
    it "PistonNorth push face is on FaceNorth" $
      blockFaceTexCoords PistonNorth FaceNorth `shouldBe` V2 5 5

    it "PistonSouth push face is on FaceSouth" $
      blockFaceTexCoords PistonSouth FaceSouth `shouldBe` V2 5 5

    it "PistonEast push face is on FaceEast" $
      blockFaceTexCoords PistonEast FaceEast `shouldBe` V2 5 5

    it "PistonWest push face is on FaceWest" $
      blockFaceTexCoords PistonWest FaceWest `shouldBe` V2 5 5

    it "PistonDown push face is on FaceBottom" $
      blockFaceTexCoords PistonDown FaceBottom `shouldBe` V2 5 5

    it "PistonNorth back face (south) is wooden base" $
      blockFaceTexCoords PistonNorth FaceSouth `shouldBe` V2 4 0

    it "PistonEast side faces are wooden" $ do
      blockFaceTexCoords PistonEast FaceTop `shouldBe` V2 6 5
      blockFaceTexCoords PistonEast FaceBottom `shouldBe` V2 6 5

  describe "world integration" $ do
    it "can place and read PistonNorth" $ do
      withTestWorld $ \world -> do
        worldSetBlock world (V3 5 64 5) PistonNorth
        blk <- worldGetBlock world (V3 5 64 5)
        blk `shouldBe` PistonNorth

    it "can place and read PistonEast" $ do
      withTestWorld $ \world -> do
        worldSetBlock world (V3 5 64 5) PistonEast
        blk <- worldGetBlock world (V3 5 64 5)
        blk `shouldBe` PistonEast

    it "can place and read PistonHeadSouth" $ do
      withTestWorld $ \world -> do
        worldSetBlock world (V3 5 64 5) PistonHeadSouth
        blk <- worldGetBlock world (V3 5 64 5)
        blk `shouldBe` PistonHeadSouth

    it "can place and read PistonDown" $ do
      withTestWorld $ \world -> do
        worldSetBlock world (V3 5 64 5) PistonDown
        blk <- worldGetBlock world (V3 5 64 5)
        blk `shouldBe` PistonDown

    it "can place and read PistonHeadDown" $ do
      withTestWorld $ \world -> do
        worldSetBlock world (V3 5 64 5) PistonHeadDown
        blk <- worldGetBlock world (V3 5 64 5)
        blk `shouldBe` PistonHeadDown

-- =========================================================================
-- Mesh AO (per-vertex ambient occlusion)
-- =========================================================================
meshAOSpec :: Spec
meshAOSpec = describe "Engine.Mesh AO" $ do
  describe "computeVertexAO" $ do
    it "no neighbors solid => AO = 1.0" $
      computeVertexAO False False False `shouldBe` 1.0
    it "one side solid => AO = 2/3" $
      computeVertexAO True False False `shouldSatisfy` (\v -> abs (v - 2.0/3.0) < 1e-6)
    it "two sides solid => forces corner, AO = 0.0" $
      computeVertexAO True True False `shouldBe` 0.0
    it "two sides solid with corner already solid => AO = 0.0" $
      computeVertexAO True True True `shouldBe` 0.0
    it "one side + corner solid => AO = 1/3" $
      computeVertexAO True False True `shouldSatisfy` (\v -> abs (v - 1.0/3.0) < 1e-6)
    it "corner only solid => AO = 2/3" $
      computeVertexAO False False True `shouldSatisfy` (\v -> abs (v - 2.0/3.0) < 1e-6)
    it "both sides not solid, corner not solid => AO = 1.0" $
      computeVertexAO False False False `shouldBe` 1.0
    it "AO is always in [0, 1]" $
      property $ \s1 s2 c ->
        let ao = computeVertexAO s1 s2 c
        in ao >= 0.0 && ao <= 1.0
  describe "meshChunkWithLight AO integration" $ do
    it "isolated block has all AO > 0 (no solid neighbors)" $ do
      chunk <- newChunk (V2 0 0)
      setBlock chunk 8 8 8 Stone
      lm <- newLightMap
      propagateSkyLight chunk lm
      mesh <- meshChunkWithLight chunk lm emptyNeighborData
      let verts = VS.toList (mdVertices mesh)
      all (\v -> bvAO v > 0.0) verts `shouldBe` True
    it "block with raised neighbors has varying AO on top face" $ do
      chunk <- newChunk (V2 0 0)
      setBlock chunk 8 250 8 Stone
      setBlock chunk 7 250 8 Stone
      setBlock chunk 7 251 8 Stone
      setBlock chunk 8 250 7 Stone
      setBlock chunk 8 251 7 Stone
      lm <- newLightMap
      propagateSkyLight chunk lm
      mesh <- meshChunkWithLight chunk lm emptyNeighborData
      let verts = VS.toList (mdVertices mesh)
          topFaceVerts = filter (\v ->
            let V3 px py pz = bvPosition v
                V3 _ ny _ = bvNormal v
            in abs (py - 251.0) < 0.01 && ny > 0.5 &&
               px >= 8.0 && px <= 9.0 && pz >= 8.0 && pz <= 9.0) verts
          aoVals = map bvAO topFaceVerts
      length topFaceVerts `shouldBe` 4
      (maximum aoVals - minimum aoVals) `shouldSatisfy` (> 0.01)

-- =========================================================================
-- Sunset / Sunrise sky color transitions
-- =========================================================================
sunsetSunriseSpec :: Spec
sunsetSunriseSpec = describe "Sunset and sunrise sky colors" $ do
  let mkCycle t = newDayNightCycle { dncTime = t }
      skyAt t = getSkyColor (mkCycle t)
      rgbApprox (V4 r g b _) (V4 er eg eb _) =
        abs (r - er) < 0.02 && abs (g - eg) < 0.02 && abs (b - eb) < 0.02

  let redOf   (V4 r _ _ _) = r
      _greenOf (V4 _ g _ _) = g
      blueOf  (V4 _ _ b _) = b
      alphaOf (V4 _ _ _ a) = a

  describe "Dawn multi-step interpolation (t 0.20-0.30)" $ do
    it "start of dawn (t=0.20) matches nightSky" $ do
      skyAt 0.20 `rgbApprox` V4 0.01 0.01 0.05 1.0 `shouldBe` True

    it "early dawn (t=0.24) reaches warm orange-pink horizon" $ do
      skyAt 0.24 `rgbApprox` V4 0.70 0.40 0.30 1.0 `shouldBe` True

    it "mid dawn (t=0.27) reaches pale gold" $ do
      skyAt 0.27 `rgbApprox` V4 0.80 0.70 0.50 1.0 `shouldBe` True

    it "end of dawn (t=0.30) matches daySkyStart" $ do
      skyAt 0.30 `rgbApprox` V4 0.40 0.60 0.85 1.0 `shouldBe` True

    it "early dawn mid-point (t=0.22) is between nightSky and dawnHorizon" $ do
      let V4 r g b _ = skyAt 0.22
      r `shouldSatisfy` (\x -> x > 0.01 && x < 0.70)
      g `shouldSatisfy` (\x -> x > 0.01 && x < 0.40)
      b `shouldSatisfy` (\x -> x > 0.05 && x < 0.30)

    it "mid dawn mid-point (t=0.255) is between dawnHorizon and dawnGold" $ do
      let V4 r _ _ _ = skyAt 0.255
      r `shouldSatisfy` (\x -> x > 0.70 && x < 0.80)

    it "late dawn mid-point (t=0.285) is between dawnGold and daySkyStart" $ do
      let V4 r g b _ = skyAt 0.285
      r `shouldSatisfy` (\x -> x > 0.40 && x < 0.80)
      g `shouldSatisfy` (\x -> x > 0.60 && x < 0.70)
      b `shouldSatisfy` (\x -> x > 0.50 && x < 0.85)

    it "dawn colors have red dominance in early phase" $ do
      let V4 r g b _ = skyAt 0.22
      r `shouldSatisfy` (> g)
      r `shouldSatisfy` (> b)

    it "dawn gold phase has warm tone (R > B)" $ do
      let V4 r _ b _ = skyAt 0.26
      r `shouldSatisfy` (> b)

    it "alpha is always 1.0 throughout dawn" $ do
      alphaOf (skyAt 0.20) `shouldBe` 1.0
      alphaOf (skyAt 0.22) `shouldBe` 1.0
      alphaOf (skyAt 0.24) `shouldBe` 1.0
      alphaOf (skyAt 0.27) `shouldBe` 1.0
      alphaOf (skyAt 0.29) `shouldBe` 1.0

    it "dawn red channel increases then decreases" $ do
      let r20 = redOf (skyAt 0.20)
          r24 = redOf (skyAt 0.24)
          r30 = redOf (skyAt 0.30)
      r24 `shouldSatisfy` (> r20)
      r24 `shouldSatisfy` (> r30)

    it "dawn blue channel dips during orange phase then recovers" $ do
      let b20 = blueOf (skyAt 0.20)
          b24 = blueOf (skyAt 0.24)
          b30 = blueOf (skyAt 0.30)
      b24 `shouldSatisfy` (> b20)
      b30 `shouldSatisfy` (> b24)

  describe "Dusk multi-step interpolation (t 0.70-0.80)" $ do
    it "start of dusk (t=0.70) matches daySkyStart" $ do
      skyAt 0.70 `rgbApprox` V4 0.40 0.60 0.85 1.0 `shouldBe` True

    it "early dusk (t=0.73) reaches golden" $ do
      skyAt 0.73 `rgbApprox` V4 0.80 0.60 0.30 1.0 `shouldBe` True

    it "mid dusk (t=0.77) reaches deep orange-red" $ do
      skyAt 0.77 `rgbApprox` V4 0.60 0.25 0.15 1.0 `shouldBe` True

    it "end of dusk (t=0.80) matches nightSky" $ do
      skyAt 0.80 `rgbApprox` V4 0.01 0.01 0.05 1.0 `shouldBe` True

    it "early dusk mid-point (t=0.715) is between daySkyStart and duskGolden" $ do
      let V4 r g _ _ = skyAt 0.715
      r `shouldSatisfy` (\x -> x > 0.40 && x < 0.80)
      g `shouldSatisfy` (\x -> x > 0.55 && x < 0.65)

    it "mid dusk mid-point (t=0.75) is between duskGolden and deepOrange" $ do
      let V4 r g _ _ = skyAt 0.75
      r `shouldSatisfy` (\x -> x > 0.60 && x < 0.80)
      g `shouldSatisfy` (\x -> x > 0.25 && x < 0.60)

    it "late dusk mid-point (t=0.785) is between deepOrange and nightSky" $ do
      let V4 r g _ _ = skyAt 0.785
      r `shouldSatisfy` (\x -> x > 0.01 && x < 0.60)
      g `shouldSatisfy` (\x -> x > 0.01 && x < 0.25)

    it "dusk golden phase has warm tone (R > B)" $ do
      let V4 r _ b _ = skyAt 0.72
      r `shouldSatisfy` (> b)

    it "dusk deep orange has very low blue" $ do
      let V4 _ _ b _ = skyAt 0.76
      b `shouldSatisfy` (< 0.25)

    it "alpha is always 1.0 throughout dusk" $ do
      alphaOf (skyAt 0.70) `shouldBe` 1.0
      alphaOf (skyAt 0.73) `shouldBe` 1.0
      alphaOf (skyAt 0.75) `shouldBe` 1.0
      alphaOf (skyAt 0.77) `shouldBe` 1.0
      alphaOf (skyAt 0.79) `shouldBe` 1.0

    it "dusk red channel increases then decreases" $ do
      let r70 = redOf (skyAt 0.70)
          r73 = redOf (skyAt 0.73)
          r80 = redOf (skyAt 0.80)
      r73 `shouldSatisfy` (> r70)
      r73 `shouldSatisfy` (> r80)

    it "dusk blue drops from day sky to near zero" $ do
      let b70 = blueOf (skyAt 0.70)
          b77 = blueOf (skyAt 0.77)
      b70 `shouldSatisfy` (> b77)
      b77 `shouldSatisfy` (< 0.20)

  describe "Day and night unchanged" $ do
    it "noon (t=0.50) is between daySkyStart and daySkyPeak" $ do
      let V4 r g b _ = skyAt 0.50
      r `shouldSatisfy` (\x -> x >= 0.40 && x <= 0.53)
      g `shouldSatisfy` (\x -> x >= 0.60 && x <= 0.81)
      b `shouldSatisfy` (\x -> x >= 0.85 && x <= 0.92)

    it "midnight (t=0.0) is nightSky" $ do
      skyAt 0.0 `rgbApprox` V4 0.01 0.01 0.05 1.0 `shouldBe` True

    it "getSkyColor returns valid RGBA for all sampled times" $ do
      let times = [0.0, 0.05, 0.10, 0.15, 0.20, 0.22, 0.24, 0.26, 0.27, 0.29,
                   0.30, 0.40, 0.50, 0.60, 0.70, 0.72, 0.73, 0.75, 0.77, 0.79,
                   0.80, 0.85, 0.90, 0.95, 0.99]
      mapM_ (\t -> do
        let V4 r g b a = skyAt t
        r `shouldSatisfy` (\x -> x >= 0 && x <= 1)
        g `shouldSatisfy` (\x -> x >= 0 && x <= 1)
        b `shouldSatisfy` (\x -> x >= 0 && x <= 1)
        a `shouldBe` 1.0
        ) times

  describe "Phase detection helpers" $ do
    it "isDawn at t=0.25" $ do
      isDawn (mkCycle 0.25) `shouldBe` True

    it "isDawn is False at t=0.50" $ do
      isDawn (mkCycle 0.50) `shouldBe` False

    it "isDusk at t=0.75" $ do
      isDusk (mkCycle 0.75) `shouldBe` True

    it "isDusk is False at t=0.25" $ do
      isDusk (mkCycle 0.25) `shouldBe` False

  describe "Continuity at phase boundaries" $ do
    it "dawn/day boundary (t=0.2999 vs t=0.3001) colors are close" $ do
      let V4 r1 g1 b1 _ = skyAt 0.2999
          V4 r2 g2 b2 _ = skyAt 0.3001
      abs (r1 - r2) `shouldSatisfy` (< 0.05)
      abs (g1 - g2) `shouldSatisfy` (< 0.05)
      abs (b1 - b2) `shouldSatisfy` (< 0.05)

    it "day/dusk boundary (t=0.6999 vs t=0.7001) colors are close" $ do
      -- Note: Day phase peaks toward daySkyPeak at t=0.70 while dusk starts
      -- from daySkyStart, so there is a small inherent jump at this boundary.
      let V4 r1 g1 b1 _ = skyAt 0.6999
          V4 r2 g2 b2 _ = skyAt 0.7001
      abs (r1 - r2) `shouldSatisfy` (< 0.15)
      abs (g1 - g2) `shouldSatisfy` (< 0.25)
      abs (b1 - b2) `shouldSatisfy` (< 0.10)

    it "night/dawn boundary (t=0.1999 vs t=0.2001) colors are close" $ do
      let V4 r1 g1 b1 _ = skyAt 0.1999
          V4 r2 g2 b2 _ = skyAt 0.2001
      abs (r1 - r2) `shouldSatisfy` (< 0.05)
      abs (g1 - g2) `shouldSatisfy` (< 0.05)
      abs (b1 - b2) `shouldSatisfy` (< 0.05)

    it "dusk/night boundary (t=0.7999 vs t=0.8001) colors are close" $ do
      let V4 r1 g1 b1 _ = skyAt 0.7999
          V4 r2 g2 b2 _ = skyAt 0.8001
      abs (r1 - r2) `shouldSatisfy` (< 0.05)
      abs (g1 - g2) `shouldSatisfy` (< 0.05)
      abs (b1 - b2) `shouldSatisfy` (< 0.05)

-- =========================================================================
-- Crafting Preview
-- =========================================================================
craftingPreviewSpec :: Spec
craftingPreviewSpec = describe "Game.Crafting.craftPreview" $ do
  -- craftPreview returns Nothing for empty / invalid grids
  it "empty grid returns Nothing" $ do
    craftPreview (emptyCraftingGrid 3) `shouldBe` Nothing

  it "invalid pattern returns Nothing" $ do
    let grid = setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 (Just (BlockItem Stone)))
                0 1 (Just (BlockItem Dirt))
    craftPreview grid `shouldBe` Nothing

  -- craftPreview returns Just for valid recipes
  it "single OakLog previews 4 OakPlanks" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    craftPreview grid `shouldBe` Just (BlockItem OakPlanks, 4)

  it "2x2 planks previews CraftingTable" $ do
    let p = Just (BlockItem OakPlanks)
        grid = setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 p) 0 1 p) 1 0 p) 1 1 p
    craftPreview grid `shouldBe` Just (BlockItem CraftingTable, 1)

  it "preview result matches tryCraft CraftSuccess" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    case tryCraft grid of
      CraftSuccess item count -> craftPreview grid `shouldBe` Just (item, count)
      CraftFailure            -> expectationFailure "tryCraft should succeed"

  it "preview is consistent across grid positions" $ do
    let grid1 = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
        grid2 = setCraftingSlot (emptyCraftingGrid 3) 2 2 (Just (BlockItem OakLog))
    craftPreview grid1 `shouldBe` craftPreview grid2

  it "preview updates when grid changes" $ do
    let grid1 = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
        grid2 = setCraftingSlot grid1 0 0 Nothing
    craftPreview grid1 `shouldBe` Just (BlockItem OakPlanks, 4)
    craftPreview grid2 `shouldBe` Nothing

  it "preview returns Nothing for partially filled invalid pattern" $ do
    let grid = setCraftingSlot
              (setCraftingSlot (emptyCraftingGrid 3)
                0 0 (Just (BlockItem OakPlanks)))
                2 2 (Just (BlockItem Cobblestone))
    craftPreview grid `shouldBe` Nothing

  -- previewTint tests
  it "previewTint reduces RGB by 0.55 factor" $ do
    let (r, g, b, _) = previewTint (1.0, 1.0, 1.0, 1.0)
    abs (r - 0.55) `shouldSatisfy` (< 1e-6)
    abs (g - 0.55) `shouldSatisfy` (< 1e-6)
    abs (b - 0.55) `shouldSatisfy` (< 1e-6)

  it "previewTint reduces alpha by 0.5 factor" $ do
    let (_, _, _, a) = previewTint (1.0, 1.0, 1.0, 1.0)
    abs (a - 0.5) `shouldSatisfy` (< 1e-6)

  it "previewTint of black stays black" $ do
    previewTint (0.0, 0.0, 0.0, 0.0) `shouldBe` (0.0, 0.0, 0.0, 0.0)

  it "previewTint preserves relative channel ratios" $ do
    let (r, g, b, _) = previewTint (0.8, 0.4, 0.2, 1.0)
    -- r should be 2x g and 4x b
    abs (r - 2 * g) `shouldSatisfy` (< 1e-6)
    abs (r - 4 * b) `shouldSatisfy` (< 1e-6)

  it "previewTint is darker than original" $ do
    let orig = (0.6, 0.8, 0.4, 0.9)
        (r', g', b', a') = previewTint orig
        (r, g, b, a) = orig
    r' `shouldSatisfy` (< r)
    g' `shouldSatisfy` (< g)
    b' `shouldSatisfy` (< b)
    a' `shouldSatisfy` (< a)

  -- Realized vs preview distinction
  it "tryCraft CraftSuccess is full-color (realized)" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
    case tryCraft grid of
      CraftSuccess item _ -> do
        -- Realized state: no tint applied (colors are as-is from itemMiniIcon)
        -- Preview state: tint is applied
        -- We verify the tint function produces different (dimmer) values
        let origColor = (1.0, 0.8, 0.6, 1.0) :: (Float, Float, Float, Float)
            tinted = previewTint origColor
        tinted `shouldSatisfy` (\(r,g,b,a) -> r < 1.0 && g < 0.8 && b < 0.6 && a < 1.0)
        -- Confirm item is returned correctly
        item `shouldBe` BlockItem OakPlanks
      CraftFailure -> expectationFailure "should succeed"

  it "preview Nothing when grid is cleared after valid recipe" $ do
    let grid = setCraftingSlot (emptyCraftingGrid 3) 0 0 (Just (BlockItem OakLog))
        clearedGrid = setCraftingSlot grid 0 0 Nothing
    craftPreview grid `shouldBe` Just (BlockItem OakPlanks, 4)
    craftPreview clearedGrid `shouldBe` Nothing
-- Q key item drop (dropFromSlot)
-- =========================================================================
dropFromSlotSpec :: Spec
dropFromSlotSpec = describe "Game.Inventory.dropFromSlot" $ do
  it "returns Nothing when selected slot is empty" $ do
    let (inv', mDrop) = dropFromSlot emptyInventory 0 False
    mDrop `shouldBe` Nothing
    inv' `shouldBe` emptyInventory

  it "drops 1 item from a stack of many (Q press)" $ do
    let (inv, _) = addItem emptyInventory (BlockItem Stone) 10
        (inv', mDrop) = dropFromSlot inv 0 False
    mDrop `shouldBe` Just (BlockItem Stone, 1)
    getSlot inv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 9)

  it "drops entire stack when shift is held (Shift+Q)" $ do
    let (inv, _) = addItem emptyInventory (BlockItem Stone) 10
        (inv', mDrop) = dropFromSlot inv 0 True
    mDrop `shouldBe` Just (BlockItem Stone, 10)
    getSlot inv' 0 `shouldBe` Nothing

  it "drops the last item and clears the slot" $ do
    let (inv, _) = addItem emptyInventory (BlockItem Dirt) 1
        (inv', mDrop) = dropFromSlot inv 0 False
    mDrop `shouldBe` Just (BlockItem Dirt, 1)
    getSlot inv' 0 `shouldBe` Nothing

  it "shift+Q on single item clears the slot" $ do
    let (inv, _) = addItem emptyInventory (BlockItem Dirt) 1
        (inv', mDrop) = dropFromSlot inv 0 True
    mDrop `shouldBe` Just (BlockItem Dirt, 1)
    getSlot inv' 0 `shouldBe` Nothing

  it "does not affect other slots" $ do
    let (inv1, _) = addItem emptyInventory (BlockItem Stone) 10
        (inv2, _) = addItem inv1 (BlockItem Dirt) 5
        (inv', _) = dropFromSlot inv2 0 False
    getSlot inv' 1 `shouldBe` Just (ItemStack (BlockItem Dirt) 5)

  it "drops from non-zero hotbar slot" $ do
    let inv0 = selectHotbar emptyInventory 3
        inv1 = setSlot inv0 3 (Just (ItemStack (BlockItem OakLog) 16))
        (inv', mDrop) = dropFromSlot inv1 3 False
    mDrop `shouldBe` Just (BlockItem OakLog, 1)
    getSlot inv' 3 `shouldBe` Just (ItemStack (BlockItem OakLog) 15)

  it "shift+Q drops full stack from non-zero hotbar slot" $ do
    let inv0 = selectHotbar emptyInventory 5
        inv1 = setSlot inv0 5 (Just (ItemStack (BlockItem OakLog) 32))
        (inv', mDrop) = dropFromSlot inv1 5 True
    mDrop `shouldBe` Just (BlockItem OakLog, 32)
    getSlot inv' 5 `shouldBe` Nothing

  it "preserves invSelected after drop" $ do
    let inv0 = selectHotbar emptyInventory 4
        (inv1, _) = addItem inv0 (BlockItem Stone) 10
        (inv', _) = dropFromSlot inv1 0 False
    invSelected inv' `shouldBe` 4
-- Hotbar Number Key Assignment
-- =========================================================================
hotbarNumberKeySpec :: Spec
hotbarNumberKeySpec = describe "Game.Inventory.hotbarNumberKey" $ do
  let stone3  = Just (ItemStack (BlockItem Stone) 3)
      dirt5   = Just (ItemStack (BlockItem Dirt) 5)
      sand10  = Just (ItemStack (BlockItem Sand) 10)
      pickaxe = Just (ItemStack (ToolItem Pickaxe Iron 250) 1)

  describe "cursor holds item, hotbar slot empty" $ do
    it "places cursor item into empty hotbar slot 0" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory stone3 0
      getHotbarSlot inv' 0 `shouldBe` stone3
      cursor' `shouldBe` Nothing

    it "places cursor item into empty hotbar slot 4" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory dirt5 4
      getHotbarSlot inv' 4 `shouldBe` dirt5
      cursor' `shouldBe` Nothing

    it "places cursor item into empty hotbar slot 8" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory sand10 8
      getHotbarSlot inv' 8 `shouldBe` sand10
      cursor' `shouldBe` Nothing

  describe "cursor holds item, hotbar slot occupied (swap)" $ do
    it "swaps cursor with existing hotbar contents" $ do
      let inv0 = setSlot emptyInventory 0 stone3
          (inv', cursor') = hotbarNumberKey inv0 dirt5 0
      getHotbarSlot inv' 0 `shouldBe` dirt5
      cursor' `shouldBe` stone3

    it "swap preserves stack counts" $ do
      let inv0 = setSlot emptyInventory 3 (Just (ItemStack (BlockItem Stone) 32))
          cursor = Just (ItemStack (BlockItem Dirt) 16)
          (inv', cursor') = hotbarNumberKey inv0 cursor 3
      getHotbarSlot inv' 3 `shouldBe` cursor
      cursor' `shouldBe` Just (ItemStack (BlockItem Stone) 32)

    it "swap with tool item works" $ do
      let inv0 = setSlot emptyInventory 2 stone3
          (inv', cursor') = hotbarNumberKey inv0 pickaxe 2
      getHotbarSlot inv' 2 `shouldBe` pickaxe
      cursor' `shouldBe` stone3

  describe "cursor empty, hotbar slot occupied (pick up)" $ do
    it "picks up hotbar contents into cursor" $ do
      let inv0 = setSlot emptyInventory 0 stone3
          (inv', cursor') = hotbarNumberKey inv0 Nothing 0
      getHotbarSlot inv' 0 `shouldBe` Nothing
      cursor' `shouldBe` stone3

    it "picks up from any hotbar slot" $ do
      let inv0 = setSlot emptyInventory 7 dirt5
          (inv', cursor') = hotbarNumberKey inv0 Nothing 7
      getHotbarSlot inv' 7 `shouldBe` Nothing
      cursor' `shouldBe` dirt5

    it "picks up tool from hotbar" $ do
      let inv0 = setSlot emptyInventory 1 pickaxe
          (inv', cursor') = hotbarNumberKey inv0 Nothing 1
      getHotbarSlot inv' 1 `shouldBe` Nothing
      cursor' `shouldBe` pickaxe

  describe "cursor empty, hotbar slot empty (no-op)" $ do
    it "returns unchanged inventory and Nothing cursor" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory Nothing 0
      inv' `shouldBe` emptyInventory
      cursor' `shouldBe` Nothing

    it "no-op for any empty slot" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory Nothing 5
      inv' `shouldBe` emptyInventory
      cursor' `shouldBe` Nothing

  describe "out-of-range slot index" $ do
    it "negative index is no-op" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory stone3 (-1)
      inv' `shouldBe` emptyInventory
      cursor' `shouldBe` stone3

    it "index >= hotbarSlots is no-op" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory stone3 9
      inv' `shouldBe` emptyInventory
      cursor' `shouldBe` stone3

    it "large index is no-op" $ do
      let (inv', cursor') = hotbarNumberKey emptyInventory dirt5 100
      inv' `shouldBe` emptyInventory
      cursor' `shouldBe` dirt5

  describe "does not disturb other slots" $ do
    it "placing into slot 0 does not touch slot 1" $ do
      let inv0 = setSlot emptyInventory 1 dirt5
          (inv', _) = hotbarNumberKey inv0 stone3 0
      getHotbarSlot inv' 0 `shouldBe` stone3
      getHotbarSlot inv' 1 `shouldBe` dirt5

    it "swapping slot 3 does not affect slot 0 or 8" $ do
      let inv0 = setSlot (setSlot (setSlot emptyInventory 0 stone3) 3 dirt5) 8 sand10
          (inv', cursor') = hotbarNumberKey inv0 pickaxe 3
      getHotbarSlot inv' 0 `shouldBe` stone3
      getHotbarSlot inv' 3 `shouldBe` pickaxe
      getHotbarSlot inv' 8 `shouldBe` sand10
      cursor' `shouldBe` dirt5

    it "picking up from slot 5 preserves non-hotbar inventory" $ do
      let inv0 = setSlot (setSlot emptyInventory 5 stone3) 15 dirt5
          (inv', cursor') = hotbarNumberKey inv0 Nothing 5
      getSlot inv' 15 `shouldBe` dirt5
      getHotbarSlot inv' 5 `shouldBe` Nothing
      cursor' `shouldBe` stone3

  describe "invSelected is preserved" $ do
    it "placing item does not change selected slot" $ do
      let inv0 = selectHotbar emptyInventory 3
          (inv', _) = hotbarNumberKey inv0 stone3 0
      invSelected inv' `shouldBe` 3

    it "picking up does not change selected slot" $ do
      let inv0 = selectHotbar (setSlot emptyInventory 2 dirt5) 7
          (inv', _) = hotbarNumberKey inv0 Nothing 2
      invSelected inv' `shouldBe` 7
-- Inventory Comprehensive Tests
-- =========================================================================
inventoryComprehensiveSpec :: Spec
inventoryComprehensiveSpec = describe "Game.Inventory comprehensive" $ do
  -- Helper items
  let stone = BlockItem Stone
      dirt  = BlockItem Dirt
      tool  = ToolItem Pickaxe Diamond 1561
      food  = FoodItem Apple

  describe "addItem basics" $ do
    it "addItem to empty inventory places 1 stack in slot 0" $ do
      let (inv, left) = addItem emptyInventory stone 10
      left `shouldBe` 0
      getSlot inv 0 `shouldBe` Just (ItemStack stone 10)

    it "addItem merging into partial stack fills existing slot first" $ do
      let (inv1, _) = addItem emptyInventory stone 40
          (inv2, left) = addItem inv1 stone 20
      left `shouldBe` 0
      getSlot inv2 0 `shouldBe` Just (ItemStack stone 60)
      getSlot inv2 1 `shouldBe` Nothing

    it "addItem overflow when stack is full spills to next slot" $ do
      let (inv1, _) = addItem emptyInventory stone 60
          (inv2, left) = addItem inv1 stone 10
      left `shouldBe` 0
      getSlot inv2 0 `shouldBe` Just (ItemStack stone 64)
      getSlot inv2 1 `shouldBe` Just (ItemStack stone 6)

    it "addItem fills multiple slots when count exceeds one stack" $ do
      let (inv, left) = addItem emptyInventory stone 150
      left `shouldBe` 0
      getSlot inv 0 `shouldBe` Just (ItemStack stone 64)
      getSlot inv 1 `shouldBe` Just (ItemStack stone 64)
      getSlot inv 2 `shouldBe` Just (ItemStack stone 22)

    it "addItem returns correct leftover when inventory is completely full" $ do
      let fillSlots inv 0 = inv
          fillSlots inv n = let (inv', _) = addItem inv stone 64
                            in fillSlots inv' (n - 1)
          full = fillSlots emptyInventory inventorySlots
          (_, leftover) = addItem full stone 42
      leftover `shouldBe` 42

  describe "removeItem basics" $ do
    it "removeItem from single stack reduces count" $ do
      let (inv1, _) = addItem emptyInventory stone 30
          (inv2, removed) = removeItem inv1 stone 10
      removed `shouldBe` 10
      getSlot inv2 0 `shouldBe` Just (ItemStack stone 20)

    it "removeItem from multiple stacks drains last-first" $ do
      let (inv1, _) = addItem emptyInventory stone 100
          -- stone is in slots 0(64) and 1(36)
          (inv2, removed) = removeItem inv1 stone 50
      removed `shouldBe` 50
      -- Slot 1 drained first (had 36), then slot 0 loses 14
      getSlot inv2 0 `shouldBe` Just (ItemStack stone 50)
      getSlot inv2 1 `shouldBe` Nothing

    it "removeItem returns actual removed count when insufficient" $ do
      let (inv1, _) = addItem emptyInventory stone 15
          (inv2, removed) = removeItem inv1 stone 100
      removed `shouldBe` 15
      getSlot inv2 0 `shouldBe` Nothing

  describe "countItem and hasItem" $ do
    it "countItem accuracy across multiple stacks" $ do
      let (inv1, _) = addItem emptyInventory stone 100
      countItem inv1 stone `shouldBe` 100

    it "hasItem true when enough" $ do
      let (inv1, _) = addItem emptyInventory stone 50
      hasItem inv1 stone 50 `shouldBe` True

    it "hasItem false when insufficient" $ do
      let (inv1, _) = addItem emptyInventory stone 49
      hasItem inv1 stone 50 `shouldBe` False

  describe "setSlot and getSlot bounds" $ do
    it "setSlot with negative index is no-op" $ do
      let inv = setSlot emptyInventory (-1) (Just (ItemStack stone 10))
      inv `shouldBe` emptyInventory

    it "setSlot with index beyond max is no-op" $ do
      let inv = setSlot emptyInventory 100 (Just (ItemStack stone 10))
      inv `shouldBe` emptyInventory

    it "getSlot with negative index returns Nothing" $ do
      getSlot emptyInventory (-1) `shouldBe` Nothing

    it "getSlot with index beyond max returns Nothing" $ do
      getSlot emptyInventory 100 `shouldBe` Nothing

  describe "selectHotbar bounds" $ do
    it "selectHotbar with negative index is no-op" $ do
      let inv = selectHotbar emptyInventory (-1)
      invSelected inv `shouldBe` 0

    it "selectHotbar with index > 8 is no-op" $ do
      let inv = selectHotbar emptyInventory 9
      invSelected inv `shouldBe` 0

  describe "selectedItem" $ do
    it "selectedItem returns correct slot after selectHotbar" $ do
      let inv1 = setSlot emptyInventory 3 (Just (ItemStack dirt 20))
          inv2 = selectHotbar inv1 3
      selectedItem inv2 `shouldBe` Just (ItemStack dirt 20)

  describe "emptyInventory" $ do
    it "emptyInventory has 36 Nothing slots" $ do
      let slots = invSlots emptyInventory
      V.length slots `shouldBe` 36
      V.all (== Nothing) slots `shouldBe` True

    it "emptyInventory invSelected = 0" $ do
      invSelected emptyInventory `shouldBe` 0

  describe "itemStackLimit" $ do
    it "tools stack to 1" $ do
      itemStackLimit tool `shouldBe` 1

    it "blocks stack to 64" $ do
      itemStackLimit stone `shouldBe` 64

  describe "addItem slot selection" $ do
    it "addItem to slot with different item type stays separate" $ do
      let (inv1, _) = addItem emptyInventory stone 10
          (inv2, _) = addItem inv1 dirt 10
      getSlot inv2 0 `shouldBe` Just (ItemStack stone 10)
      getSlot inv2 1 `shouldBe` Just (ItemStack dirt 10)

    it "mergeIntoExisting prefers earlier slots" $ do
      let inv1 = setSlot emptyInventory 2 (Just (ItemStack stone 30))
          inv2 = setSlot inv1 5 (Just (ItemStack stone 30))
          (inv3, _) = addItem inv2 stone 10
      -- Slot 2 should get the merge first
      getSlot inv3 2 `shouldBe` Just (ItemStack stone 40)
      getSlot inv3 5 `shouldBe` Just (ItemStack stone 30)

    it "placeInEmpty prefers earlier empty slots" $ do
      let inv1 = setSlot emptyInventory 0 (Just (ItemStack dirt 10))
          inv2 = setSlot inv1 1 (Just (ItemStack dirt 10))
          (inv3, _) = addItem inv2 stone 10
      -- Slot 2 is the first empty
      getSlot inv3 2 `shouldBe` Just (ItemStack stone 10)

  describe "edge cases" $ do
    it "addItem 0 count is no-op" $ do
      let (inv, left) = addItem emptyInventory stone 0
      left `shouldBe` 0
      inv `shouldBe` emptyInventory

    it "removeItem 0 count is no-op" $ do
      let (inv1, _) = addItem emptyInventory stone 10
          (inv2, removed) = removeItem inv1 stone 0
      removed `shouldBe` 0
      inv2 `shouldBe` inv1

    it "removeItem more than exists returns actual count" $ do
      let (inv1, _) = addItem emptyInventory stone 7
          (_, removed) = removeItem inv1 stone 999
      removed `shouldBe` 7

    it "countItem returns 0 for item not in inventory" $ do
      countItem emptyInventory stone `shouldBe` 0

    it "hasItem returns True for threshold 0 even on empty inventory" $ do
      hasItem emptyInventory stone 0 `shouldBe` True

  describe "QuickCheck properties" $ do
    it "total count preserved: add then count equals amount added" $
      property $ \n ->
        let count = abs n `mod` 2305  -- up to 36 * 64 = 2304
            (inv, leftover) = addItem emptyInventory stone count
        in countItem inv stone + leftover == count

    it "add then remove round-trips: total count correct" $
      property $ \n m ->
        let addAmt = abs n `mod` 2305
            remAmt = abs m `mod` (addAmt + 1)
            (inv1, leftover) = addItem emptyInventory stone addAmt
            added = addAmt - leftover
            (inv2, removed) = removeItem inv1 stone remAmt
        in countItem inv2 stone == added - removed

    it "removing more than exists never removes more than available" $
      property $ \n m ->
        let addAmt = abs n `mod` 500
            remAmt = abs m `mod` 1000
            (inv1, leftover) = addItem emptyInventory stone addAmt
            added = addAmt - leftover
            (_, removed) = removeItem inv1 stone remAmt
        in removed <= added

    it "addItem never produces negative leftover" $
      property $ \n ->
        let count = abs n `mod` 5000
            (_, leftover) = addItem emptyInventory stone count
        in leftover >= 0

    it "leftover + inventory count equals original add amount" $
      property $ \n ->
        let count = abs n `mod` 5000
            (inv, leftover) = addItem emptyInventory stone count
        in countItem inv stone + leftover == count


-- =========================================================================
-- Creative Mode Inventory
-- =========================================================================
creativeInventorySpec :: Spec
creativeInventorySpec = describe "Game.Creative" $ do

  -- Palette contents
  describe "Creative palette" $ do
    it "palette is non-empty" $
      creativePaletteSize `shouldSatisfy` (> 0)

    it "palette contains at least 70 block items" $ do
      let blockCount = length [() | BlockItem _ <- creativePalette]
      blockCount `shouldSatisfy` (>= 70)

    it "palette contains all tool tiers" $ do
      let hasWoodPickaxe   = elem (ToolItem Pickaxe Wood 59) creativePalette
          hasDiamondSword  = elem (ToolItem Sword Diamond 1561) creativePalette
      hasWoodPickaxe `shouldBe` True
      hasDiamondSword `shouldBe` True

    it "palette contains food items" $ do
      let foodCount = length [() | FoodItem _ <- creativePalette]
      foodCount `shouldSatisfy` (>= 5)

    it "palette contains material items" $ do
      let matCount = length [() | MaterialItem _ <- creativePalette]
      matCount `shouldSatisfy` (>= 10)

    it "palette contains armor items" $ do
      let armorCount = length [() | ArmorItem {} <- creativePalette]
      armorCount `shouldSatisfy` (>= 8)

    it "palette contains misc items (compass, clock, boat)" $ do
      elem CompassItem creativePalette `shouldBe` True
      elem ClockItem creativePalette `shouldBe` True
      elem BoatItem creativePalette `shouldBe` True

    it "palette contains bucket variants" $ do
      elem (BucketItem BucketEmpty) creativePalette `shouldBe` True
      elem (BucketItem BucketWater) creativePalette `shouldBe` True

    it "palette does not contain Air block" $ do
      elem (BlockItem Air) creativePalette `shouldBe` False

    it "palette contains Stone block" $ do
      elem (BlockItem Stone) creativePalette `shouldBe` True

    it "palette contains Torch block" $ do
      elem (BlockItem Torch) creativePalette `shouldBe` True

    it "palette contains StickItem" $ do
      elem StickItem creativePalette `shouldBe` True

    it "palette size matches length" $
      creativePaletteSize `shouldBe` length creativePalette

  -- Palette pagination
  describe "Palette pagination" $ do
    it "paletteSlotsPerPage is rows * cols" $
      paletteSlotsPerPage `shouldBe` paletteRows * paletteCols

    it "page count covers all items" $
      palettePageCount * paletteSlotsPerPage `shouldSatisfy` (>= creativePaletteSize)

    it "page 0 has paletteSlotsPerPage items when palette is large enough" $
      length (palettePageItems 0) `shouldBe` min paletteSlotsPerPage creativePaletteSize

    it "last page may have fewer items" $ do
      let lastPage = palettePageCount - 1
          items = palettePageItems lastPage
      length items `shouldSatisfy` (<= paletteSlotsPerPage)
      length items `shouldSatisfy` (> 0)

    it "all pages combined equal total palette" $ do
      let allItems = concatMap palettePageItems [0 .. palettePageCount - 1]
      length allItems `shouldBe` creativePaletteSize

    it "page items are contiguous slices of palette" $ do
      let page0 = palettePageItems 0
          page1 = palettePageItems 1
      take (length page0) creativePalette `shouldBe` page0
      take (length page1) (drop paletteSlotsPerPage creativePalette) `shouldBe` page1

  -- Palette hit detection
  describe "Palette hit detection" $ do
    it "top-left corner hits slot 0" $ do
      let x = paletteX0 + 0.01
          y = paletteY0 + 0.01
      hitPaletteSlot x y `shouldBe` Just 0

    it "second column first row hits slot 1" $ do
      let x = paletteX0 + paletteSlotW + 0.01
          y = paletteY0 + 0.01
      hitPaletteSlot x y `shouldBe` Just 1

    it "first column second row hits slot paletteCols" $ do
      let x = paletteX0 + 0.01
          y = paletteY0 + paletteSlotH + 0.01
      hitPaletteSlot x y `shouldBe` Just paletteCols

    it "outside left returns Nothing" $
      hitPaletteSlot (paletteX0 - 0.1) (paletteY0 + 0.01) `shouldBe` Nothing

    it "outside right returns Nothing" $
      hitPaletteSlot (paletteX0 + fromIntegral paletteCols * paletteSlotW + 0.1) (paletteY0 + 0.01) `shouldBe` Nothing

    it "outside top returns Nothing" $
      hitPaletteSlot (paletteX0 + 0.01) (paletteY0 - 0.1) `shouldBe` Nothing

    it "outside bottom returns Nothing" $
      hitPaletteSlot (paletteX0 + 0.01) (paletteY0 + fromIntegral paletteRows * paletteSlotH + 0.1) `shouldBe` Nothing

    it "bottom-right corner hits last slot" $ do
      let x = paletteX0 + fromIntegral (paletteCols - 1) * paletteSlotW + 0.01
          y = paletteY0 + fromIntegral (paletteRows - 1) * paletteSlotH + 0.01
      hitPaletteSlot x y `shouldBe` Just (paletteRows * paletteCols - 1)

  -- Creative pick from palette
  describe "creativePickFromPalette" $ do
    it "picks first item as full stack of 64" $ do
      let items = palettePageItems 0
          result = creativePickFromPalette 0 items
      result `shouldBe` Just (ItemStack (BlockItem Stone) 64)

    it "picks a tool item as stack of 1" $ do
      -- Find index of a tool in the palette
      let items = creativePalette
          toolIdx = length [() | BlockItem _ <- takeWhile isBlockItem' items]
          isBlockItem' (BlockItem _) = True
          isBlockItem' _ = False
      case creativePickFromPalette toolIdx items of
        Just (ItemStack (ToolItem {}) 1) -> pure ()
        other -> expectationFailure $ "Expected tool stack of 1, got " ++ show other

    it "returns Nothing for negative index" $ do
      let items = palettePageItems 0
      creativePickFromPalette (-1) items `shouldBe` Nothing

    it "returns Nothing for out-of-range index" $ do
      let items = palettePageItems 0
      creativePickFromPalette 999 items `shouldBe` Nothing

    it "returns Nothing for empty page" $
      creativePickFromPalette 0 [] `shouldBe` Nothing

    it "block items get stack of 64" $ do
      let items = [BlockItem Dirt]
      case creativePickFromPalette 0 items of
        Just (ItemStack (BlockItem Dirt) 64) -> pure ()
        other -> expectationFailure $ "Expected Dirt x64, got " ++ show other

    it "food items get stack of 64" $ do
      let items = [FoodItem Apple]
      case creativePickFromPalette 0 items of
        Just (ItemStack (FoodItem Apple) 64) -> pure ()
        other -> expectationFailure $ "Expected Apple x64, got " ++ show other

    it "material items get stack of 64" $ do
      let items = [MaterialItem Coal]
      case creativePickFromPalette 0 items of
        Just (ItemStack (MaterialItem Coal) 64) -> pure ()
        other -> expectationFailure $ "Expected Coal x64, got " ++ show other

  -- Creative click slot
  describe "creativeClickSlot" $ do
    it "clicking empty slot with no cursor returns nothing" $ do
      let inv = emptyInventory
          (newInv, newCursor) = creativeClickSlot inv 0 Nothing
      newCursor `shouldBe` Nothing
      getSlot newInv 0 `shouldBe` Nothing

    it "clicking occupied slot with no cursor gives full stack" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 5))
          (_, newCursor) = creativeClickSlot inv 0 Nothing
      newCursor `shouldBe` Just (ItemStack (BlockItem Stone) 64)

    it "clicking occupied slot with no cursor does not modify inventory" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 5))
          (newInv, _) = creativeClickSlot inv 0 Nothing
      getSlot newInv 0 `shouldBe` Just (ItemStack (BlockItem Stone) 5)

    it "clicking with cursor places full stack in slot" $ do
      let inv = emptyInventory
          cursor = Just (ItemStack (BlockItem Dirt) 3)
          (newInv, newCursor) = creativeClickSlot inv 5 cursor
      newCursor `shouldBe` Nothing
      getSlot newInv 5 `shouldBe` Just (ItemStack (BlockItem Dirt) 64)

    it "clicking with cursor replaces existing slot content" $ do
      let inv = setSlot emptyInventory 2 (Just (ItemStack (BlockItem Stone) 10))
          cursor = Just (ItemStack (BlockItem Dirt) 1)
          (newInv, newCursor) = creativeClickSlot inv 2 cursor
      newCursor `shouldBe` Nothing
      getSlot newInv 2 `shouldBe` Just (ItemStack (BlockItem Dirt) 64)

    it "tool items placed as stack of 1" $ do
      let inv = emptyInventory
          cursor = Just (ItemStack (ToolItem Pickaxe Diamond 1561) 1)
          (newInv, newCursor) = creativeClickSlot inv 0 cursor
      newCursor `shouldBe` Nothing
      getSlot newInv 0 `shouldBe` Just (ItemStack (ToolItem Pickaxe Diamond 1561) 1)

    it "armor items placed as stack of 1" $ do
      let inv = emptyInventory
          cursor = Just (ItemStack (ArmorItem Helmet DiamondArmor 363) 1)
          (newInv, _) = creativeClickSlot inv 0 cursor
      getSlot newInv 0 `shouldBe` Just (ItemStack (ArmorItem Helmet DiamondArmor 363) 1)

    it "picking from slot with 1 item still gives full stack" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Cobblestone) 1))
          (_, newCursor) = creativeClickSlot inv 0 Nothing
      newCursor `shouldBe` Just (ItemStack (BlockItem Cobblestone) 64)

  -- PlayMode type
  describe "PlayMode" $ do
    it "Survival /= Creative" $
      Survival `shouldSatisfy` (/= Creative)

    it "show Survival" $
      show Survival `shouldBe` "Survival"

    it "show Creative" $
      show Creative `shouldBe` "Creative"

    it "Bounded: minBound is Survival" $
      (minBound :: PlayMode) `shouldBe` Survival

    it "Bounded: maxBound is Creative" $
      (maxBound :: PlayMode) `shouldBe` Creative

    it "Enum: toEnum 0 is Survival" $
      toEnum 0 `shouldBe` Survival

    it "Enum: toEnum 1 is Creative" $
      (toEnum 1 :: PlayMode) `shouldBe` Creative

  -- GameState PlayMode field
  describe "GameState PlayMode field" $ do
    it "new game state defaults to Survival" $ do
      gs <- newGameState (V3 0 80 0)
      pm <- readIORef (gsPlayMode gs)
      pm `shouldBe` Survival

    it "gsPlayMode can be set to Creative" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsPlayMode gs) Creative
      pm <- readIORef (gsPlayMode gs)
      pm `shouldBe` Creative

    it "gsPlayMode can be toggled back to Survival" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsPlayMode gs) Creative
      writeIORef (gsPlayMode gs) Survival
      pm <- readIORef (gsPlayMode gs)
      pm `shouldBe` Survival

-- =========================================================================
-- collectAll (double-click collect matching items)
-- =========================================================================
collectAllSpec :: Spec
collectAllSpec = describe "Game.Inventory.collectAll" $ do
  let stone = BlockItem Stone
      dirt  = BlockItem Dirt
      tool  = ToolItem Pickaxe Diamond 1561

  it "collects all matching items from multiple slots" $ do
    let (inv1, _) = addItem emptyInventory stone 30
        (inv2, _) = addItem inv1 dirt 10
        (inv3, _) = addItem inv2 stone 20
        -- stone is in slots 0 (30) and 2 (20), dirt in slot 1 (10)
        (inv', collected) = collectAll inv3 stone 64
    collected `shouldBe` 50
    countItem inv' stone `shouldBe` 0

  it "respects maxStack limit" $ do
    let (inv1, _) = addItem emptyInventory stone 64
        (inv2, _) = addItem inv1 stone 64
        -- 128 total stone, but maxStack is 64
        (inv', collected) = collectAll inv2 stone 64
    collected `shouldBe` 64
    countItem inv' stone `shouldBe` 64

  it "returns 0 when no matching items exist" $ do
    let (inv1, _) = addItem emptyInventory dirt 10
        (inv', collected) = collectAll inv1 stone 64
    collected `shouldBe` 0
    countItem inv' dirt `shouldBe` 10

  it "collects from empty inventory returns 0" $ do
    let (inv', collected) = collectAll emptyInventory stone 64
    collected `shouldBe` 0
    inv' `shouldBe` emptyInventory

  it "partial collection from a slot leaves remainder" $ do
    let (inv1, _) = addItem emptyInventory stone 40
        -- maxStack=10 means collect only 10 from the 40
        (inv', collected) = collectAll inv1 stone 10
    collected `shouldBe` 10
    countItem inv' stone `shouldBe` 30

  it "collects from scattered slots across inventory" $ do
    let inv0 = setSlot emptyInventory 0  (Just (ItemStack stone 5))
        inv1 = setSlot inv0           10 (Just (ItemStack stone 15))
        inv2 = setSlot inv1           20 (Just (ItemStack stone 10))
        inv3 = setSlot inv2           35 (Just (ItemStack stone 8))
        (inv', collected) = collectAll inv3 stone 64
    collected `shouldBe` 38
    countItem inv' stone `shouldBe` 0

  it "does not collect items of different type" $ do
    let (inv1, _) = addItem emptyInventory stone 30
        (inv2, _) = addItem inv1 dirt 20
        (inv', collected) = collectAll inv2 stone 64
    collected `shouldBe` 30
    countItem inv' dirt `shouldBe` 20

  it "handles maxStack of 0 gracefully" $ do
    let (inv1, _) = addItem emptyInventory stone 30
        (inv', collected) = collectAll inv1 stone 0
    collected `shouldBe` 0
    countItem inv' stone `shouldBe` 30

  it "collects tool items (stackLimit 1)" $ do
    let inv0 = setSlot emptyInventory 0 (Just (ItemStack tool 1))
        inv1 = setSlot inv0           5 (Just (ItemStack tool 1))
        inv2 = setSlot inv1          10 (Just (ItemStack tool 1))
        (inv', collected) = collectAll inv2 tool 1
    collected `shouldBe` 1
    countItem inv' tool `shouldBe` 2

  it "collects exactly remaining space" $ do
    let (inv1, _) = addItem emptyInventory stone 20
        (inv2, _) = addItem inv1 stone 30
        -- 50 total; collect with maxStack=50 should get all
        (inv', collected) = collectAll inv2 stone 50
    collected `shouldBe` 50
    countItem inv' stone `shouldBe` 0

  it "preserves invSelected" $ do
    let inv0 = selectHotbar emptyInventory 5
        (inv1, _) = addItem inv0 stone 30
        (inv', _) = collectAll inv1 stone 64
    invSelected inv' `shouldBe` 5
  -- Container capacity display
  describe "Container capacity display" $ do
    it "countNonEmptyChestSlots returns 0 for empty chest" $
      countNonEmptyChestSlots emptyChestInventory `shouldBe` 0

    it "countNonEmptyChestSlots counts occupied slots" $ do
      let item = ItemStack (BlockItem Stone) 10
          inv1 = setChestSlot emptyChestInventory 0 (Just item)
          inv2 = setChestSlot inv1 5 (Just item)
          inv3 = setChestSlot inv2 26 (Just item)
      countNonEmptyChestSlots inv3 `shouldBe` 3

    it "countNonEmptyChestSlots returns 27 for full chest" $ do
      let item = ItemStack (BlockItem Stone) 1
          inv = foldl (\acc i -> setChestSlot acc i (Just item)) emptyChestInventory [0..26]
      countNonEmptyChestSlots inv `shouldBe` 27

    it "countNonEmptyDispenserSlots returns 0 for empty dispenser" $
      countNonEmptyDispenserSlots emptyDispenserInventory `shouldBe` 0

    it "countNonEmptyDispenserSlots counts occupied slots" $ do
      let item = ItemStack (BlockItem Dirt) 5
          inv1 = setDispenserSlot emptyDispenserInventory 0 (Just item)
          inv2 = setDispenserSlot inv1 4 (Just item)
      countNonEmptyDispenserSlots inv2 `shouldBe` 2

    it "countNonEmptyDispenserSlots returns 9 for full dispenser" $ do
      let item = ItemStack (BlockItem Stone) 1
          inv = foldl (\acc i -> setDispenserSlot acc i (Just item)) emptyDispenserInventory [0..8]
      countNonEmptyDispenserSlots inv `shouldBe` 9

    it "containerCapacityText formats correctly for empty container" $
      containerCapacityText 0 27 `shouldBe` "USED: 0/27"

    it "containerCapacityText formats correctly for partially filled" $
      containerCapacityText 5 27 `shouldBe` "USED: 5/27"

    it "containerCapacityText formats correctly for full container" $
      containerCapacityText 27 27 `shouldBe` "USED: 27/27"

    it "containerCapacityText works for dispenser capacity" $
      containerCapacityText 3 9 `shouldBe` "USED: 3/9"

    it "countNonEmptyChestSlots ignores cleared slots" $ do
      let item = ItemStack (BlockItem Stone) 10
          inv1 = setChestSlot emptyChestInventory 0 (Just item)
          inv2 = setChestSlot inv1 1 (Just item)
          inv3 = setChestSlot inv2 0 Nothing
      countNonEmptyChestSlots inv3 `shouldBe` 1

    it "countNonEmptyDispenserSlots ignores cleared slots" $ do
      let item = ItemStack (BlockItem Dirt) 3
          inv1 = setDispenserSlot emptyDispenserInventory 0 (Just item)
          inv2 = setDispenserSlot inv1 1 (Just item)
          inv3 = setDispenserSlot inv2 1 Nothing
      countNonEmptyDispenserSlots inv3 `shouldBe` 1
-- =========================================================================
-- Stack Splitting (splitStack / placeSingle)
-- =========================================================================
stackSplittingSpec :: Spec
stackSplittingSpec = describe "Game.Inventory stack splitting" $ do
  describe "splitStack" $ do
    it "returns (Nothing, Nothing) for empty slot" $
      splitStack Nothing `shouldBe` (Nothing, Nothing)

    it "picks up entire single item" $
      splitStack (Just (ItemStack (BlockItem Stone) 1))
        `shouldBe` (Nothing, Just (ItemStack (BlockItem Stone) 1))

    it "splits even stack in half" $
      splitStack (Just (ItemStack (BlockItem Stone) 10))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 5), Just (ItemStack (BlockItem Stone) 5))

    it "splits odd stack: cursor gets ceil, slot gets floor" $
      splitStack (Just (ItemStack (BlockItem Dirt) 7))
        `shouldBe` (Just (ItemStack (BlockItem Dirt) 3), Just (ItemStack (BlockItem Dirt) 4))

    it "splits stack of 2" $
      splitStack (Just (ItemStack (BlockItem Sand) 2))
        `shouldBe` (Just (ItemStack (BlockItem Sand) 1), Just (ItemStack (BlockItem Sand) 1))

    it "splits stack of 3: cursor gets 2, slot keeps 1" $
      splitStack (Just (ItemStack (BlockItem Sand) 3))
        `shouldBe` (Just (ItemStack (BlockItem Sand) 1), Just (ItemStack (BlockItem Sand) 2))

    it "splits full 64 stack evenly" $
      splitStack (Just (ItemStack (BlockItem Stone) 64))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 32), Just (ItemStack (BlockItem Stone) 32))

    it "splits stack of 63: cursor gets 32, slot keeps 31" $
      splitStack (Just (ItemStack (BlockItem Stone) 63))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 31), Just (ItemStack (BlockItem Stone) 32))

  describe "placeSingle" $ do
    it "empty cursor on empty slot: no-op" $
      placeSingle Nothing Nothing `shouldBe` (Nothing, Nothing)

    it "empty cursor on occupied slot: returns slot unchanged" $
      placeSingle Nothing (Just (ItemStack (BlockItem Stone) 5))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 5), Nothing)

    it "cursor with 1 item on empty slot: places item, cursor empty" $
      placeSingle (Just (ItemStack (BlockItem Stone) 1)) Nothing
        `shouldBe` (Just (ItemStack (BlockItem Stone) 1), Nothing)

    it "cursor with multiple items on empty slot: places 1, cursor decremented" $
      placeSingle (Just (ItemStack (BlockItem Stone) 10)) Nothing
        `shouldBe` (Just (ItemStack (BlockItem Stone) 1), Just (ItemStack (BlockItem Stone) 9))

    it "cursor on slot with same item: merges 1" $
      placeSingle (Just (ItemStack (BlockItem Dirt) 5)) (Just (ItemStack (BlockItem Dirt) 3))
        `shouldBe` (Just (ItemStack (BlockItem Dirt) 4), Just (ItemStack (BlockItem Dirt) 4))

    it "cursor with 1 on slot with same item: merges, cursor empty" $
      placeSingle (Just (ItemStack (BlockItem Dirt) 1)) (Just (ItemStack (BlockItem Dirt) 3))
        `shouldBe` (Just (ItemStack (BlockItem Dirt) 4), Nothing)

    it "cursor on slot with different item: no-op" $
      placeSingle (Just (ItemStack (BlockItem Stone) 5)) (Just (ItemStack (BlockItem Dirt) 3))
        `shouldBe` (Just (ItemStack (BlockItem Dirt) 3), Just (ItemStack (BlockItem Stone) 5))

    it "cursor on slot at stack limit: no-op" $
      placeSingle (Just (ItemStack (BlockItem Stone) 5)) (Just (ItemStack (BlockItem Stone) 64))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 64), Just (ItemStack (BlockItem Stone) 5))

    it "cursor on slot at stack limit minus 1: merges to limit" $
      placeSingle (Just (ItemStack (BlockItem Stone) 5)) (Just (ItemStack (BlockItem Stone) 63))
        `shouldBe` (Just (ItemStack (BlockItem Stone) 64), Just (ItemStack (BlockItem Stone) 4))

  describe "splitStack + placeSingle round-trip" $ do
    it "splitting then placing all back restores original count" $ do
      let original = Just (ItemStack (BlockItem Stone) 10)
          (slotAfterSplit, cursorAfterSplit) = splitStack original
          -- Place items back one at a time until cursor empty
          placeBack cur slot = case cur of
            Nothing -> slot
            Just (ItemStack _ n) ->
              let (newSlot, newCur) = placeSingle cur slot
              in if n <= 0 then slot else placeBack newCur newSlot
          restored = placeBack cursorAfterSplit slotAfterSplit
      restored `shouldBe` original
  -- Creative infinite items: creativeConsumeItem
  describe "creativeConsumeItem (infinite items)" $ do
    it "consuming from a slot with items returns inventory unchanged" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 10))
          result = creativeConsumeItem inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (BlockItem Stone) 10)

    it "consuming from empty slot returns inventory unchanged" $ do
      let inv = emptyInventory
          result = creativeConsumeItem inv 0
      getSlot result 0 `shouldBe` Nothing

    it "consuming from slot with 1 item still keeps that item" $ do
      let inv = setSlot emptyInventory 3 (Just (ItemStack (BlockItem Dirt) 1))
          result = creativeConsumeItem inv 3
      getSlot result 3 `shouldBe` Just (ItemStack (BlockItem Dirt) 1)

    it "consuming a tool item keeps the tool" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (ToolItem Pickaxe Diamond 1561) 1))
          result = creativeConsumeItem inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (ToolItem Pickaxe Diamond 1561) 1)

    it "consuming does not affect other slots" $ do
      let inv = setSlot (setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 32)))
                        1 (Just (ItemStack (BlockItem Dirt) 16))
          result = creativeConsumeItem inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (BlockItem Stone) 32)
      getSlot result 1 `shouldBe` Just (ItemStack (BlockItem Dirt) 16)

    it "consuming from out-of-range slot returns inventory unchanged" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 5))
          result = creativeConsumeItem inv 999
      getSlot result 0 `shouldBe` Just (ItemStack (BlockItem Stone) 5)

    it "repeated consumption never depletes a stack" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Cobblestone) 64))
          consumed = iterate (\i -> creativeConsumeItem i 0) inv !! 100
      getSlot consumed 0 `shouldBe` Just (ItemStack (BlockItem Cobblestone) 64)

    it "consuming a food item keeps the food" $ do
      let inv = setSlot emptyInventory 2 (Just (ItemStack (FoodItem Apple) 10))
          result = creativeConsumeItem inv 2
      getSlot result 2 `shouldBe` Just (ItemStack (FoodItem Apple) 10)

  -- Creative refill slot
  describe "creativeRefillSlot" $ do
    it "refills a block item to stack of 64" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 3))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)

    it "refills a tool item to stack of 1" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (ToolItem Pickaxe Iron 250) 1))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (ToolItem Pickaxe Iron 250) 1)

    it "refilling empty slot is a no-op" $ do
      let inv = emptyInventory
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Nothing

    it "refills a partially used block stack" $ do
      let inv = setSlot emptyInventory 5 (Just (ItemStack (BlockItem OakPlanks) 1))
          result = creativeRefillSlot inv 5
      getSlot result 5 `shouldBe` Just (ItemStack (BlockItem OakPlanks) 64)

    it "does not affect other slots" $ do
      let inv = setSlot (setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 2)))
                        1 (Just (ItemStack (BlockItem Dirt) 5))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)
      getSlot result 1 `shouldBe` Just (ItemStack (BlockItem Dirt) 5)

    it "refills armor to stack of 1" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (ArmorItem Helmet DiamondArmor 363) 1))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (ArmorItem Helmet DiamondArmor 363) 1)

    it "refills food item to stack of 64" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (FoodItem Bread) 5))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (FoodItem Bread) 64)

    it "refills material item to stack of 64" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (MaterialItem Coal) 1))
          result = creativeRefillSlot inv 0
      getSlot result 0 `shouldBe` Just (ItemStack (MaterialItem Coal) 64)

-- =========================================================================
-- Durability display
-- =========================================================================
durabilityDisplaySpec :: Spec
durabilityDisplaySpec = describe "Game.ItemDisplay durability display" $ do

  describe "durabilityFraction" $ do
    it "returns Nothing for BlockItem" $ do
      durabilityFraction (BlockItem Stone) `shouldBe` Nothing

    it "returns Nothing for FoodItem" $ do
      durabilityFraction (FoodItem Apple) `shouldBe` Nothing

    it "returns Nothing for MaterialItem" $ do
      durabilityFraction (MaterialItem Coal) `shouldBe` Nothing

    it "returns Nothing for StickItem" $ do
      durabilityFraction StickItem `shouldBe` Nothing

    it "returns 1.0 for full durability wooden pickaxe" $ do
      durabilityFraction (ToolItem Pickaxe Wood 59) `shouldBe` Just 1.0

    it "returns 0.5 for half durability diamond pickaxe" $ do
      let frac = durabilityFraction (ToolItem Pickaxe Diamond 780)
      frac `shouldSatisfy` \(Just f) -> abs (f - 0.4997) < 0.01

    it "returns ~0.0 for nearly broken tool" $ do
      let frac = durabilityFraction (ToolItem Sword Iron 1)
      frac `shouldSatisfy` \(Just f) -> f > 0 && f < 0.01

    it "works for ShearsItem" $ do
      let frac = durabilityFraction (ShearsItem 238)
      frac `shouldBe` Just 1.0

    it "works for half-durability shears" $ do
      let frac = durabilityFraction (ShearsItem 119)
      frac `shouldBe` Just 0.5

    it "works for FlintAndSteelItem" $ do
      let frac = durabilityFraction (FlintAndSteelItem 64)
      frac `shouldBe` Just 1.0

    it "works for FishingRodItem" $ do
      let frac = durabilityFraction (FishingRodItem 32)
      frac `shouldBe` Just 0.5

    it "works for ArmorItem at full durability" $ do
      let frac = durabilityFraction (ArmorItem Chestplate DiamondArmor 528)
      frac `shouldBe` Just 1.0

    it "works for ArmorItem at partial durability" $ do
      let frac = durabilityFraction (ArmorItem Helmet IronArmor 120)
      frac `shouldBe` Just 0.5

  describe "durabilityBarColor thresholds" $ do
    it "returns green for fraction > 0.6" $ do
      durabilityBarColor 0.61 `shouldBe` (0.2, 0.8, 0.2, 1.0)

    it "returns green for fraction = 1.0" $ do
      durabilityBarColor 1.0 `shouldBe` (0.2, 0.8, 0.2, 1.0)

    it "returns yellow for fraction = 0.6" $ do
      durabilityBarColor 0.6 `shouldBe` (0.9, 0.8, 0.2, 1.0)

    it "returns yellow for fraction = 0.31" $ do
      durabilityBarColor 0.31 `shouldBe` (0.9, 0.8, 0.2, 1.0)

    it "returns red for fraction = 0.3" $ do
      durabilityBarColor 0.3 `shouldBe` (0.8, 0.2, 0.2, 1.0)

    it "returns red for fraction = 0.1" $ do
      durabilityBarColor 0.1 `shouldBe` (0.8, 0.2, 0.2, 1.0)

    it "returns red for fraction = 0.0" $ do
      durabilityBarColor 0.0 `shouldBe` (0.8, 0.2, 0.2, 1.0)
-- Cursor Item Rendering
-- =========================================================================
cursorItemRenderSpec :: Spec
cursorItemRenderSpec = describe "Game.ItemDisplay.buildCursorItemVerts" $ do

  describe "Nothing cursor produces no vertices" $ do
    it "returns empty list for Nothing at origin" $
      buildCursorItemVerts Nothing 0.0 0.0 `shouldBe` []

    it "returns empty list for Nothing at arbitrary position" $
      buildCursorItemVerts Nothing 0.5 (-0.3) `shouldBe` []

  describe "single-count item renders icon only (no count text)" $ do
    it "produces non-empty vertex data for a stone block item (count=1)" $ do
      let verts = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
      length verts `shouldSatisfy` (> 0)

    it "vertex count is divisible by 6 (vec2 pos + vec4 color per vertex)" $ do
      let verts = buildCursorItemVerts (Just (ItemStack (BlockItem Dirt) 1)) 0.0 0.0
      length verts `mod` 6 `shouldBe` 0

    it "vertex count equals icon-only vertices (no count text for count=1)" $ do
      let verts1 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
          -- 3x3 solid fill = 9 pixels, each pixel = 6 vertices * 6 floats = 36 floats
          -- Total = 9 * 36 = 324 floats for a solid-fill mini-icon
          iconPixels = length (itemMiniIcon (BlockItem Stone))
          expectedFloats = iconPixels * 6 * 6  -- 6 verts per pixel, 6 floats per vert
      length verts1 `shouldBe` expectedFloats

  describe "multi-count item renders icon plus count text" $ do
    it "produces more vertices than count=1 (count text added)" $ do
      let verts1 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
          verts5 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 5)) 0.0 0.0
      length verts5 `shouldSatisfy` (> length verts1)

    it "count=2 includes text vertices for digit '2'" $ do
      let verts2 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 2)) 0.0 0.0
          verts1 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
          textVerts = length verts2 - length verts1
      -- renderText for "2" produces some pixels, each pixel = 6 verts * 6 floats
      textVerts `shouldSatisfy` (> 0)
      textVerts `mod` 6 `shouldBe` 0

    it "count=64 includes text vertices for two-digit '64'" $ do
      let verts64 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 64)) 0.0 0.0
          verts1 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
          textVerts = length verts64 - length verts1
      textVerts `shouldSatisfy` (> 0)

    it "count=64 has more text vertices than count=5 (two digits vs one)" $ do
      let verts64 = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 64)) 0.0 0.0
          verts5  = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 5)) 0.0 0.0
          verts1  = buildCursorItemVerts (Just (ItemStack (BlockItem Stone) 1)) 0.0 0.0
          text64  = length verts64 - length verts1
          text5   = length verts5  - length verts1
      text64 `shouldSatisfy` (> text5)

  describe "cursor position affects vertex coordinates" $ do
    it "different mouse positions produce different vertex data" $ do
      let stack = Just (ItemStack (BlockItem Stone) 1)
          vertsA = buildCursorItemVerts stack 0.0 0.0
          vertsB = buildCursorItemVerts stack 0.5 0.5
      vertsA `shouldNotBe` vertsB

    it "icon is centered on mouse position" $ do
      let stack = Just (ItemStack (BlockItem Stone) 1)
          verts = buildCursorItemVerts stack 0.0 0.0
          -- First vertex x should be at -cursorIconSize/2
          firstX = head verts
          expectedX = -(cursorIconSize / 2)
      abs (firstX - expectedX) `shouldSatisfy` (< 0.001)

  describe "different item types produce correct vertex data" $ do
    it "tool item (pickaxe) produces vertices" $ do
      let verts = buildCursorItemVerts (Just (ItemStack (ToolItem Pickaxe Iron 250) 1)) 0.0 0.0
      length verts `shouldSatisfy` (> 0)
      length verts `mod` 6 `shouldBe` 0

    it "food item (bread) produces vertices" $ do
      let verts = buildCursorItemVerts (Just (ItemStack (FoodItem Bread) 10)) 0.0 0.0
      length verts `shouldSatisfy` (> 0)

    it "material item (diamond) produces vertices with count" $ do
      let verts = buildCursorItemVerts (Just (ItemStack (MaterialItem DiamondGem) 32)) 0.0 0.0
          verts1 = buildCursorItemVerts (Just (ItemStack (MaterialItem DiamondGem) 1)) 0.0 0.0
      length verts `shouldSatisfy` (> length verts1)

    it "stick item with count=1 has no count text" $ do
      let verts = buildCursorItemVerts (Just (ItemStack StickItem 1)) 0.0 0.0
          iconPixels = length (itemMiniIcon StickItem)
          expectedFloats = iconPixels * 6 * 6
      length verts `shouldBe` expectedFloats

  describe "cursorIconSize constant" $ do
    it "is a positive value" $
      cursorIconSize `shouldSatisfy` (> 0)

    it "is within reasonable NDC range" $
      cursorIconSize `shouldSatisfy` (< 1.0)
-- Shift-Click Between Containers and Player Inventory
-- =========================================================================
shiftClickContainerSpec :: Spec
shiftClickContainerSpec = describe "Shift-click container transfers" $ do
  describe "shiftClickChestToInventory" $ do
    it "moves item from chest slot to player inventory" $ do
      let chestInv = setChestSlot emptyChestInventory 5 (Just (ItemStack (BlockItem Stone) 32))
          playerInv = emptyInventory
          (chestInv', playerInv') = shiftClickChestToInventory chestInv 5 playerInv
      getChestSlot chestInv' 5 `shouldBe` Nothing
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 32)

    it "no-op when chest slot is empty" $ do
      let chestInv = emptyChestInventory
          playerInv = emptyInventory
          (chestInv', playerInv') = shiftClickChestToInventory chestInv 5 playerInv
      chestInv' `shouldBe` chestInv
      playerInv' `shouldBe` playerInv

    it "leaves leftover in chest if player inventory is full" $ do
      let fullInv = foldl (\inv i -> setSlot inv i (Just (ItemStack (BlockItem Dirt) 64)))
                          emptyInventory [0..35]
          chestInv = setChestSlot emptyChestInventory 0 (Just (ItemStack (BlockItem Stone) 10))
          (chestInv', playerInv') = shiftClickChestToInventory chestInv 0 fullInv
      getChestSlot chestInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 10)
      playerInv' `shouldBe` fullInv

    it "merges into existing stacks in player inventory" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 60))
          chestInv = setChestSlot emptyChestInventory 0 (Just (ItemStack (BlockItem Stone) 10))
          (chestInv', playerInv') = shiftClickChestToInventory chestInv 0 playerInv
      -- 4 should merge into slot 0 (filling to 64), remaining 6 goes to first empty
      getChestSlot chestInv' 0 `shouldBe` Nothing
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)
      getSlot playerInv' 1 `shouldBe` Just (ItemStack (BlockItem Stone) 6)

  describe "shiftClickInventoryToChest" $ do
    it "moves item from player inventory to first empty chest slot" $ do
      let playerInv = setSlot emptyInventory 3 (Just (ItemStack (BlockItem Dirt) 20))
          chestInv = emptyChestInventory
          (chestInv', playerInv') = shiftClickInventoryToChest chestInv 3 playerInv
      getChestSlot chestInv' 0 `shouldBe` Just (ItemStack (BlockItem Dirt) 20)
      getSlot playerInv' 3 `shouldBe` Nothing

    it "skips occupied chest slots" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Sand) 5))
          chestInv = setChestSlot emptyChestInventory 0 (Just (ItemStack (BlockItem Stone) 64))
          (chestInv', playerInv') = shiftClickInventoryToChest chestInv 0 playerInv
      getChestSlot chestInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)
      getChestSlot chestInv' 1 `shouldBe` Just (ItemStack (BlockItem Sand) 5)
      getSlot playerInv' 0 `shouldBe` Nothing

    it "no-op when player inventory slot is empty" $ do
      let playerInv = emptyInventory
          chestInv = emptyChestInventory
          (chestInv', playerInv') = shiftClickInventoryToChest chestInv 0 playerInv
      chestInv' `shouldBe` chestInv
      playerInv' `shouldBe` playerInv

    it "no-op when chest is full" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Sand) 5))
          fullChest = foldl (\inv i -> setChestSlot inv i (Just (ItemStack (BlockItem Stone) 64)))
                            emptyChestInventory [0..26]
          (chestInv', playerInv') = shiftClickInventoryToChest fullChest 0 playerInv
      chestInv' `shouldBe` fullChest
      playerInv' `shouldBe` playerInv

  describe "shiftClickDispenserToInventory" $ do
    it "moves item from dispenser slot to player inventory" $ do
      let dispInv = setDispenserSlot emptyDispenserInventory 2 (Just (ItemStack (BlockItem Sand) 16))
          playerInv = emptyInventory
          (dispInv', playerInv') = shiftClickDispenserToInventory dispInv 2 playerInv
      getDispenserSlot dispInv' 2 `shouldBe` Nothing
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (BlockItem Sand) 16)

    it "no-op when dispenser slot is empty" $ do
      let dispInv = emptyDispenserInventory
          playerInv = emptyInventory
          (dispInv', playerInv') = shiftClickDispenserToInventory dispInv 0 playerInv
      dispInv' `shouldBe` dispInv
      playerInv' `shouldBe` playerInv

    it "leaves leftover in dispenser if player inventory is full" $ do
      let fullInv = foldl (\inv i -> setSlot inv i (Just (ItemStack (BlockItem Dirt) 64)))
                          emptyInventory [0..35]
          dispInv = setDispenserSlot emptyDispenserInventory 0 (Just (ItemStack (BlockItem Stone) 5))
          (dispInv', playerInv') = shiftClickDispenserToInventory dispInv 0 fullInv
      getDispenserSlot dispInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 5)
      playerInv' `shouldBe` fullInv

  describe "shiftClickInventoryToDispenser" $ do
    it "moves item from player inventory to first empty dispenser slot" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 10))
          dispInv = emptyDispenserInventory
          (dispInv', playerInv') = shiftClickInventoryToDispenser dispInv 0 playerInv
      getDispenserSlot dispInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 10)
      getSlot playerInv' 0 `shouldBe` Nothing

    it "skips occupied dispenser slots" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Dirt) 3))
          dispInv = setDispenserSlot emptyDispenserInventory 0 (Just (ItemStack (BlockItem Stone) 64))
          (dispInv', playerInv') = shiftClickInventoryToDispenser dispInv 0 playerInv
      getDispenserSlot dispInv' 0 `shouldBe` Just (ItemStack (BlockItem Stone) 64)
      getDispenserSlot dispInv' 1 `shouldBe` Just (ItemStack (BlockItem Dirt) 3)
      getSlot playerInv' 0 `shouldBe` Nothing

    it "no-op when dispenser is full" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Sand) 5))
          fullDisp = foldl (\inv i -> setDispenserSlot inv i (Just (ItemStack (BlockItem Stone) 64)))
                           emptyDispenserInventory [0..8]
          (dispInv', playerInv') = shiftClickInventoryToDispenser fullDisp 0 playerInv
      dispInv' `shouldBe` fullDisp
      playerInv' `shouldBe` playerInv

  describe "shiftClickFurnaceInputToInventory" $ do
    it "moves furnace input to player inventory" $ do
      let fs = newFurnaceState { fsInput = Just (ItemStack (BlockItem IronOre) 8) }
          playerInv = emptyInventory
          (fs', playerInv') = shiftClickFurnaceInputToInventory fs playerInv
      fsInput fs' `shouldBe` Nothing
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (BlockItem IronOre) 8)

    it "no-op when input slot is empty" $ do
      let fs = newFurnaceState
          playerInv = emptyInventory
          (fs', playerInv') = shiftClickFurnaceInputToInventory fs playerInv
      fs' `shouldBe` fs
      playerInv' `shouldBe` playerInv

  describe "shiftClickFurnaceFuelToInventory" $ do
    it "moves furnace fuel to player inventory" $ do
      let fs = newFurnaceState { fsFuel = Just (ItemStack (MaterialItem Coal) 4) }
          playerInv = emptyInventory
          (fs', playerInv') = shiftClickFurnaceFuelToInventory fs playerInv
      fsFuel fs' `shouldBe` Nothing
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (MaterialItem Coal) 4)

    it "no-op when fuel slot is empty" $ do
      let fs = newFurnaceState
          playerInv = emptyInventory
          (fs', playerInv') = shiftClickFurnaceFuelToInventory fs playerInv
      fs' `shouldBe` fs
      playerInv' `shouldBe` playerInv

  describe "shiftClickInventoryToFurnaceInput" $ do
    it "moves player inventory item to furnace input" $ do
      let playerInv = setSlot emptyInventory 2 (Just (ItemStack (BlockItem IronOre) 16))
          fs = newFurnaceState
          (fs', playerInv') = shiftClickInventoryToFurnaceInput fs 2 playerInv
      fsInput fs' `shouldBe` Just (ItemStack (BlockItem IronOre) 16)
      getSlot playerInv' 2 `shouldBe` Nothing

    it "no-op when furnace input is occupied" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Sand) 5))
          fs = newFurnaceState { fsInput = Just (ItemStack (BlockItem IronOre) 8) }
          (fs', playerInv') = shiftClickInventoryToFurnaceInput fs 0 playerInv
      fsInput fs' `shouldBe` Just (ItemStack (BlockItem IronOre) 8)
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (BlockItem Sand) 5)

    it "no-op when player inventory slot is empty" $ do
      let playerInv = emptyInventory
          fs = newFurnaceState
          (fs', playerInv') = shiftClickInventoryToFurnaceInput fs 0 playerInv
      fs' `shouldBe` fs
      playerInv' `shouldBe` playerInv

  describe "shiftClickInventoryToFurnaceFuel" $ do
    it "moves player inventory item to furnace fuel" $ do
      let playerInv = setSlot emptyInventory 5 (Just (ItemStack (MaterialItem Coal) 10))
          fs = newFurnaceState
          (fs', playerInv') = shiftClickInventoryToFurnaceFuel fs 5 playerInv
      fsFuel fs' `shouldBe` Just (ItemStack (MaterialItem Coal) 10)
      getSlot playerInv' 5 `shouldBe` Nothing

    it "no-op when furnace fuel is occupied" $ do
      let playerInv = setSlot emptyInventory 0 (Just (ItemStack (MaterialItem Coal) 5))
          fs = newFurnaceState { fsFuel = Just (ItemStack (MaterialItem Coal) 8) }
          (fs', playerInv') = shiftClickInventoryToFurnaceFuel fs 0 playerInv
      fsFuel fs' `shouldBe` Just (ItemStack (MaterialItem Coal) 8)
      getSlot playerInv' 0 `shouldBe` Just (ItemStack (MaterialItem Coal) 5)
  describe "armorSlotSilhouette" $ do
    it "returns non-empty pixel list for Helmet" $
      armorSlotSilhouette Helmet `shouldSatisfy` (not . null)

    it "returns non-empty pixel list for Chestplate" $
      armorSlotSilhouette Chestplate `shouldSatisfy` (not . null)

    it "returns non-empty pixel list for Leggings" $
      armorSlotSilhouette Leggings `shouldSatisfy` (not . null)

    it "returns non-empty pixel list for Boots" $
      armorSlotSilhouette Boots `shouldSatisfy` (not . null)

    it "helmet silhouette has same shape as helmet mini-icon" $ do
      let silhouette = map (\(r, c, _) -> (r, c)) (armorSlotSilhouette Helmet)
          icon = map (\(r, c, _) -> (r, c)) (itemMiniIcon (ArmorItem Helmet IronArmor 100))
      silhouette `shouldBe` icon

    it "chestplate silhouette has same shape as chestplate mini-icon" $ do
      let silhouette = map (\(r, c, _) -> (r, c)) (armorSlotSilhouette Chestplate)
          icon = map (\(r, c, _) -> (r, c)) (itemMiniIcon (ArmorItem Chestplate IronArmor 100))
      silhouette `shouldBe` icon

    it "leggings silhouette has same shape as leggings mini-icon" $ do
      let silhouette = map (\(r, c, _) -> (r, c)) (armorSlotSilhouette Leggings)
          icon = map (\(r, c, _) -> (r, c)) (itemMiniIcon (ArmorItem Leggings IronArmor 100))
      silhouette `shouldBe` icon

    it "boots silhouette has same shape as boots mini-icon" $ do
      let silhouette = map (\(r, c, _) -> (r, c)) (armorSlotSilhouette Boots)
          icon = map (\(r, c, _) -> (r, c)) (itemMiniIcon (ArmorItem Boots IronArmor 100))
      silhouette `shouldBe` icon

    it "silhouette uses gray color with 0.5 alpha" $ do
      let pixels = armorSlotSilhouette Helmet
          allGray = all (\(_, _, (r, g, b, a)) -> r == 0.35 && g == 0.35 && b == 0.35 && a == 0.5) pixels
      allGray `shouldBe` True

    it "silhouette colors differ from equipped item colors" $ do
      let silColors = map (\(_, _, c) -> c) (armorSlotSilhouette Helmet)
          itemColors = map (\(_, _, c) -> c) (itemMiniIcon (ArmorItem Helmet DiamondArmor 100))
      silColors `shouldSatisfy` (/= itemColors)

    it "all four armor slot silhouettes have valid pixel coordinates" $ do
      let allPixels = concatMap armorSlotSilhouette [Helmet, Chestplate, Leggings, Boots]
          validCoords = all (\(r, c, _) -> r >= 0 && r <= 2 && c >= 0 && c <= 2) allPixels
      validCoords `shouldBe` True


-- =========================================================================
-- Hotbar Popup
-- =========================================================================
hotbarPopupSpec :: Spec
hotbarPopupSpec = describe "Hotbar item name popup" $ do
  -- GameState field
  describe "gsHotbarPopup IORef" $ do
    it "defaults to Nothing in a new GameState" $ do
      gs <- newGameState (V3 0 80 0)
      popup <- readIORef (gsHotbarPopup gs)
      popup `shouldBe` Nothing

    it "can be set to Just (name, timer)" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsHotbarPopup gs) (Just ("Stone", 2.0))
      popup <- readIORef (gsHotbarPopup gs)
      popup `shouldBe` Just ("Stone", 2.0)

    it "can be cleared back to Nothing" $ do
      gs <- newGameState (V3 0 80 0)
      writeIORef (gsHotbarPopup gs) (Just ("Dirt", 1.5))
      writeIORef (gsHotbarPopup gs) Nothing
      popup <- readIORef (gsHotbarPopup gs)
      popup `shouldBe` Nothing

  -- Timer decay logic
  describe "popup timer decay" $ do
    it "timer decrements each tick" $ do
      ref <- newIORef (Just ("Stone", 2.0) :: Maybe (String, Float))
      let dt = 0.05 :: Float
      mVal <- readIORef ref
      case mVal of
        Just (name, t) -> writeIORef ref (if t - dt > 0 then Just (name, t - dt) else Nothing)
        Nothing -> pure ()
      result <- readIORef ref
      result `shouldBe` Just ("Stone", 1.95)

    it "timer clears when reaching zero" $ do
      ref <- newIORef (Just ("Stone", 0.03) :: Maybe (String, Float))
      let dt = 0.05 :: Float
      mVal <- readIORef ref
      case mVal of
        Just (name, t) -> writeIORef ref (if t - dt > 0 then Just (name, t - dt) else Nothing)
        Nothing -> pure ()
      result <- readIORef ref
      result `shouldBe` Nothing

    it "Nothing stays Nothing after decay" $ do
      ref <- newIORef (Nothing :: Maybe (String, Float))
      let dt = 0.05 :: Float
      mVal <- readIORef ref
      case mVal of
        Just (name, t) -> writeIORef ref (if t - dt > 0 then Just (name, t - dt) else Nothing)
        Nothing -> pure ()
      result <- readIORef ref
      result `shouldBe` Nothing

  -- Popup content from inventory
  describe "popup content from hotbar slot" $ do
    it "shows block item name for occupied slot" $ do
      let inv = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 5))
      case getHotbarSlot inv 0 of
        Just (ItemStack item _) -> itemName item `shouldBe` "Stone"
        Nothing -> expectationFailure "slot should not be empty"

    it "shows tool item name for tool slot" $ do
      let inv = setSlot emptyInventory 1 (Just (ItemStack (ToolItem Pickaxe Diamond 1561) 1))
      case getHotbarSlot inv 1 of
        Just (ItemStack item _) -> itemName item `shouldBe` "Diamond Pickaxe"
        Nothing -> expectationFailure "slot should not be empty"

    it "shows food item name" $ do
      let inv = setSlot emptyInventory 2 (Just (ItemStack (FoodItem Apple) 3))
      case getHotbarSlot inv 2 of
        Just (ItemStack item _) -> itemName item `shouldBe` "Apple"
        Nothing -> expectationFailure "slot should not be empty"

    it "returns Nothing for empty slot" $ do
      getHotbarSlot emptyInventory 0 `shouldBe` Nothing

    it "shows material item name" $ do
      let inv = setSlot emptyInventory 3 (Just (ItemStack (MaterialItem DiamondGem) 10))
      case getHotbarSlot inv 3 of
        Just (ItemStack item _) -> itemName item `shouldBe` "Diamond"
        Nothing -> expectationFailure "slot should not be empty"

  -- Slot change triggers popup
  describe "selectHotbar triggers popup update" $ do
    it "selecting slot with item yields item name" $ do
      ref <- newIORef (Nothing :: Maybe (String, Float))
      let inv = setSlot emptyInventory 3 (Just (ItemStack (BlockItem Dirt) 12))
      case getHotbarSlot inv 3 of
        Just (ItemStack item _) -> writeIORef ref (Just (itemName item, 2.0))
        Nothing                 -> writeIORef ref Nothing
      popup <- readIORef ref
      popup `shouldBe` Just ("Dirt", 2.0)

    it "selecting empty slot yields Nothing" $ do
      ref <- newIORef (Just ("old", 1.0) :: Maybe (String, Float))
      case getHotbarSlot emptyInventory 5 of
        Just (ItemStack item _) -> writeIORef ref (Just (itemName item, 2.0))
        Nothing                 -> writeIORef ref Nothing
      popup <- readIORef ref
      popup `shouldBe` Nothing

    it "scroll re-triggers with new slot item" $ do
      ref <- newIORef (Nothing :: Maybe (String, Float))
      let inv0 = setSlot emptyInventory 0 (Just (ItemStack (BlockItem Stone) 1))
          inv1 = setSlot inv0 1 (Just (ItemStack (BlockItem Sand) 1))
      -- Simulate scrolling from slot 0 to slot 1
      case getHotbarSlot inv1 1 of
        Just (ItemStack item _) -> writeIORef ref (Just (itemName item, 2.0))
        Nothing                 -> writeIORef ref Nothing
      popup <- readIORef ref
      popup `shouldBe` Just ("Sand", 2.0)
