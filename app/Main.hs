module Main (main) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Graphics.UI.GLFW as GLFW

import Engine.Window
import Engine.Camera
import Engine.Mesh (BlockVertex(..), MeshData(..), meshChunkWithLight)
import Engine.BitmapFont
import Engine.Types (defaultEngineConfig, EngineConfig(..), UniformBufferObject(..))
import Engine.Vulkan.Init
import Engine.Vulkan.Swapchain
import Engine.Vulkan.Pipeline
import Engine.Vulkan.Command
import Engine.Vulkan.Memory
import Engine.Vulkan.Descriptor
import Engine.Vulkan.Texture
import World.Block (BlockType(..), BlockProperties(..), blockProperties, isSolid, isGravityAffected, isLeafBlock, blockCollisionHeight)
import World.Chunk
import World.Generation
import World.World
import Game.Player
import Game.Physics (BlockQuery, BlockHeightQuery)
import Game.Inventory
import Game.Item
import Game.Crafting
import Game.Enchanting
import Game.DayNight
import World.Biome (biomeAt)
import World.Weather (WeatherState(..), WeatherType(..), newWeatherState, updateWeather, isRaining, weatherSkyMultiplier, weatherAmbientMultiplier)
import Game.Furnace
import World.Fluid
import World.Light
import Entity.ECS
import Entity.Mob (MobType(..), MobInfo(..), updateMobAI, AIState(..), mobInfo, damageEntity, isHostile)
import Entity.Spawn
import Game.Save
import Game.DroppedItem
import Game.BlockEntity
import Game.State (GameState(..), GameMode(..), Projectile(..), newGameState)
import Game.ItemDisplay (itemColor, itemMiniIcon)
import Engine.Sound
import Game.Particle

import World.Redstone (RedstoneState, newRedstoneState, setPower, getPower, propagateRedstone)

import Control.Monad (unless, when, forM_, forM, void)
import Control.Concurrent.STM (readTVarIO, atomically, writeTVar)
import Control.Exception (finally)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.IORef
import Linear
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf, poke)
import Foreign.Marshal.Utils (copyBytes)
import Data.Bits ((.|.))
import System.FilePath ((</>))
import qualified System.Random
import System.Random (randomRIO)
import System.Environment (getArgs)
import Data.Char (isDigit)

-- | Debug overlay information shown when F3 is active
data DebugInfo = DebugInfo
  { dbgPos         :: !(V3 Float)
  , dbgYaw         :: !Float
  , dbgPitch       :: !Float
  , dbgFps         :: !Int
  , dbgChunkCount  :: !Int
  , dbgBiome       :: !String
  , dbgTargetBlock :: !String
  , dbgLightLevel  :: !String
  , dbgEntityCount :: !Int
  , dbgWeather     :: !String
  , dbgGameMode    :: !String
  , dbgHealth      :: !Int
  , dbgHunger      :: !Int
  , dbgTimeOfDay   :: !String
  , dbgDayTime     :: !Float
  }

-- | Fixed timestep for physics (20 ticks per second, like Minecraft)
tickRate :: Float
tickRate = 1.0 / 20.0

-- | Max reach distance for block interaction
maxReach :: Float
maxReach = 5.0

-- | Default save directory root
savesRoot :: FilePath
savesRoot = "saves"

-- | Starting inventory for a new world
defaultStartInventory :: Inventory
defaultStartInventory = selectHotbar (foldl (\i (item, cnt) -> fst $ addItem i item cnt) emptyInventory
  [ (ToolItem Pickaxe Wood 59, 1)
  , (ToolItem Sword Wood 59, 1)
  , (BlockItem Stone, 64)
  , (BlockItem Dirt, 64)
  , (BlockItem OakPlanks, 64)
  , (BlockItem Torch, 16)
  ]) 2

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let seed = case args of
        (s:_) | all isDigit s && not (null s) -> read s
        _ -> 12345
  let config = defaultEngineConfig
  putStrLn $ "Starting " ++ ecWindowTitle config ++ " (seed: " ++ show seed ++ ")..."

  withWindow (ecWindowWidth config) (ecWindowHeight config) (ecWindowTitle config) $ \wh -> do
    -- Initialize Vulkan
    putStrLn "Initializing Vulkan..."
    vc <- createVulkanContext (whWindow wh) (ecEnableValidation config)
    let device = vcDevice vc
        physDevice = vcPhysicalDevice vc
        qfi = vcQueueFamilies vc

    -- Start with cursor visible for menu
    GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal

    -- Create swapchain
    windowSize <- getWindowSize wh
    scRef <- newIORef =<< createSwapchain vc windowSize

    -- Command pool
    cmdPool <- createCommandPool device (qfGraphicsFamily qfi)

    -- Texture atlas
    texAtlas <- createPlaceholderAtlas physDevice device cmdPool (vcGraphicsQueue vc)

    -- Descriptors
    dsLayout <- createDescriptorSetLayout device
    let maxFrames = ecMaxFramesInFlight config
    dsPool <- createDescriptorPool device maxFrames
    dsSets <- allocateDescriptorSets device dsPool dsLayout maxFrames

    let uboSize = fromIntegral $ sizeOf (undefined :: UniformBufferObject)
    uniformBufs <- V.replicateM maxFrames $ createUniformBuffer physDevice device uboSize

    V.forM_ (V.zip dsSets uniformBufs) $ \(ds, ub) ->
      updateDescriptorSetWithTexture device ds ub (tiImageView texAtlas) (tiSampler texAtlas)

    -- Render pass and pipeline (with depth buffer)
    sc <- readIORef scRef
    depthFormat <- findDepthFormat physDevice
    renderPass <- createRenderPass device (scFormat sc) depthFormat
    depthRef <- newIORef =<< createDepthResources physDevice device (scExtent sc)
    let shaderDir = "shaders"
    pc <- createGraphicsPipeline device renderPass dsLayout
      (shaderDir </> "block_vert.spv")
      (shaderDir </> "block_frag.spv")
    hudPipeline <- createHudPipeline device renderPass
      (shaderDir </> "hud_vert.spv")
      (shaderDir </> "hud_frag.spv")
    depth <- readIORef depthRef
    fbRef <- newIORef =<< createFramebuffers device renderPass sc (drImageView depth)

    cmdBuffers <- allocateCommandBuffers device cmdPool maxFrames
    syncObjects <- createSyncObjects device maxFrames

    -- World
    let genCfg = defaultGenConfig { gcSeed = seed }
        renderDist = 4
    world <- newWorld genCfg renderDist

    -- Player (try to load from save, else default)
    let spawnPos = V3 0 80 0
    -- Unified game state
    gs <- newGameState spawnPos
    let saveDirRef          = gsSaveDir gs
        playerRef           = gsPlayer gs
        inventoryRef        = gsInventory gs
        gameModeRef         = gsGameMode gs
        cursorItemRef       = gsCursorItem gs
        craftingGridRef     = gsCraftingGrid gs
        furnaceStateRef     = gsFurnaceState gs
        furnacePosRef       = gsFurnacePos gs
        debugOverlayRef     = gsDebugOverlay gs
        targetBlockRef      = gsTargetBlock gs
        dayNightRef         = gsDayNight gs
        weatherRef          = gsWeather gs
        spawnPointRef       = gsSpawnPoint gs
        sleepMessageRef     = gsSleepMessage gs
        blockEntityMapRef   = gsBlockEntities gs
        chestPosRef         = gsChestPos gs
        dispenserPosRef     = gsDispenserPos gs
        redstoneStateRef    = gsRedstone gs
        enchantMapRef       = gsEnchantMap gs
        enchantItemRef      = gsEnchantItem gs
        enchantOptionsRef   = gsEnchantOptions gs
        playerXPRef         = gsPlayerXP gs
        damageFlashRef      = gsDamageFlash gs
        cactusDamageTimerRef = gsCactusDmgTimer gs
        poisonTimerRef      = gsPoisonTimer gs
        speedBuffTimerRef   = gsSpeedBuff gs
        spawnRngRef         = gsSpawnRng gs
        spawnCooldownRef    = gsSpawnCooldown gs
        aiStatesRef         = gsAiStates gs
        creeperFuseRef      = gsCreeperFuse gs
        projectilesRef      = gsProjectiles gs
        skeletonCooldownRef = gsSkeletonCd gs
        fishingStateRef     = gsFishing gs
        ridingEntityRef     = gsRiding gs
        hudVertCountRef     = gsHudVertCount gs
        meshCacheRef        = gsMeshCache gs
        inputRef            = gsInput gs
        lastCursorRef       = gsLastCursor gs
        mousePosRef         = gsMousePos gs
        miningRef           = gsMining gs
        lmbHeldRef          = gsLmbHeld gs
        rmbHeldRef          = gsRmbHeld gs
        frameRef            = gsFrame gs
        lastTimeRef         = gsLastTime gs
        accumRef            = gsAccum gs
        fpsCounterRef       = gsFpsCounter gs
        fpsTimerRef         = gsFpsTimer gs
        fpsDisplayRef       = gsFpsDisplay gs

    -- World save management: use world1 as default
    let defaultSaveDir = savesRoot </> "world1"
    mSavedData <- loadPlayer defaultSaveDir
    -- Restore saved player state if available
    case mSavedData of
      Just sd -> do
        writeIORef playerRef (playerFromSaveData sd)
        writeIORef inventoryRef (slotListToInventory (sdInventory sd) (sdSelectedSlot sd))
        writeIORef dayNightRef (newDayNightCycle { dncTime = sdDayTime sd, dncDayCount = sdDayCount sd })
      Nothing -> pure ()
    fluidState <- newFluidState
    droppedItems <- newDroppedItems
    particleSystemRef <- newParticleSystem

    -- Sound system (no-op stub, ready for real audio backend)
    soundSystem <- initSoundSystem

    -- Entity system
    entityWorld <- newEntityWorld

    -- Give player some starting blocks (only when no saved inventory)
    case mSavedData of
      Just _  -> pure ()  -- inventory already restored from save
      Nothing -> writeIORef inventoryRef defaultStartInventory

    -- Try to load saved world, otherwise generate fresh
    loaded <- loadWorld defaultSaveDir world
    if loaded
      then do
        chunkCount <- loadedChunkCount world
        putStrLn $ "Loaded " ++ show chunkCount ++ " saved chunks"
      else do
        _ <- updateLoadedChunks world spawnPos
        chunkCount <- loadedChunkCount world
        putStrLn $ "Generated " ++ show chunkCount ++ " initial chunks"

    -- HUD: use a host-visible buffer that's updated each frame with inventory contents
    let hudMaxVerts = 5000  -- enough for inventory/crafting screens + text
        hudBufSize = fromIntegral (hudMaxVerts * 24) :: Vk.DeviceSize  -- 24 bytes per vertex (vec2 + vec4)
    hudBuf <- createBuffer physDevice device hudBufSize
      Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
      (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

    -- Settle gravity-affected blocks in all initially loaded chunks
    settleAllLoadedChunks world
    rebuildAllChunkMeshes world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef

    -- Mouse movement callback (FPS look in Playing, position tracking in UI)
    GLFW.setCursorPosCallback (whWindow wh) $ Just $ \_win xpos ypos -> do
      writeIORef mousePosRef (xpos, ypos)
      mode <- readIORef gameModeRef
      when (mode == Playing) $ do
        mLast <- readIORef lastCursorRef
        case mLast of
          Nothing -> writeIORef lastCursorRef (Just (xpos, ypos))
          Just (lastX, lastY) -> do
            let dx = realToFrac (xpos - lastX) :: Float
                dy = realToFrac (lastY - ypos) :: Float
            modifyIORef' inputRef $ \inp -> inp { piMouseDX = piMouseDX inp + dx
                                                , piMouseDY = piMouseDY inp + dy }
            writeIORef lastCursorRef (Just (xpos, ypos))

    -- Helper: cancel eating (resets timer to 0)
    let cancelEating = modifyIORef' playerRef (\p -> p { plEatingTimer = 0.0 })
        selectSlot n = modifyIORef' inventoryRef (`selectHotbar` n) >> cancelEating
        triggerDamageFlash = do
          writeIORef damageFlashRef 0.3
          playSound soundSystem SndPlayerHurt
        -- Helper: sync furnace IORef to/from block entity map
        syncFurnaceToMap = do
          fs <- readIORef furnaceStateRef
          mPos <- readIORef furnacePosRef
          case mPos of
            Just pos -> setFurnaceState blockEntityMapRef pos fs
            Nothing  -> pure ()
        syncFurnaceFromMap = do
          mPos <- readIORef furnacePosRef
          case mPos of
            Just pos -> do
              mFs <- getFurnaceState blockEntityMapRef pos
              case mFs of
                Just fs -> writeIORef furnaceStateRef fs
                Nothing -> pure ()
            Nothing -> pure ()

    -- Scroll wheel callback (cycle hotbar slots)
    GLFW.setScrollCallback (whWindow wh) $ Just $ \_win _dx dy -> do
      mode <- readIORef gameModeRef
      when (mode == Playing) $ do
        inv <- readIORef inventoryRef
        let cur = invSelected inv
            newSlot
              | dy > 0    = (cur - 1) `mod` hotbarSlots
              | dy < 0    = (cur + 1) `mod` hotbarSlots
              | otherwise = cur
        when (newSlot /= cur) cancelEating
        modifyIORef' inventoryRef (`selectHotbar` newSlot)

    -- Mouse button callback
    GLFW.setMouseButtonCallback (whWindow wh) $ Just $ \_win button action _mods -> do
      mode <- readIORef gameModeRef
      case mode of
        MainMenu -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
          -- "New World" button
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= -0.12 && ndcY <= 0.07) $ do
            -- Create a new world with a unique save name
            newName <- nextSaveName
            let newDir = savesRoot </> newName
            writeIORef saveDirRef newDir
            -- Reset world chunks (clear old data)
            atomically $ writeTVar (worldChunks world) HM.empty
            -- Generate fresh chunks around spawn
            _ <- updateLoadedChunks world spawnPos
            -- Reset player to spawn
            writeIORef playerRef (defaultPlayer spawnPos)
            -- Reset all game state for new world
            writeIORef inventoryRef defaultStartInventory
            writeIORef cursorItemRef Nothing
            writeIORef craftingGridRef (emptyCraftingGrid 3)
            writeIORef dayNightRef newDayNightCycle
            writeIORef furnaceStateRef newFurnaceState
            writeIORef furnacePosRef Nothing
            writeIORef chestPosRef Nothing
            writeIORef dispenserPosRef Nothing
            writeIORef blockEntityMapRef HM.empty
            writeIORef droppedItems []
            writeIORef (ewEntities entityWorld) IM.empty
            writeIORef (ewNextId entityWorld) 1
            writeIORef aiStatesRef HM.empty
            writeIORef creeperFuseRef IM.empty
            writeIORef projectilesRef []
            writeIORef skeletonCooldownRef HM.empty
            writeIORef fishingStateRef Nothing
            writeIORef ridingEntityRef Nothing
            writeIORef spawnCooldownRef 0.0
            writeIORef spawnPointRef spawnPos
            writeIORef ridingEntityRef Nothing
            writeIORef sleepMessageRef Nothing
            writeIORef miningRef Nothing
            writeIORef weatherRef newWeatherState
            writeIORef particleSystemRef []
            writeIORef (fsFluids fluidState) Map.empty
            writeIORef (fsDirty fluidState) Seq.empty
            writeIORef poisonTimerRef 0.0
            writeIORef speedBuffTimerRef 0.0
            -- Settle gravity and rebuild chunk meshes
            settleAllLoadedChunks world
            rebuildAllChunkMeshes world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef
            writeIORef gameModeRef Playing
            GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
            writeIORef lastCursorRef Nothing
            putStrLn $ "New world: " ++ newName
          -- "Load World" button: load most recent save
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= 0.1 && ndcY <= 0.27) $ do
            saves <- listSaves
            case saves of
              [] -> putStrLn "No saved worlds found"
              _  -> do
                -- Load the last (most recent by name) save
                let saveName = last saves
                    sd = savesRoot </> saveName
                writeIORef saveDirRef sd
                -- Clear and reload world
                atomically $ writeTVar (worldChunks world) HM.empty
                loaded <- loadWorld sd world
                when loaded $ do
                  mData <- loadPlayer sd
                  case mData of
                    Just savedData ->
                      restoreFromSave playerRef inventoryRef dayNightRef savedData
                    Nothing -> pure ()
                  settleAllLoadedChunks world
                  rebuildAllChunkMeshes world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef
                  putStrLn $ "Loaded world: " ++ saveName
                writeIORef gameModeRef Playing
                GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
                writeIORef lastCursorRef Nothing
          -- "Quit" button: centered, y = 0.3 to 0.4
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= 0.32 && ndcY <= 0.49) $
            GLFW.setWindowShouldClose (whWindow wh) True

        Paused -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
          -- Resume button (larger hit area)
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= -0.2 && ndcY <= 0.0) $ do
            writeIORef gameModeRef Playing
            GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
            writeIORef lastCursorRef Nothing
          -- Save & Quit to Menu button
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= 0.05 && ndcY <= 0.25) $ do
            player <- readIORef playerRef
            inv <- readIORef inventoryRef
            dayNight <- readIORef dayNightRef
            sd <- readIORef saveDirRef
            savePlayer sd (buildSaveData player inv dayNight)
            saveWorld sd world
            putStrLn "World saved."
            writeIORef gameModeRef MainMenu
          -- Quit Game button
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= 0.3 && ndcY <= 0.5) $
            GLFW.setWindowShouldClose (whWindow wh) True

        InventoryOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
          -- Check 2x2 crafting grid (above inventory, center-right)
          let craft2x2X0 = -0.03 :: Float
              craft2x2Y0 = -0.80 :: Float
              craft2x2Sz = 0.10 :: Float
              craftCol = floor ((ndcX - craft2x2X0) / craft2x2Sz) :: Int
              craftRow = floor ((ndcY - craft2x2Y0) / craft2x2Sz) :: Int
              inCraftGrid = craftCol >= 0 && craftCol < 2 && craftRow >= 0 && craftRow < 2
              -- Output slot area
              outX0 = 0.35 :: Float; outY0 = -0.75 :: Float; outSz = 0.10 :: Float
              inOutput = ndcX >= outX0 && ndcX <= outX0 + outSz && ndcY >= outY0 && ndcY <= outY0 + outSz
          if inCraftGrid then do
            -- Click on 2x2 crafting grid slot
            cursor <- readIORef cursorItemRef
            grid <- readIORef craftingGridRef
            let slotContent = getCraftingSlot grid craftRow craftCol
            case cursor of
              Just (ItemStack item 1) -> do
                writeIORef craftingGridRef (setCraftingSlot grid craftRow craftCol (Just item))
                writeIORef cursorItemRef (fmap (\i -> ItemStack i 1) slotContent)
              Just (ItemStack item n) | n > 1 -> case slotContent of
                Nothing -> do
                  writeIORef craftingGridRef (setCraftingSlot grid craftRow craftCol (Just item))
                  writeIORef cursorItemRef (Just (ItemStack item (n - 1)))
                _ -> pure ()
              Nothing -> do
                writeIORef craftingGridRef (setCraftingSlot grid craftRow craftCol Nothing)
                writeIORef cursorItemRef (fmap (\i -> ItemStack i 1) slotContent)
              _ -> pure ()
          else if inOutput then do
            -- Take crafted output from 2x2 grid
            grid <- readIORef craftingGridRef
            case tryCraft grid of
              CraftSuccess item count -> do
                cursor <- readIORef cursorItemRef
                case cursor of
                  Nothing -> do
                    writeIORef cursorItemRef (Just (ItemStack item count))
                    writeIORef craftingGridRef (emptyCraftingGrid 2)
                  _ -> pure ()
              CraftFailure -> pure ()
          else do
            -- Check inventory slot
            let mSlot = hitInventorySlot ndcX ndcY
            case mSlot of
              Nothing -> pure ()
              Just slotIdx -> do
                inv <- readIORef inventoryRef
                cursor <- readIORef cursorItemRef
                let slotContent = getSlot inv slotIdx
                writeIORef inventoryRef (setSlot inv slotIdx cursor)
                writeIORef cursorItemRef slotContent

        CraftingOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              mSlot = hitCraftingSlot ndcX ndcY
          case mSlot of
            Just (CraftGrid row col) -> do
              cursor <- readIORef cursorItemRef
              grid <- readIORef craftingGridRef
              let slotContent = getCraftingSlot grid row col
              -- Place cursor item into grid (only block items)
              case cursor of
                Just (ItemStack item 1) -> do
                  writeIORef craftingGridRef (setCraftingSlot grid row col (Just item))
                  writeIORef cursorItemRef (fmap (\i -> ItemStack i 1) slotContent)
                Just (ItemStack item n) | n > 1 -> case slotContent of
                  Nothing -> do  -- slot empty: place one from cursor
                    writeIORef craftingGridRef (setCraftingSlot grid row col (Just item))
                    writeIORef cursorItemRef (Just (ItemStack item (n - 1)))
                  Just existing | existing == item -> pure ()  -- same item already there
                  Just _ -> pure ()  -- different item: refuse placement
                Nothing -> do
                  writeIORef craftingGridRef (setCraftingSlot grid row col Nothing)
                  writeIORef cursorItemRef (fmap (\i -> ItemStack i 1) slotContent)
                _ -> pure ()
            Just CraftOutput -> do
              -- Take crafted item
              grid <- readIORef craftingGridRef
              case tryCraft grid of
                CraftSuccess item count -> do
                  cursor <- readIORef cursorItemRef
                  case cursor of
                    Nothing -> do
                      writeIORef cursorItemRef (Just (ItemStack item count))
                      -- Consume 1 item per occupied slot (not clearing entire stacks)
                      let size = cgSize grid
                          consumed = foldl (\g (r, c) ->
                            case getCraftingSlot g r c of
                              Just _  -> setCraftingSlot g r c Nothing
                              Nothing -> g
                            ) grid [(r, c) | r <- [0..size-1], c <- [0..size-1]]
                      writeIORef craftingGridRef consumed
                    _ -> pure ()  -- cursor occupied
                CraftFailure -> pure ()
            Just (CraftInvSlot idx) -> do
              inv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot inv idx
              writeIORef inventoryRef (setSlot inv idx cursor)
              writeIORef cursorItemRef slotContent
            Nothing -> pure ()

        ChestOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              mSlot = hitChestSlot ndcX ndcY
          case mSlot of
            Just (ChestSlot idx) -> do
              -- Click on chest slot (0-26): swap with cursor
              mChestPos <- readIORef chestPosRef
              case mChestPos of
                Nothing -> pure ()
                Just cPos -> do
                  mChestInv <- getChestInventory blockEntityMapRef cPos
                  case mChestInv of
                    Nothing -> pure ()
                    Just chestInv -> do
                      cursor <- readIORef cursorItemRef
                      let slotContent = getChestSlot chestInv idx
                          newChestInv = setChestSlot chestInv idx cursor
                      setChestInventory blockEntityMapRef cPos newChestInv
                      writeIORef cursorItemRef slotContent
            Just (ChestInvSlot idx) -> do
              -- Click on player inventory slot: swap with cursor
              curInv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot curInv idx
              modifyIORef' inventoryRef (\i -> setSlot i idx cursor)
              writeIORef cursorItemRef slotContent
            Nothing -> pure ()
        FurnaceOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              mSlot = hitFurnaceSlot ndcX ndcY
          case mSlot of
            Just FurnaceInputSlot -> do
              fs <- readIORef furnaceStateRef
              cursor <- readIORef cursorItemRef
              let slotContent = getFurnaceInput fs
              writeIORef furnaceStateRef (setFurnaceInput fs cursor)
              writeIORef cursorItemRef slotContent
            Just FurnaceFuelSlot -> do
              fs <- readIORef furnaceStateRef
              cursor <- readIORef cursorItemRef
              let slotContent = getFurnaceFuel fs
              writeIORef furnaceStateRef (setFurnaceFuel fs cursor)
              writeIORef cursorItemRef slotContent
            Just FurnaceOutputSlot -> do
              -- Output slot: only take, don't place
              fs <- readIORef furnaceStateRef
              cursor <- readIORef cursorItemRef
              case (cursor, getFurnaceOutput fs) of
                (Nothing, Just outStack) -> do
                  writeIORef cursorItemRef (Just outStack)
                  writeIORef furnaceStateRef (setFurnaceOutput fs Nothing)
                (Just (ItemStack cItem cCount), Just (ItemStack oItem oCount))
                  | cItem == oItem && cCount + oCount <= 64 -> do
                      writeIORef cursorItemRef (Just (ItemStack cItem (cCount + oCount)))
                      writeIORef furnaceStateRef (setFurnaceOutput fs Nothing)
                _ -> pure ()
            Just (FurnaceInvSlot idx) -> do
              inv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot inv idx
              writeIORef inventoryRef (setSlot inv idx cursor)
              writeIORef cursorItemRef slotContent
            Nothing -> pure ()
          -- Sync furnace IORef back to block entity after any UI interaction
          syncFurnaceToMap

        DispenserOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              mSlot = hitDispenserSlot ndcX ndcY
          case mSlot of
            Just (DispenserSlot idx) -> do
              mDispPos <- readIORef dispenserPosRef
              case mDispPos of
                Nothing -> pure ()
                Just dPos -> do
                  mDispInv <- getDispenserInventory blockEntityMapRef dPos
                  case mDispInv of
                    Nothing -> pure ()
                    Just dispInv -> do
                      cursor <- readIORef cursorItemRef
                      let slotContent = getDispenserSlot dispInv idx
                          newDispInv = setDispenserSlot dispInv idx cursor
                      setDispenserInventory blockEntityMapRef dPos newDispInv
                      writeIORef cursorItemRef slotContent
            Just (DispenserInvSlot idx) -> do
              curInv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot curInv idx
              modifyIORef' inventoryRef (\i -> setSlot i idx cursor)
              writeIORef cursorItemRef slotContent
            Nothing -> pure ()

        EnchantingOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              itemSlotX0 = -0.26 :: Float
              itemSlotY0 = -0.16 :: Float
              itemSlotSz = 0.12 :: Float
              inItemSlot = ndcX >= itemSlotX0 && ndcX <= itemSlotX0 + itemSlotSz
                        && ndcY >= itemSlotY0 && ndcY <= itemSlotY0 + itemSlotSz
              optBtnX0 = 0.05 :: Float
              optBtnW  = 0.50 :: Float
              optBtnH  = 0.10 :: Float
              optBtnGap = 0.02 :: Float
              optionIdx = if ndcX >= optBtnX0 && ndcX <= optBtnX0 + optBtnW
                          then let relY = ndcY - (-0.25)
                                   row = floor (relY / (optBtnH + optBtnGap)) :: Int
                               in if row >= 0 && row < 3 && relY >= 0
                                     && (relY - fromIntegral row * (optBtnH + optBtnGap)) <= optBtnH
                                  then Just row
                                  else Nothing
                          else Nothing
          if inItemSlot then do
            cursor <- readIORef cursorItemRef
            eItem <- readIORef enchantItemRef
            writeIORef enchantItemRef cursor
            writeIORef cursorItemRef eItem
            xpLvl <- readIORef playerXPRef
            eSeed <- randomRIO (0, 999999 :: Int)
            writeIORef enchantOptionsRef (generateEnchantments xpLvl eSeed)
          else case optionIdx of
            Just idx -> do
              eItem <- readIORef enchantItemRef
              case eItem of
                Just (ItemStack _item _cnt) -> do
                  options <- readIORef enchantOptionsRef
                  when (idx < length options) $ do
                    let (etype, elvl, cost) = options !! idx
                    xpLvl <- readIORef playerXPRef
                    when (xpLvl >= cost) $ do
                      writeIORef playerXPRef (xpLvl - cost)
                      existingEnchants <- getEnchantments enchantMapRef 0
                      let newEnchant = Enchantment etype elvl
                      setEnchantments enchantMapRef 0 (newEnchant : existingEnchants)
                      newXp <- readIORef playerXPRef
                      eSeed <- randomRIO (0, 999999 :: Int)
                      writeIORef enchantOptionsRef (generateEnchantments newXp eSeed)
                Nothing -> pure ()
            Nothing -> do
              let mSlot = hitInventorySlot ndcX ndcY
              case mSlot of
                Just slotIdx -> do
                  inv <- readIORef inventoryRef
                  cursor <- readIORef cursorItemRef
                  let slotContent = getSlot inv slotIdx
                  writeIORef inventoryRef (setSlot inv slotIdx cursor)
                  writeIORef cursorItemRef slotContent
                Nothing -> pure ()

        DeathScreen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
          -- "Respawn" button hit test
          when (ndcX >= -0.25 && ndcX <= 0.25 && ndcY >= 0.15 && ndcY <= 0.30) $ do
            spawnPos <- readIORef spawnPointRef
            modifyIORef' playerRef (respawnPlayer spawnPos)
            writeIORef gameModeRef Playing
            GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
            writeIORef lastCursorRef Nothing

        Playing -> do
          when (button == GLFW.MouseButton'1) $
            writeIORef lmbHeldRef (action == GLFW.MouseButtonState'Pressed)
          when (button == GLFW.MouseButton'2) $
            writeIORef rmbHeldRef (action == GLFW.MouseButtonState'Pressed)
          -- Cancel eating on RMB release
          when (button == GLFW.MouseButton'2 && action == GLFW.MouseButtonState'Released)
            cancelEating
          -- Melee attack / boat breaking: LMB pressed
          when (button == GLFW.MouseButton'1 && action == GLFW.MouseButtonState'Pressed) $ do
            inv <- readIORef inventoryRef
            player <- readIORef playerRef
            nearby <- entitiesInRange entityWorld (plPos player) 3.0
            let boats = filter (\e -> entTag e == "Boat") nearby
            case boats of
              (_:_) -> do
                let closestBoat = closestTo (plPos player) boats
                riding <- readIORef ridingEntityRef
                when (riding == Just (entId closestBoat)) $
                  writeIORef ridingEntityRef Nothing
                destroyEntity entityWorld (entId closestBoat)
                spawnDrop droppedItems BoatItem 1 (entPosition closestBoat)
                putStrLn $ "Broke boat at " ++ show (entPosition closestBoat)
              [] -> case selectedItem inv of
                Just (ItemStack (ToolItem Sword material _) _) -> do
                  let attackDmg = tiAttackDamage (toolInfo material)
                  case nearby of
                    [] -> pure ()
                    _  -> do
                      let closest = closestTo (plPos player) nearby
                      updateEntity entityWorld (entId closest) (\e -> damageEntity e attackDmg)
                      putStrLn $ "Attacked " ++ entTag closest ++ " for " ++ show attackDmg ++ " damage!"
                _ -> pure ()

          -- Stop mining on LMB release (Playing mode only)
          when (button == GLFW.MouseButton'1 && action == GLFW.MouseButtonState'Released) $
            writeIORef miningRef Nothing

          -- Raycast and block interaction (Playing mode only)
          when (action == GLFW.MouseButtonState'Pressed) $ do
            player <- readIORef playerRef
            let eyePos = plPos player + V3 0 1.62 0
                dir = dirFromPlayer player
                blockQueryCb bx by bz = do
                  bt <- worldGetBlock world (V3 bx by bz)
                  pure (World.Block.isSolid bt)

            -- RMB: start eating if holding food and hungry
            when (button == GLFW.MouseButton'2) $ do
              inv <- readIORef inventoryRef
              case selectedItem inv of
                Just (ItemStack (FoodItem _) _)
                  | plHunger player < maxHunger -> do
                      modifyIORef' playerRef (\p -> p { plEatingTimer = 1.6 })
                _ -> pure ()

            -- RMB: drink potion if holding potion item
            when (button == GLFW.MouseButton'2) $ do
              inv <- readIORef inventoryRef
              case selectedItem inv of
                Just (ItemStack (PotionItem pt) _) -> do
                  let (inv', _) = removeItem inv (PotionItem pt) 1
                      (inv'', _) = addItem inv' GlassBottleItem 1
                  writeIORef inventoryRef inv''
                  case pt of
                    HealingPotion -> do
                      modifyIORef' playerRef (\p -> p { plHealth = min maxHealth (plHealth p + 4) })
                      putStrLn "Drank Healing Potion, restored 4 health"
                    PoisonPotion -> do
                      writeIORef poisonTimerRef 5.0
                      putStrLn "Drank Poison Potion, poisoned for 5 seconds"
                    SpeedPotion -> do
                      writeIORef speedBuffTimerRef 30.0
                      putStrLn "Drank Speed Potion, speed boost for 30 seconds"
                    _ -> putStrLn $ "Drank " ++ potionName pt ++ " (no effect)"
                _ -> pure ()

            -- RMB: shear sheep if holding shears
            when (button == GLFW.MouseButton'2) $ do
              inv <- readIORef inventoryRef
              case selectedItem inv of
                Just (ItemStack (ShearsItem _) _) -> do
                  nearby <- entitiesInRange entityWorld (plPos player) 3.0
                  let sheep = filter (\e -> entTag e == "Sheep") nearby
                  case sheep of
                    [] -> pure ()
                    _  -> do
                      let closest = closestTo (plPos player) sheep
                      -- Drop 1-3 wool blocks
                      woolCount <- randomRIO (1 :: Int, 3)
                      spawnDrop droppedItems (BlockItem Wool) woolCount (entPosition closest)
                      putStrLn $ "Sheared sheep for " ++ show woolCount ++ " wool!"
                      -- Consume 1 durability from shears
                      let inv' = case getSlot inv (invSelected inv) of
                            Just (ItemStack (ShearsItem dur) 1)
                              | dur <= 1  -> setSlot inv (invSelected inv) Nothing
                              | otherwise -> setSlot inv (invSelected inv) (Just (ItemStack (ShearsItem (dur - 1)) 1))
                            _ -> inv
                      writeIORef inventoryRef inv'
                _ -> pure ()

            -- RMB: wolf interactions (tame, sit toggle, heal)
            when (button == GLFW.MouseButton'2) $ do
              inv <- readIORef inventoryRef
              player <- readIORef playerRef
              nearby <- entitiesInRange entityWorld (plPos player) 3.0
              let wolves = filter (\e -> entTag e `elem` ["Wolf", "TamedWolf", "TamedWolfSitting"]) nearby
              case wolves of
                [] -> pure ()
                _  -> do
                  let closest = foldr1 (\a b -> if distance (entPosition a) (plPos player)
                                                  < distance (entPosition b) (plPos player)
                                                then a else b) wolves
                      eid = entId closest
                      tag = entTag closest
                  case selectedItem inv of
                    -- Tame wild wolf with bone (33% chance)
                    Just (ItemStack (MaterialItem Bone) _) | tag == "Wolf" -> do
                      roll <- randomRIO (1 :: Int, 3)
                      -- Consume 1 bone
                      let (inv', _) = removeItem inv (MaterialItem Bone) 1
                      writeIORef inventoryRef inv'
                      if roll == 1
                        then do
                          updateEntity entityWorld eid (\e -> e { entTag = "TamedWolf" })
                          putStrLn "Wolf tamed!"
                        else
                          putStrLn "Wolf not interested... (taming failed)"
                    -- Heal tamed wolf with food
                    Just (ItemStack (FoodItem ft) _) | tag `elem` ["TamedWolf", "TamedWolfSitting"] -> do
                      mEnt <- getEntity entityWorld eid
                      case mEnt of
                        Just ent | entHealth ent < entMaxHealth ent -> do
                          updateEntity entityWorld eid (\e -> e { entHealth = min (entMaxHealth e) (entHealth e + 2) })
                          let (inv', _) = removeItem inv (FoodItem ft) 1
                          writeIORef inventoryRef inv'
                          putStrLn "Healed tamed wolf!"
                        _ -> pure ()
                    -- Toggle sit for tamed wolf (no item needed or non-bone/food item)
                    _ | tag == "TamedWolf" -> do
                          updateEntity entityWorld eid (\e -> e { entTag = "TamedWolfSitting" })
                          modifyIORef' aiStatesRef (HM.insert eid (AIIdle 999999))
                          putStrLn "Wolf is now sitting."
                      | tag == "TamedWolfSitting" -> do
                          updateEntity entityWorld eid (\e -> e { entTag = "TamedWolf" })
                          modifyIORef' aiStatesRef (HM.insert eid (AIIdle 1.0))
                          putStrLn "Wolf is now following."
                      | otherwise -> pure ()

            -- RMB: fishing rod cast/reel
            when (button == GLFW.MouseButton'2) $ do
              inv <- readIORef inventoryRef
              case selectedItem inv of
                Just (ItemStack (FishingRodItem _) _) -> do
                  mFishing <- readIORef fishingStateRef
                  case mFishing of
                    Just (_, _, ready) -> do
                      -- Reel in
                      when ready $ do
                        -- Catch a random item
                        roll <- randomRIO (1 :: Int, 100)
                        let catchItem
                              | roll <= 80 = FoodItem RawFish
                              | roll <= 95 = FoodItem RawSalmon
                              | otherwise  = MaterialItem Bone
                        modifyIORef' inventoryRef (\i -> fst $ addItem i catchItem 1)
                        putStrLn $ "Caught: " ++ show catchItem
                      -- Re-read inventory after potential modification, then consume durability
                      inv' <- readIORef inventoryRef
                      let sel = invSelected inv'
                      case getSlot inv' sel of
                        Just (ItemStack (FishingRodItem dur) 1)
                          | dur <= 1  -> writeIORef inventoryRef (setSlot inv' sel Nothing)
                          | otherwise -> writeIORef inventoryRef (setSlot inv' sel (Just (ItemStack (FishingRodItem (dur - 1)) 1)))
                        _ -> pure ()
                      writeIORef fishingStateRef Nothing
                    Nothing -> do
                      -- Cast: raycast to find water surface
                      let waterQueryCb bx' by' bz' = do
                            bt' <- worldGetBlock world (V3 bx' by' bz')
                            pure (bt' == Water)
                      mWaterHit <- raycastBlock waterQueryCb eyePos dir maxReach
                      case mWaterHit of
                        Just waterHit -> do
                          let V3 wx wy wz = rhBlockPos waterHit
                              bobberPos = V3 (fromIntegral wx + 0.5) (fromIntegral wy + 1.0) (fromIntegral wz + 0.5)
                          timer <- randomRIO (5.0, 30.0 :: Float)
                          writeIORef fishingStateRef (Just (bobberPos, timer, False))
                          putStrLn $ "Cast fishing line! Wait time: " ++ show (round timer :: Int) ++ "s"
                        Nothing -> pure ()
                _ -> pure ()

            -- RMB: interact with boat entity (ride)
            when (button == GLFW.MouseButton'2) $ do
              riding <- readIORef ridingEntityRef
              case riding of
                Just _ -> pure ()
                Nothing -> do
                  nearbyBoats <- entitiesInRange entityWorld (plPos player) 3.0
                  let boats = filter (\e -> entTag e == "Boat") nearbyBoats
                  case boats of
                    (_:_) -> do
                      let closestBoat = closestTo (plPos player) boats
                      writeIORef ridingEntityRef (Just (entId closestBoat))
                      putStrLn $ "Mounted boat (entity " ++ show (entId closestBoat) ++ ")"
                    [] -> pure ()

            -- RMB: mount nearby minecart (when not already riding)
            when (button == GLFW.MouseButton'2) $ do
              riding <- readIORef ridingEntityRef
              case riding of
                Just _ -> pure ()  -- already riding
                Nothing -> do
                  nearby <- entitiesInRange entityWorld (plPos player) 3.0
                  let carts = filter (\e -> entTag e == "Minecart" && entAlive e) nearby
                  case carts of
                    [] -> pure ()
                    _  -> do
                      let closest = closestTo (plPos player) carts
                      writeIORef ridingEntityRef (Just (entId closest))
                      putStrLn $ "Mounted minecart " ++ show (entId closest)

            -- LMB: break minecart entity (when not riding it)
            when (button == GLFW.MouseButton'1) $ do
              riding <- readIORef ridingEntityRef
              nearby <- entitiesInRange entityWorld (plPos player) 3.0
              let carts = filter (\e -> entTag e == "Minecart" && entAlive e) nearby
              case carts of
                [] -> pure ()
                _  -> do
                  let closest = closestTo (plPos player) carts
                  when (riding /= Just (entId closest)) $ do
                    destroyEntity entityWorld (entId closest)
                    inv <- readIORef inventoryRef
                    let (inv', _) = addItem inv MinecartItem 1
                    writeIORef inventoryRef inv'
                    putStrLn $ "Broke minecart " ++ show (entId closest)

            mHit <- raycastBlock blockQueryCb eyePos dir maxReach
            case mHit of
              Nothing -> pure ()
              Just hit -> do
                let V3 bx by bz = rhBlockPos hit
                case button of
                  GLFW.MouseButton'1 -> do  -- Left click: start mining
                    writeIORef miningRef (Just (V3 bx by bz, 0.0))
                  GLFW.MouseButton'2 -> do  -- Right click: interact or place
                    hitBlock <- worldGetBlock world (V3 bx by bz)
                    let doorToggle = case hitBlock of
                          OakDoorClosed -> Just OakDoorOpen
                          OakDoorOpen   -> Just OakDoorClosed
                          FenceGateClosed -> Just FenceGateOpen
                          FenceGateOpen   -> Just FenceGateClosed
                          TrapdoorClosed -> Just TrapdoorOpen
                          TrapdoorOpen   -> Just TrapdoorClosed
                          _             -> Nothing
                    case doorToggle of
                      Just newDoor -> do
                        worldSetBlock world (V3 bx by bz) newDoor
                        rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
                        playSound soundSystem SndDoorOpen
                      Nothing -> case hitBlock of
                        Bed -> do
                          dnc <- readIORef dayNightRef
                          if isNight dnc
                            then do
                              -- Check for hostile mobs within 8 blocks
                              let bedPos = fmap fromIntegral (V3 bx by bz) + V3 0.5 1.0 0.5
                              nearbyEnts <- entitiesInRange entityWorld bedPos 8.0
                              let hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"]
                                  hasHostile = any (\e -> entTag e `elem` hostileTags) nearbyEnts
                              if hasHostile
                                then writeIORef sleepMessageRef (Just (3.0, "YOU MAY NOT REST NOW, THERE ARE MONSTERS NEARBY"))
                                else do
                                  writeIORef spawnPointRef bedPos
                                  modifyIORef' dayNightRef (\d -> d { dncTime = 0.25, dncDayCount = dncDayCount d + 1 })
                            else
                              writeIORef sleepMessageRef (Just (3.0, "YOU CAN ONLY SLEEP AT NIGHT"))
                        Chest -> do
                          let chestPos = V3 bx by bz
                          mExisting <- getChestInventory blockEntityMapRef chestPos
                          case mExisting of
                            Nothing -> setChestInventory blockEntityMapRef chestPos emptyChestInventory
                            Just _  -> pure ()
                          writeIORef chestPosRef (Just chestPos)
                          writeIORef gameModeRef ChestOpen
                          GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                          writeIORef lastCursorRef Nothing
                        Dispenser -> do
                          let dPos = V3 bx by bz
                          mExisting <- getDispenserInventory blockEntityMapRef dPos
                          case mExisting of
                            Nothing -> setDispenserInventory blockEntityMapRef dPos emptyDispenserInventory
                            Just _  -> pure ()
                          writeIORef dispenserPosRef (Just dPos)
                          writeIORef gameModeRef DispenserOpen
                          GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                          writeIORef lastCursorRef Nothing
                        CraftingTable -> do
                          writeIORef gameModeRef CraftingOpen
                          writeIORef craftingGridRef (emptyCraftingGrid 3)
                          GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                          writeIORef lastCursorRef Nothing
                        EnchantingTable -> do
                          writeIORef gameModeRef EnchantingOpen
                          writeIORef enchantItemRef Nothing
                          -- Generate enchantment options with a random seed
                          xpLvl <- readIORef playerXPRef
                          eSeed <- randomRIO (0, 999999 :: Int)
                          writeIORef enchantOptionsRef (generateEnchantments xpLvl eSeed)
                          GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                          writeIORef lastCursorRef Nothing
                        Furnace -> do
                          -- Load existing furnace state or create new one
                          let fPos = V3 bx by bz
                          mExisting <- getFurnaceState blockEntityMapRef fPos
                          case mExisting of
                            Just fs -> writeIORef furnaceStateRef fs
                            Nothing -> do
                              writeIORef furnaceStateRef newFurnaceState
                              setFurnaceState blockEntityMapRef fPos newFurnaceState
                          writeIORef gameModeRef FurnaceOpen
                          writeIORef furnacePosRef (Just fPos)
                          GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                          writeIORef lastCursorRef Nothing
                        Lever -> do
                          -- Toggle lever: update redstone state
                          rsState <- readIORef redstoneStateRef
                          let pos = V3 bx by bz
                          currentPower <- getPower rsState pos
                          let newPower = if currentPower > 0 then 0 else 15
                          setPower rsState pos newPower
                          propagateRedstone rsState [(pos, newPower)]
                          -- Update iron doors adjacent to redstone-powered positions
                          updateIronDoors world rsState pos physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef
                          -- Update pistons adjacent to redstone-powered positions
                          updatePistons world rsState pos physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef
                          -- Dispense items from adjacent dispensers when powered
                          when (newPower > 0) $
                            dispenseFromDispensers world rsState blockEntityMapRef droppedItems pos
                          rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
                        _ -> do
                          -- Place painting on wall face when hand is empty
                          inv <- readIORef inventoryRef
                          let faceNorm = rhFaceNormal hit
                              isWallFace = faceNorm /= V3 0 1 0 && faceNorm /= V3 0 (-1) 0
                          case selectedItem inv of
                            Nothing -> when isWallFace $ do
                              let V3 fnx _fny fnz = faceNorm
                                  paintingPos = fmap fromIntegral (V3 bx by bz)
                                              + V3 (fromIntegral fnx * 0.5) 0 (fromIntegral fnz * 0.5)
                                  -- Encode wall direction as yaw: N=0, E=90, S=180, W=270
                                  paintingYaw
                                    | fnz == -1 = 0    -- North
                                    | fnx == 1  = 90   -- East
                                    | fnz == 1  = 180  -- South
                                    | fnx == -1 = 270  -- West
                                    | otherwise = 0
                              eid <- spawnEntity entityWorld paintingPos 1.0 "Painting"
                              updateEntity entityWorld eid (\e -> e { entYaw = paintingYaw })
                              putStrLn $ "Placed painting at " ++ show paintingPos
                            Just (ItemStack (FoodItem _) _) -> pure ()
                            Just (ItemStack GlassBottleItem _) -> do
                              -- Fill glass bottle with water: check hit block or adjacent block
                              let V3 nx ny nz = rhFaceNormal hit
                                  adjPos = V3 (bx + nx) (by + ny) (bz + nz)
                              adjBlock <- worldGetBlock world adjPos
                              let isNearWater = hitBlock == Water || adjBlock == Water
                              when isNearWater $ do
                                let (inv', _) = removeItem inv GlassBottleItem 1
                                    (inv'', _) = addItem inv' (PotionItem WaterBottle) 1
                                writeIORef inventoryRef inv''
                                putStrLn "Filled glass bottle with water"
                            Just (ItemStack (PotionItem _) _) -> pure ()  -- handled in pre-raycast
                            Just (ItemStack MinecartItem _) -> do
                              -- Place minecart on rail
                              when (hitBlock == Rail) $ do
                                let sel = invSelected inv
                                    cartPos = V3 (fromIntegral bx + 0.5) (fromIntegral by + 0.25) (fromIntegral bz + 0.5) :: V3 Float
                                _ <- spawnEntity entityWorld cartPos 1.0 "Minecart"
                                case getSlot inv sel of
                                  Just (ItemStack MinecartItem cnt)
                                    | cnt <= 1  -> writeIORef inventoryRef (setSlot inv sel Nothing)
                                    | otherwise -> writeIORef inventoryRef (setSlot inv sel (Just (ItemStack MinecartItem (cnt - 1))))
                                  _ -> pure ()
                                putStrLn $ "Placed minecart at " ++ show cartPos
                            Just (ItemStack (FlintAndSteelItem dur) _) -> do
                              let sel = invSelected inv
                                  consumeDurability =
                                    if dur <= 1
                                      then writeIORef inventoryRef (setSlot inv sel Nothing)
                                      else writeIORef inventoryRef (setSlot inv sel (Just (ItemStack (FlintAndSteelItem (dur - 1)) 1)))
                              if hitBlock == TNT
                                then do
                                  worldSetBlock world (V3 bx by bz) Air
                                  explodeAt world (fmap fromIntegral (V3 bx by bz) + V3 0.5 0.5 0.5) 3 droppedItems
                                  playSound soundSystem SndExplosion
                                  rebuildExplosionChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz 3
                                  consumeDurability
                                  putStrLn $ "Ignited TNT at " ++ show (V3 bx by bz)
                                else do
                                  let V3 nx ny nz = rhFaceNormal hit
                                      firePos = V3 (bx + nx) (by + ny) (bz + nz)
                                      V3 _ fireY _ = firePos
                                  when (fireY >= 0 && fireY < chunkHeight) $ do
                                    existing <- worldGetBlock world firePos
                                    when (existing == Air) $ do
                                      worldSetBlock world firePos Fire
                                      consumeDurability
                                      let V3 fpx _ fpz = firePos
                                      rebuildChunkWithNeighbors world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef fpx fpz
                                      putStrLn $ "Placed fire at " ++ show firePos
                            Just (ItemStack BoatItem _) -> do
                              when (hitBlock == Water) $ do
                                let sel = invSelected inv
                                    boatPos = V3 (fromIntegral bx + 0.5) (fromIntegral by + 1.0) (fromIntegral bz + 0.5) :: V3 Float
                                case getSlot inv sel of
                                  Just (ItemStack BoatItem cnt) ->
                                    if cnt <= 1
                                      then writeIORef inventoryRef (setSlot inv sel Nothing)
                                      else writeIORef inventoryRef (setSlot inv sel (Just (ItemStack BoatItem (cnt - 1))))
                                  _ -> pure ()
                                _ <- spawnEntity entityWorld boatPos 1.0 "Boat"
                                putStrLn $ "Placed boat at " ++ show boatPos
                            Just (ItemStack item _) -> case itemToBlock item of
                              Nothing -> pure ()
                              Just bt -> do
                                let V3 nx ny nz = rhFaceNormal hit
                                    placePos = V3 (bx + nx) (by + ny) (bz + nz)
                                    V3 placeX placeY placeZ = placePos
                                    sel = invSelected inv
                                -- Sugar cane placement validation
                                canPlace <- if bt == SugarCane
                                  then do
                                    belowBlock <- worldGetBlock world (placePos - V3 0 1 0)
                                    if belowBlock == SugarCane
                                      then pure True
                                      else if belowBlock == Sand || belowBlock == Dirt
                                        then do
                                          let ns = [V3 (placeX-1) (placeY-1) placeZ, V3 (placeX+1) (placeY-1) placeZ,
                                                    V3 placeX (placeY-1) (placeZ-1), V3 placeX (placeY-1) (placeZ+1)]
                                          or <$> mapM (\p -> (== Water) <$> worldGetBlock world p) ns
                                        else pure False
                                  -- Cactus placement validation: only on Sand, with 4-side air clearance
                                  else if bt == Cactus
                                    then do
                                      below <- worldGetBlock world (V3 placeX (placeY - 1) placeZ)
                                      n' <- worldGetBlock world (V3 (placeX + 1) placeY placeZ)
                                      s' <- worldGetBlock world (V3 (placeX - 1) placeY placeZ)
                                      e' <- worldGetBlock world (V3 placeX placeY (placeZ + 1))
                                      w' <- worldGetBlock world (V3 placeX placeY (placeZ - 1))
                                      pure ((below == Sand || below == Cactus) && n' == Air && s' == Air && e' == Air && w' == Air)
                                  -- Rail placement validation: only on solid block
                                  else if bt == Rail
                                    then do
                                      below <- worldGetBlock world (V3 placeX (placeY - 1) placeZ)
                                      pure (isSolid below)
                                  else pure True
                                -- Height limit: only place blocks in valid range [0, chunkHeight)
                                when (placeY >= 0 && placeY < chunkHeight && canPlace) $ do
                                    -- Consume from selected slot directly
                                    case getSlot inv sel of
                                      Just (ItemStack si cnt) | si == item ->
                                        if cnt <= 1
                                          then writeIORef inventoryRef (setSlot inv sel Nothing)
                                          else writeIORef inventoryRef (setSlot inv sel (Just (ItemStack si (cnt - 1))))
                                      _ -> pure ()
                                    worldSetBlock world placePos bt
                                    playSound soundSystem SndBlockPlace
                                    let V3 px' _ pz' = placePos
                                    when (isGravityAffected bt) $
                                      void $ settleGravityBlock world placePos
                                    if bpLightEmit (blockProperties bt) > 0
                                      then rebuildChunkWithNeighbors world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef px' pz'
                                      else rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef px' pz'
                  _ -> pure ()

    -- Key callback
    GLFW.setKeyCallback (whWindow wh) $ Just $ \_win key _scancode action _mods ->
      when (action == GLFW.KeyState'Pressed) $ do
        mode <- readIORef gameModeRef
        case mode of
          MainMenu -> case key of
            GLFW.Key'Escape -> GLFW.setWindowShouldClose (whWindow wh) True
            _ -> pure ()
          Playing -> case key of
            GLFW.Key'Escape -> do
              writeIORef gameModeRef Paused
              GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
              writeIORef lastCursorRef Nothing
            GLFW.Key'F ->
              modifyIORef' inputRef $ \inp -> inp { piToggleFly = True }
            GLFW.Key'F3 ->
              modifyIORef' debugOverlayRef not
            GLFW.Key'E -> do
              writeIORef gameModeRef InventoryOpen
              writeIORef craftingGridRef (emptyCraftingGrid 2)  -- 2x2 for inventory crafting
              GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
              writeIORef lastCursorRef Nothing
            GLFW.Key'1 -> selectSlot 0
            GLFW.Key'2 -> selectSlot 1
            GLFW.Key'3 -> selectSlot 2
            GLFW.Key'4 -> selectSlot 3
            GLFW.Key'5 -> selectSlot 4
            GLFW.Key'6 -> selectSlot 5
            GLFW.Key'7 -> selectSlot 6
            GLFW.Key'8 -> selectSlot 7
            GLFW.Key'9 -> selectSlot 8
            GLFW.Key'F5 -> do
              player <- readIORef playerRef
              inv <- readIORef inventoryRef
              dayNight <- readIORef dayNightRef
              sd <- readIORef saveDirRef
              savePlayer sd (buildSaveData player inv dayNight)
              saveWorld sd world
              putStrLn "Quick-saved!"
            GLFW.Key'F9 -> do
              sd <- readIORef saveDirRef
              mData <- loadPlayer sd
              case mData of
                Just savedData -> do
                  restoreFromSave playerRef inventoryRef dayNightRef savedData
                  putStrLn "Quick-loaded!"
                Nothing -> putStrLn "No save found."
            _ -> pure ()
          Paused -> case key of
            GLFW.Key'Escape -> do
              -- Resume game
              writeIORef gameModeRef Playing
              GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
              writeIORef lastCursorRef Nothing
            _ -> pure ()
          DeathScreen -> pure ()  -- no keyboard input on death screen
          _ -> case key of  -- InventoryOpen, CraftingOpen, ChestOpen, or FurnaceOpen
            GLFW.Key'Escape -> closeUIScreen
            GLFW.Key'E      -> closeUIScreen
            _ -> pure ()
            where
              closeUIScreen = do
                curMode <- readIORef gameModeRef
                -- Return cursor item to inventory
                mCursor <- readIORef cursorItemRef
                case mCursor of
                  Just (ItemStack item cnt) -> do
                    modifyIORef' inventoryRef (\inv -> fst $ addItem inv item cnt)
                    writeIORef cursorItemRef Nothing
                  Nothing -> pure ()
                -- Return crafting grid items to inventory when closing crafting screen
                when (curMode == CraftingOpen) $ do
                  grid <- readIORef craftingGridRef
                  forM_ [(r,c) | r <- [0..2], c <- [0..2]] $ \(r,c) ->
                    case getCraftingSlot grid r c of
                      Just item -> modifyIORef' inventoryRef (\inv -> fst $ addItem inv item 1)
                      Nothing -> pure ()
                  writeIORef craftingGridRef (emptyCraftingGrid 3)
                -- Clear chest reference when closing chest
                when (curMode == ChestOpen) $
                  writeIORef chestPosRef Nothing
                -- Save furnace state back to block entity when closing
                when (curMode == FurnaceOpen) $ do
                  syncFurnaceToMap
                  writeIORef furnacePosRef Nothing
                -- Clear dispenser reference when closing dispenser
                when (curMode == DispenserOpen) $
                  writeIORef dispenserPosRef Nothing
                -- Return enchanting item to inventory when closing
                when (curMode == EnchantingOpen) $ do
                  mEItem <- readIORef enchantItemRef
                  case mEItem of
                    Just (ItemStack eItem eCnt) -> do
                      modifyIORef' inventoryRef (\inv -> fst $ addItem inv eItem eCnt)
                      writeIORef enchantItemRef Nothing
                    Nothing -> pure ()
                  clearEnchantments enchantMapRef 0
                -- Switch back to playing
                writeIORef gameModeRef Playing
                GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
                writeIORef lastCursorRef Nothing

    -- Initialize last-time ref from GLFW clock (overrides default 0)
    writeIORef lastTimeRef =<< maybe 0 id <$> GLFW.getTime

    -- Block queries for physics
    let mkBlockQuery :: (BlockType -> Bool) -> BlockQuery
        mkBlockQuery predicate bx by bz = do
          bt <- worldGetBlock world (V3 bx by bz)
          pure (predicate bt)

    let blockQuery :: BlockHeightQuery
        blockQuery bx by bz = do
          bt <- worldGetBlock world (V3 bx by bz)
          pure (if World.Block.isSolid bt then blockCollisionHeight bt else 0.0)

    let solidQuery :: BlockQuery
        solidQuery = mkBlockQuery World.Block.isSolid

    let waterQuery :: BlockQuery
        waterQuery = mkBlockQuery (== Water)

    let ladderQuery :: BlockQuery
        ladderQuery = mkBlockQuery (== Ladder)

    -- Main loop (wrapped in finally for exception-safe cleanup)
    putStrLn "Controls: WASD=move, Mouse=look, Space=jump, F=fly, 1-9=hotbar, LMB=break, RMB=place, ESC=quit"
    let loop = do
          shouldClose <- windowShouldClose wh
          unless shouldClose $ do
            pollWindowEvents

            -- Delta time
            now <- maybe 0 id <$> GLFW.getTime
            lastTime <- readIORef lastTimeRef
            writeIORef lastTimeRef now
            let rawDt = realToFrac (now - lastTime) :: Float
                dt = min rawDt 0.25  -- cap to avoid spiral of death

            -- FPS counter (update once per second)
            modifyIORef' fpsCounterRef (+ 1)
            modifyIORef' fpsTimerRef (+ dt)
            fpsTimer <- readIORef fpsTimerRef
            when (fpsTimer >= 1.0) $ do
              fpsCount <- readIORef fpsCounterRef
              writeIORef fpsDisplayRef fpsCount
              writeIORef fpsCounterRef 0
              writeIORef fpsTimerRef (fpsTimer - 1.0)

            -- Skip gameplay input when UI is open
            gameMode <- readIORef gameModeRef

            -- Read keyboard state
            let win = whWindow wh
            wDown <- isKeyDown win GLFW.Key'W
            sDown <- isKeyDown win GLFW.Key'S
            aDown <- isKeyDown win GLFW.Key'A
            dDown <- isKeyDown win GLFW.Key'D
            spaceDown <- isKeyDown win GLFW.Key'Space
            shiftDown <- isKeyDown win GLFW.Key'LeftShift
            ctrlDown  <- isKeyDown win GLFW.Key'LeftControl

            -- Build input
            baseInput <- readIORef inputRef
            let input = baseInput
                  { piForward  = wDown
                  , piBackward = sDown
                  , piLeft     = aDown
                  , piRight    = dDown
                  , piJump     = spaceDown
                  , piSneak    = shiftDown
                  , piSprint   = ctrlDown
                  }

            -- Fixed timestep physics (skip when UI is open)
            accum <- readIORef accumRef
            let accum' = accum + dt
            healthBefore <- plHealth <$> readIORef playerRef
            when (gameMode == Playing) $ do
              -- Apply speed buff: force sprint when speed potion is active
              speedBuff <- readIORef speedBuffTimerRef
              let input' = if speedBuff > 0
                            then input { piSprint = True }
                            else input
              playerLoop input' blockQuery waterQuery ladderQuery accumRef accum' playerRef
            healthAfter <- plHealth <$> readIORef playerRef

            -- Boat riding: move boat and snap player to boat position
            when (gameMode == Playing) $ do
              mRiding <- readIORef ridingEntityRef
              case mRiding of
                Nothing -> pure ()
                Just boatEid -> do
                  mBoat <- getEntity entityWorld boatEid
                  case mBoat of
                    Nothing -> writeIORef ridingEntityRef Nothing
                    Just boat -> do
                      if piSneak input
                        then do
                          let V3 bpx bpy bpz = entPosition boat
                              dismountPos = V3 (bpx + 1.0) bpy bpz
                          modifyIORef' playerRef (\p -> p { plPos = dismountPos, plVelocity = V3 0 0 0 })
                          writeIORef ridingEntityRef Nothing
                          putStrLn "Dismounted boat"
                        else do
                          player' <- readIORef playerRef
                          let yawR   = plYaw player' * pi / 180
                              front  = normalize $ V3 (sin yawR) 0 (cos yawR)
                              right  = normalize $ front `cross` V3 0 1 0
                              boatSpeed = 5.0 :: Float
                              moveDir = V3 0 0 0
                                + (if piForward  input then front  else V3 0 0 0)
                                + (if piBackward input then -front else V3 0 0 0)
                                + (if piLeft     input then -right else V3 0 0 0)
                                + (if piRight    input then right  else V3 0 0 0)
                              normalizedDir = if norm moveDir > 0.001 then normalize moveDir else V3 0 0 0
                              hVel = normalizedDir ^* boatSpeed
                              V3 hvx _ hvz = hVel
                              V3 bpx bpy bpz = entPosition boat
                              newBpx = bpx + hvx * dt
                              newBpz = bpz + hvz * dt
                          let blockX = floor newBpx :: Int
                              blockY = floor bpy :: Int
                              blockZ = floor newBpz :: Int
                          blockBelow <- worldGetBlock world (V3 blockX (blockY - 1) blockZ)
                          blockAt    <- worldGetBlock world (V3 blockX blockY blockZ)
                          let newBpy
                                | blockAt == Water    = fromIntegral blockY + 1.0
                                | blockBelow == Water = bpy
                                | otherwise           = bpy - 9.8 * dt
                              newBoatPos = V3 newBpx newBpy newBpz
                          updateEntity entityWorld boatEid (\e -> e { entPosition = newBoatPos })
                          modifyIORef' playerRef (\p -> p { plPos = newBoatPos, plVelocity = V3 0 0 0, plFallDist = 0 })

            -- Flash screen on fall/drowning/starvation/void damage
            when (healthAfter < healthBefore && healthAfter > 0)
              triggerDamageFlash

            -- Minecart riding logic
            when (gameMode == Playing) $ do
              riding <- readIORef ridingEntityRef
              case riding of
                Nothing -> pure ()
                Just cartId -> do
                  mCart <- getEntity entityWorld cartId
                  case mCart of
                    Nothing -> writeIORef ridingEntityRef Nothing  -- cart was destroyed
                    Just cart -> do
                      -- Sneak to dismount
                      if piSneak input
                        then do
                          writeIORef ridingEntityRef Nothing
                          let V3 cx cy cz = entPosition cart
                          modifyIORef' playerRef (\p -> p { plPos = V3 cx (cy + 0.5) cz })
                          putStrLn "Dismounted minecart"
                        else do
                          let V3 cx cy cz = entPosition cart
                              cartBlockX = floor cx :: Int
                              cartBlockY = floor cy :: Int
                              cartBlockZ = floor cz :: Int
                              maxCartSpeed = 8.0 :: Float
                              -- Determine speed from W/S input
                              accel = (if piForward input then 1.0 else 0.0)
                                    + (if piBackward input then (-1.0) else 0.0)
                          let V3 vx _vy vz = entVelocity cart
                              speed = sqrt (vx * vx + vz * vz) * (if vx + vz >= 0 then 1 else (-1))
                          -- Check adjacent rails to determine movement direction
                          let checkRail dx dz = do
                                let pos = V3 (cartBlockX + dx) cartBlockY (cartBlockZ + dz)
                                b <- worldGetBlock world pos
                                pure (b == Rail)
                          hasN <- checkRail 0 1
                          hasS <- checkRail 0 (-1)
                          hasE <- checkRail 1 0
                          hasW <- checkRail (-1) 0
                          -- Determine rail direction: prefer Z-axis, then X-axis
                          let (dirX, dirZ) = if hasN || hasS then (0.0 :: Float, 1.0 :: Float)
                                             else if hasE || hasW then (1.0, 0.0)
                                             else (0.0, 0.0)  -- no adjacent rails, don't move
                          -- Apply speed with friction
                          let newSpeed = clamp (-maxCartSpeed) maxCartSpeed ((speed + accel * dt * 4.0) * (1.0 - dt * 0.5))
                              newVx = dirX * newSpeed
                              newVz = dirZ * newSpeed
                              newCx = cx + newVx * dt
                              newCz = cz + newVz * dt
                          -- Check if new position is on a rail
                          let newBlockX = floor newCx :: Int
                              newBlockZ = floor newCz :: Int
                          onRail <- worldGetBlock world (V3 newBlockX cartBlockY newBlockZ)
                          let (finalCx, finalCz, finalVx, finalVz) =
                                if onRail == Rail
                                  then (newCx, newCz, newVx, newVz)
                                  else (cx, cz, 0, 0)  -- stop at rail edge
                              newCartPos = V3 finalCx cy finalCz
                          updateEntity entityWorld cartId (\e -> e
                            { entPosition = newCartPos
                            , entVelocity = V3 finalVx 0 finalVz
                            })
                          -- Snap player to cart position
                          modifyIORef' playerRef (\p -> p
                            { plPos = V3 finalCx (cy + 0.5) finalCz
                            , plVelocity = V3 0 0 0
                            })

            -- Cactus contact damage: 1 HP per second while player AABB overlaps any Cactus
            when (gameMode == Playing) $ do
              player' <- readIORef playerRef
              let V3 px py pz = plPos player'
                  -- Player AABB: 0.6 wide, 1.8 tall (centered on x/z, feet at py)
                  minX = floor (px - 0.3) :: Int
                  maxX = floor (px + 0.3) :: Int
                  minY = floor py :: Int
                  maxY = floor (py + 1.8) :: Int
                  minZ = floor (pz - 0.3) :: Int
                  maxZ = floor (pz + 0.3) :: Int
                  -- Early-exit scan: stop as soon as we find one Cactus
                  checkCactus [] = pure False
                  checkCactus ((bx,by,bz):rest) = do
                    bt <- worldGetBlock world (V3 bx by bz)
                    if bt == Cactus then pure True else checkCactus rest
                  coords = [(bx,by,bz) | bx <- [minX..maxX], by <- [minY..maxY], bz <- [minZ..maxZ]]
              touchingCactus <- checkCactus coords
              if touchingCactus
                then do
                  timer <- readIORef cactusDamageTimerRef
                  let timer' = timer + dt
                  if timer' >= 1.0
                    then do
                      modifyIORef' playerRef (damagePlayer 1)
                      writeIORef cactusDamageTimerRef (timer' - 1.0)
                      triggerDamageFlash
                    else writeIORef cactusDamageTimerRef timer'
                else writeIORef cactusDamageTimerRef 0.0

            -- Check for player death → show death screen
            do p <- readIORef playerRef
               when (isPlayerDead p) $ do
                 writeIORef gameModeRef DeathScreen
                 GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                 writeIORef lastCursorRef Nothing

            let physicsTickRan = accum' >= tickRate

            -- Clear per-frame mouse movement, but keep one-shot toggles queued
            -- until a physics tick has consumed them.
            writeIORef inputRef (endFrameInput physicsTickRan baseInput)

            -- Mining tick: advance progress while LMB held (only when playing)
            lmbHeld <- readIORef lmbHeldRef
            when (lmbHeld && gameMode == Playing) $ do
              mining <- readIORef miningRef
              case mining of
                Nothing -> pure ()
                Just (blockPos, progress) -> do
                  let V3 bx by bz = blockPos
                  bt <- worldGetBlock world blockPos
                  let hardness = bpHardness (blockProperties bt)
                  if hardness <= 0
                    then writeIORef miningRef Nothing  -- unbreakable or air
                    else do
                      -- Tool speed multiplier
                      inv <- readIORef inventoryRef
                      let toolSpeed = case selectedItem inv of
                            Just (ItemStack (ShearsItem _) _)
                              | isLeafBlock bt -> 15.0  -- shears mine leaves very fast
                            Just (ItemStack (ToolItem tt tm _) _)
                              | blockPreferredTool bt == Just tt -> toolMiningSpeed tt tm
                            _ -> 1.0  -- hand speed
                          progressPerSec = toolSpeed / hardness
                          newProgress = progress + progressPerSec * dt
                      if newProgress >= 1.0
                        then do
                          -- Block is broken!
                          worldSetBlock world blockPos Air
                          -- Spawn visual break particles
                          let blockColor = itemColor (BlockItem bt)
                              blockCenter = fmap fromIntegral blockPos :: V3 Float
                          spawnBlockBreakParticles particleSystemRef blockCenter blockColor
                          when (bt == Water || bt == Lava) $
                            removeFluid fluidState world blockPos
                          -- Drop items as entities in the world
                          -- Shears override: mining leaves with shears drops the leaf block
                          let drops = case selectedItem inv of
                                Just (ItemStack (ShearsItem _) _)
                                  | isLeafBlock bt -> [(BlockItem bt, 1)]
                                _ -> blockDrops bt
                              V3 bxf byf bzf = fmap fromIntegral blockPos :: V3 Float
                          mapM_ (\(item, cnt) -> spawnDrop droppedItems item cnt (V3 bxf byf bzf)) drops
                          -- If it's a chest, also drop all stored items and remove block entity
                          when (bt == Chest) $ do
                            mBE <- removeBlockEntity blockEntityMapRef blockPos
                            case mBE of
                              Just (ChestData chestInv) ->
                                forM_ [0 .. chestSlots - 1] $ \si ->
                                  case getChestSlot chestInv si of
                                    Just (ItemStack item cnt) ->
                                      spawnDrop droppedItems item cnt (V3 bxf byf bzf)
                                    Nothing -> pure ()
                              _ -> pure ()
                          -- If it's a furnace, drop all stored items and remove block entity
                          when (bt == Furnace) $ do
                            mBE <- removeBlockEntity blockEntityMapRef blockPos
                            case mBE of
                              Just (FurnaceData fs) -> do
                                let dropStack mStack = case mStack of
                                      Just (ItemStack item cnt) ->
                                        spawnDrop droppedItems item cnt (V3 bxf byf bzf)
                                      Nothing -> pure ()
                                dropStack (getFurnaceInput fs)
                                dropStack (getFurnaceFuel fs)
                                dropStack (getFurnaceOutput fs)
                              _ -> pure ()
                          -- If it's a dispenser, drop all stored items and remove block entity
                          when (bt == Dispenser) $ do
                            mBE <- removeBlockEntity blockEntityMapRef blockPos
                            case mBE of
                              Just (DispenserData dispInv) ->
                                forM_ [0 .. dispenserSlots - 1] $ \si ->
                                  case getDispenserSlot dispInv si of
                                    Just (ItemStack item cnt) ->
                                      spawnDrop droppedItems item cnt (V3 bxf byf bzf)
                                    Nothing -> pure ()
                              _ -> pure ()
                          -- Consume tool durability (regular tools and shears)
                          inv' <- readIORef inventoryRef
                          let inv'' = case getSlot inv' (invSelected inv') of
                                Just (ItemStack (ToolItem tt tm dur) 1)
                                  | dur <= 1  -> setSlot inv' (invSelected inv') Nothing
                                  | otherwise -> setSlot inv' (invSelected inv') (Just (ItemStack (ToolItem tt tm (dur - 1)) 1))
                                Just (ItemStack (ShearsItem dur) 1)
                                  | dur <= 1  -> setSlot inv' (invSelected inv') Nothing
                                  | otherwise -> setSlot inv' (invSelected inv') (Just (ItemStack (ShearsItem (dur - 1)) 1))
                                _ -> inv'
                          writeIORef inventoryRef inv''
                          putStrLn $ "Broke " ++ show bt ++ " at " ++ show blockPos
                          playSound soundSystem SndBlockBreak
                          -- Trigger falling blocks above the broken block
                          void $ triggerGravityAbove world blockPos
                          -- Sugar cane cascade: break all sugar cane above
                          when (bt == SugarCane) $ do
                            let cascadeBreak pos = do
                                  let abovePos = pos + V3 0 1 0
                                  above <- worldGetBlock world abovePos
                                  when (above == SugarCane) $ do
                                    let V3 abx aby abz = fmap fromIntegral abovePos :: V3 Float
                                    worldSetBlock world abovePos Air
                                    mapM_ (\(itm, cnt) -> spawnDrop droppedItems itm cnt (V3 abx aby abz)) (blockDrops SugarCane)
                                    cascadeBreak abovePos
                            cascadeBreak blockPos
                          -- Rail cascade: break rail above if block below it is removed
                          do let abovePos = blockPos + V3 0 1 0
                             above <- worldGetBlock world abovePos
                             when (above == Rail) $ do
                               let V3 abx aby abz = fmap fromIntegral abovePos :: V3 Float
                               worldSetBlock world abovePos Air
                               mapM_ (\(itm, cnt) -> spawnDrop droppedItems itm cnt (V3 abx aby abz)) (blockDrops Rail)
                          -- Destroy paintings attached to the broken block
                          do let blockCenter = fmap fromIntegral blockPos + V3 0.5 0.5 0.5 :: V3 Float
                             nearby <- entitiesInRange entityWorld blockCenter 1.5
                             forM_ (filter (\e -> entTag e == "Painting") nearby) $ \p -> do
                               destroyEntity entityWorld (entId p)
                               putStrLn $ "Painting at " ++ show (entPosition p) ++ " destroyed (wall broken)"
                          if bpLightEmit (blockProperties bt) > 0
                            then rebuildChunkWithNeighbors world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
                            else rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
                          writeIORef miningRef Nothing
                        else writeIORef miningRef (Just (blockPos, newProgress))

            -- Eating tick: advance eating timer while RMB held with food
            when (gameMode == Playing) $ do
              player' <- readIORef playerRef
              let timer = plEatingTimer player'
              when (timer > 0) $ do
                rmbHeld <- readIORef rmbHeldRef
                if not rmbHeld
                  then cancelEating
                  else do
                    let newTimer = timer - dt
                    if newTimer <= 0
                      then do
                        -- Eating complete: restore hunger, consume food item
                        inv <- readIORef inventoryRef
                        case selectedItem inv of
                          Just (ItemStack (FoodItem ft) _) -> do
                            let restore = foodHungerRestore ft
                                satRestore = foodSaturation ft
                                newHunger = min maxHunger (plHunger player' + restore)
                                (inv', _) = removeItem inv (FoodItem ft) 1
                            writeIORef inventoryRef inv'
                            modifyIORef' playerRef (\p -> p
                              { plHunger = newHunger
                              , plEatingTimer = 0.0
                              , plSaturation = min maxSaturation (plSaturation p + satRestore)
                              })
                            putStrLn $ "Ate " ++ show ft ++ ", restored " ++ show restore ++ " hunger"
                            playSound soundSystem SndEat
                          _ -> cancelEating
                      else modifyIORef' playerRef (\p -> p { plEatingTimer = newTimer })

            -- Fishing tick: decrement timer, set ready when expired
            when (gameMode == Playing) $ do
              mFishing <- readIORef fishingStateRef
              case mFishing of
                Just (pos, timer, False) -> do
                  let newTimer = timer - dt
                  if newTimer <= 0
                    then do
                      writeIORef fishingStateRef (Just (pos, 0, True))
                      putStrLn "Fish is biting! Reel in with RMB!"
                    else writeIORef fishingStateRef (Just (pos, newTimer, False))
                _ -> pure ()

            -- Tick potion effects (poison and speed buff)
            when (gameMode == Playing) $ do
              -- Poison: 2 HP over 5 seconds = 1 HP per 2.5 seconds
              -- Timer counts down from 5.0; deal 1 HP at 2.5s and 0.0s remaining
              poisonTimer <- readIORef poisonTimerRef
              when (poisonTimer > 0) $ do
                let newTimer = max 0 (poisonTimer - dt)
                    -- Damage at threshold crossings: 5.0->2.5 and 2.5->0.0
                    crossedMid = poisonTimer > 2.5 && newTimer <= 2.5
                    crossedEnd = newTimer <= 0
                    dmg = (if crossedMid then 1 else 0) + (if crossedEnd then 1 else 0) :: Int
                when (dmg > 0) $
                  modifyIORef' playerRef (\p -> p { plHealth = max 1 (plHealth p - dmg) })
                writeIORef poisonTimerRef newTimer
                when (newTimer <= 0) $ putStrLn "Poison effect expired"
              -- Speed buff: just decrement timer (actual speed change applied in physics)
              speedTimer <- readIORef speedBuffTimerRef
              when (speedTimer > 0) $ do
                let newTimer = max 0 (speedTimer - dt)
                writeIORef speedBuffTimerRef newTimer
                when (newTimer <= 0) $ putStrLn "Speed boost expired"

            -- Update day/night cycle
            modifyIORef' dayNightRef (updateDayNight dt)

            -- Update weather (rain/clear transitions)
            readIORef weatherRef >>= updateWeather dt >>= writeIORef weatherRef

            -- Tick ALL active furnaces (not just the open one)
            do activeFurnaces <- allFurnaceEntities blockEntityMapRef
               forM_ activeFurnaces $ \(pos, fs) -> do
                 let fs' = tickFurnace dt fs
                 when (fs' /= fs) $ setFurnaceState blockEntityMapRef pos fs'
               -- Sync the open furnace's IORef with the updated block entity
               when (gameMode == FurnaceOpen) syncFurnaceFromMap

            -- Tick fluid simulation every physics update
            tickFluids fluidState world

            -- Update dropped items (gravity, friction) and auto-collect nearby
            updateDroppedItems dt droppedItems

            -- Update particle effects
            tickParticles dt particleSystemRef
            do player' <- readIORef playerRef
               collected <- collectNearby droppedItems (plPos player') 1.5
               unless (null collected) $ do
                 playSound soundSystem SndItemPickup
                 inv' <- readIORef inventoryRef
                 let inv'' = foldl (\i (item, cnt) -> fst $ addItem i item cnt) inv' collected
                 writeIORef inventoryRef inv''

            -- Entity spawning (every ~2 seconds)
            frameIdx <- readIORef frameRef
            when (frameIdx `mod` 120 == 0) $ do
              player <- readIORef playerRef
              dayNight <- readIORef dayNightRef
              weather <- readIORef weatherRef
              _ <- trySpawnMobs defaultSpawnRules entityWorld dayNight weather solidQuery (plPos player) spawnRngRef
              pure ()

            -- Update mob AI (every 3 frames)
            when (frameIdx `mod` 3 == 0) $ do
              let aiDt = dt * 3  -- compensate for running every 3rd frame
              player <- readIORef playerRef
              ents <- livingEntities entityWorld
              aiStates <- readIORef aiStatesRef
              forM_ ents $ \ent -> do
                let eid = entId ent
                    tag = entTag ent
                    mobType = readMobType tag
                    currentAI = HM.lookupDefault (AIIdle 2.0) eid aiStates

                -- Tamed wolf following logic (skip normal AI)
                if tag == "TamedWolf" then do
                  let wolfPos = entPosition ent
                      playerPos = plPos player
                      distToOwner = distance wolfPos playerPos
                  if distToOwner > 10.0
                    then do
                      -- Teleport wolf near player
                      dx <- randomRIO (-2.0 :: Float, 2.0)
                      dz <- randomRIO (-2.0 :: Float, 2.0)
                      let tpPos = playerPos + V3 dx 0 dz
                      updateEntity entityWorld eid (\e -> e { entPosition = tpPos })
                      modifyIORef' aiStatesRef (HM.insert eid (AIIdle 0.5))
                    else if distToOwner > 2.0
                      then do
                        -- Move toward player
                        let dir = normalize (playerPos - wolfPos)
                            speed = 0.3
                            newVel = dir ^* speed
                            newPos = wolfPos + newVel ^* aiDt
                        updateEntity entityWorld eid (\e -> e { entPosition = newPos, entVelocity = newVel })
                        modifyIORef' aiStatesRef (HM.insert eid (AIIdle 0.5))
                      else
                        modifyIORef' aiStatesRef (HM.insert eid (AIIdle 1.0))
                -- Sitting wolves don't move
                else if tag == "TamedWolfSitting" then
                  pure ()
                else do

                  (ent', newAI) <- updateMobAI dt ent mobType (plPos player) solidQuery currentAI spawnRngRef
                  updateEntity entityWorld eid (const ent')
                  modifyIORef' aiStatesRef (HM.insert eid newAI)
                  -- Skeleton ranged attack, Creeper explosion, or melee
                  case mobType of
                    Skeleton -> case newAI of
                      AIAttack _ _ -> do
                        cooldowns <- readIORef skeletonCooldownRef
                        let cd = HM.lookupDefault 0 eid cooldowns
                            cd' = cd + aiDt
                        if cd' >= 2.0
                          then do
                            modifyIORef' skeletonCooldownRef (HM.insert eid 0)
                            arrowDmg <- System.Random.randomRIO (2 :: Int, 5)
                            let skelPos = entPosition ent'
                                dir = normalize (plPos player - skelPos)
                                vel = dir ^* 20.0 + V3 0 3.0 0
                                arrow = Projectile
                                  { projPos = skelPos + V3 0 1.5 0
                                  , projVelocity = vel
                                  , projAge = 0
                                  , projDamage = arrowDmg
                                  }
                            modifyIORef' projectilesRef (arrow :)
                          else modifyIORef' skeletonCooldownRef (HM.insert eid cd')
                      _ -> modifyIORef' skeletonCooldownRef (HM.delete eid)
                    Creeper -> do
                      let info = mobInfo Creeper
                          creeperPos = entPosition ent'
                          distToPlayer = distance creeperPos (plPos player)
                          explosionRadius = 3 :: Int
                      case newAI of
                        AIAttack _ _ | distToPlayer < miAttackRange info -> do
                          fuseMap <- readIORef creeperFuseRef
                          let oldFuse = IM.findWithDefault 0.0 eid fuseMap
                              newFuse = oldFuse + aiDt
                          if newFuse >= 1.5
                            then do
                              putStrLn $ "Creeper exploded at " ++ show creeperPos ++ "!"
                              playSound soundSystem SndExplosion
                              explodeAt world creeperPos explosionRadius droppedItems
                              let dmg = max 0 (floor (miAttackDmg info * (1.0 - distToPlayer / 7.0)) :: Int)
                              when (dmg > 0) $ do
                                modifyIORef' playerRef (damagePlayer dmg)
                                triggerDamageFlash
                                putStrLn $ "Explosion dealt " ++ show dmg ++ " damage!"
                              updateEntity entityWorld eid (\e -> e { entAlive = False, entHealth = 0 })
                              destroyEntity entityWorld eid
                              modifyIORef' aiStatesRef (HM.delete eid)
                              modifyIORef' creeperFuseRef (IM.delete eid)
                              let V3 cpx _ cpz = creeperPos
                              rebuildExplosionChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef (floor cpx) (floor cpz) explosionRadius
                            else
                              modifyIORef' creeperFuseRef (IM.insert eid newFuse)
                        _ ->
                          modifyIORef' creeperFuseRef (IM.delete eid)
                    -- Apply melee attack damage for other mobs
                    _ -> case (currentAI, newAI) of
                      (AIAttack _ cd, AIAttack _ 1.0) | cd <= 0 -> do
                        let dmg = floor $ miAttackDmg (mobInfo mobType)
                        when (dmg > 0) $ do
                          modifyIORef' playerRef (damagePlayer dmg)
                          triggerDamageFlash
                          putStrLn $ entTag ent ++ " attacked you for " ++ show dmg ++ " damage!"
                          -- Tamed wolves retaliate against attacker
                          allEnts <- livingEntities entityWorld
                          let tamedWolves = filter (\w -> entTag w == "TamedWolf") allEnts
                          forM_ tamedWolves $ \wolf -> do
                            let wolfDist = distance (entPosition wolf) (plPos player)
                            when (wolfDist < 16.0) $ do
                              modifyIORef' aiStatesRef (HM.insert (entId wolf) (AIChase Nothing 0))
                              -- Move wolf toward the attacking mob
                              let attackerPos = entPosition ent'
                                  wolfPos = entPosition wolf
                                  dir = normalize (attackerPos - wolfPos)
                                  speed = 0.3
                                  newVel = dir ^* speed
                                  newPos = wolfPos + newVel ^* aiDt
                              updateEntity entityWorld (entId wolf) (\w -> w { entPosition = newPos, entVelocity = newVel })
                              -- Deal wolf damage to attacker
                              let wolfDmg = miAttackDmg (mobInfo Wolf)
                              updateEntity entityWorld eid (\e -> damageEntity e wolfDmg)
                              putStrLn $ "Tamed wolf attacked " ++ entTag ent ++ " for " ++ show wolfDmg ++ " damage!"
                      _ -> pure ()
                  -- Check for mob death and spawn item drops
                  when (entHealth ent' <= 0) $ do
                    playSound soundSystem SndMobDeath
                    destroyEntity entityWorld eid
                    modifyIORef' aiStatesRef (HM.delete eid)
                    modifyIORef' skeletonCooldownRef (HM.delete eid)
                    let dropPos = entPosition ent'
                    drops <- mobDrops (entTag ent)
                    forM_ drops $ \(item, count) ->
                      when (count > 0) $ do
                        spawnDrop droppedItems item count dropPos
                        putStrLn $ entTag ent ++ " died and dropped " ++ show count ++ "x " ++ show item

            -- Tick arrow projectiles (every frame)
            do projectiles <- readIORef projectilesRef
               player <- readIORef playerRef
               let gravity = V3 0 (-20.0) 0
               newProjectiles <- fmap catMaybes $ forM projectiles $ \proj -> do
                 let newVel = projVelocity proj + gravity ^* dt
                     newPos = projPos proj + newVel ^* dt
                     newAge = projAge proj + dt
                     playerDist = norm (newPos - plPos player)
                 if newAge > 5.0 then pure Nothing         -- despawn after 5 seconds
                 else if playerDist < 1.0 then do          -- hit player
                   modifyIORef' playerRef (damagePlayer (projDamage proj))
                   triggerDamageFlash
                   putStrLn $ "Arrow hit you for " ++ show (projDamage proj) ++ " damage!"
                   pure Nothing
                 else do
                   let blockPos = fmap floor newPos :: V3 Int
                   bt <- worldGetBlock world blockPos
                   if isSolid bt
                     then pure Nothing                      -- stuck in block
                     else pure (Just proj { projPos = newPos, projVelocity = newVel, projAge = newAge })
               writeIORef projectilesRef newProjectiles

            -- Auto-save every ~5 minutes (18000 frames at 60fps)
            when (frameIdx > 0 && frameIdx `mod` 18000 == 0) $ do
              player <- readIORef playerRef
              inv <- readIORef inventoryRef
              dayNight <- readIORef dayNightRef
              sd <- readIORef saveDirRef
              savePlayer sd (buildSaveData player inv dayNight)
              saveWorld sd world

            -- Sapling & cactus growth tick (every 600 frames / ~10 seconds)
            when (frameIdx > 0 && frameIdx `mod` 600 == 0) $ do
              chunks <- HM.toList <$> readTVarIO (worldChunks world)
              dirtyChunks <- newIORef ([] :: [ChunkPos])
              forM_ chunks $ \(V2 cx' cz', chunk) ->
                forEachBlock chunk $ \lx ly lz bt -> do
                  -- Sapling growth
                  when (bt == OakSapling) $ do
                    roll <- randomRIO (1 :: Int, 120)
                    when (roll == 1) $ do
                      let wx = cx' * chunkWidth + lx
                          wz = cz' * chunkDepth + lz
                          basePos = V3 wx ly wz
                      worldSetBlock world basePos Air
                      placeTreeWorld (worldGetBlock world) (worldSetBlock world) basePos
                      modifyIORef' dirtyChunks (chunkPos chunk :)
                  -- Cactus growth
                  when (bt == Cactus) $ do
                    let wx = cx' * chunkWidth + lx
                        wz = cz' * chunkDepth + lz
                    above <- worldGetBlock world (V3 wx (ly + 1) wz)
                    when (above == Air && ly + 1 < chunkHeight) $ do
                      let countDown :: Int -> IO Int
                          countDown y
                            | y <= 0 = pure 0
                            | otherwise = do
                                b <- worldGetBlock world (V3 wx y wz)
                                if b == Cactus then (1 +) <$> countDown (y - 1) else pure 0
                      columnH <- countDown ly
                      when (columnH < 3) $ do
                        roll <- randomRIO (1 :: Int, 20)
                        when (roll == 1) $ do  -- 5% chance
                          worldSetBlock world (V3 wx (ly + 1) wz) Cactus
                          modifyIORef' dirtyChunks (chunkPos chunk :)
              remeshDirtyChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef dirtyChunks

            -- Sugar cane growth tick (every 600 frames / ~10 seconds)
            when (frameIdx > 0 && frameIdx `mod` 600 == 0) $ do
              chunks <- HM.toList <$> readTVarIO (worldChunks world)
              dirtyChunks <- newIORef ([] :: [ChunkPos])
              forM_ chunks $ \(V2 cx' cz', chunk) ->
                forEachBlock chunk $ \lx ly lz bt ->
                  when (bt == SugarCane) $ do
                    let wx = cx' * chunkWidth + lx
                        wz = cz' * chunkDepth + lz
                    -- Check if top of column (air above)
                    aboveBlock <- worldGetBlock world (V3 wx (ly + 1) wz)
                    when (aboveBlock == Air) $ do
                      -- Count sugar cane column height
                      let countBelow pos acc = do
                            b <- worldGetBlock world pos
                            if b == SugarCane && acc < 3
                              then countBelow (pos - V3 0 1 0) (acc + 1)
                              else pure acc
                      columnH <- countBelow (V3 wx ly wz) 0
                      when (columnH < 3) $ do
                        -- Check base block: must be on Sand/Dirt adjacent to Water
                        let baseY = ly - columnH + 1
                            groundPos = V3 wx (baseY - 1) wz
                        groundBlock <- worldGetBlock world groundPos
                        when (groundBlock == Sand || groundBlock == Dirt) $ do
                          let neighbors = [V3 (wx-1) (baseY-1) wz, V3 (wx+1) (baseY-1) wz,
                                           V3 wx (baseY-1) (wz-1), V3 wx (baseY-1) (wz+1)]
                          hasWater <- or <$> mapM (\p -> (== Water) <$> worldGetBlock world p) neighbors
                          when hasWater $ do
                            roll <- randomRIO (1 :: Int, 20)
                            when (roll == 1) $ do  -- 5% chance
                              worldSetBlock world (V3 wx (ly + 1) wz) SugarCane
                              modifyIORef' dirtyChunks (chunkPos chunk :)
              remeshDirtyChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef dirtyChunks

            -- Leaf decay tick (every 300 frames / ~5 seconds)
            when (frameIdx > 0 && frameIdx `mod` 300 == 0) $ do
              chunks <- HM.toList <$> readTVarIO (worldChunks world)
              dirtyChunks <- newIORef ([] :: [ChunkPos])
              forM_ chunks $ \(V2 cx' cz', chunk) ->
                forEachBlock chunk $ \lx ly lz bt ->
                  when (isLeafBlock bt) $ do
                    let wx = cx' * chunkWidth + lx
                        wz = cz' * chunkDepth + lz
                    hasLog <- checkLogNearby world (V3 wx ly wz) 4
                    unless hasLog $ do
                      roll <- randomRIO (1 :: Int, 10)
                      when (roll == 1) $ do
                        worldSetBlock world (V3 wx ly wz) Air
                        -- 5% chance to drop a sapling on decay
                        sapRoll <- randomRIO (1 :: Int, 20)
                        when (sapRoll == 1) $
                          spawnDrop droppedItems (BlockItem OakSapling) 1
                            (V3 (fromIntegral wx + 0.5) (fromIntegral ly + 0.5) (fromIntegral wz + 0.5))
                        modifyIORef' dirtyChunks (chunkPos chunk :)
              remeshDirtyChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef dirtyChunks

            -- Fire tick (every 60 frames / ~1 second): spread and burn out
            when (frameIdx > 0 && frameIdx `mod` 60 == 0) $ do
              chunks <- HM.toList <$> readTVarIO (worldChunks world)
              dirtyChunks <- newIORef ([] :: [ChunkPos])
              forM_ chunks $ \(V2 cx' cz', chunk) ->
                forEachBlock chunk $ \lx ly lz bt ->
                  when (bt == Fire) $ do
                    let wx = cx' * chunkWidth + lx
                        wz = cz' * chunkDepth + lz
                        firePos = V3 wx ly wz
                    -- 20% chance to burn out
                    burnRoll <- randomRIO (1 :: Int, 5)
                    if burnRoll == 1
                      then do
                        worldSetBlock world firePos Air
                        modifyIORef' dirtyChunks (chunkPos chunk :)
                      else do
                        -- Check adjacent blocks for fire spread (5% chance per flammable neighbor)
                        let neighbors = [ V3 (wx+1) ly wz, V3 (wx-1) ly wz
                                        , V3 wx (ly+1) wz, V3 wx (ly-1) wz
                                        , V3 wx ly (wz+1), V3 wx ly (wz-1) ]
                        forM_ neighbors $ \nPos@(V3 nx' ny' nz') -> do
                          nbt <- worldGetBlock world nPos
                          when (isFlammable nbt) $ do
                            spreadRoll <- randomRIO (1 :: Int, 20)
                            when (spreadRoll == 1) $ do
                              worldSetBlock world nPos Fire
                              modifyIORef' dirtyChunks (chunkPos chunk :)
                          -- Also try to spread to the air block above a flammable neighbor
                          when (nbt == Air && ny' >= 0 && ny' < chunkHeight) $ do
                            belowBt <- worldGetBlock world (V3 nx' (ny' - 1) nz')
                            when (isFlammable belowBt) $ do
                              spreadRoll <- randomRIO (1 :: Int, 20)
                              when (spreadRoll == 1) $ do
                                worldSetBlock world nPos Fire
                                modifyIORef' dirtyChunks (chunkPos chunk :)
              remeshDirtyChunks world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef dirtyChunks

            -- Update chunks periodically
            when (frameIdx `mod` 60 == 0) $ do
              player <- readIORef playerRef
              newChunks <- updateLoadedChunks world (plPos player)
              -- Settle gravity-affected blocks in newly generated chunks
              forM_ newChunks $ \c ->
                void $ settleChunkGravity world c
              -- Mesh only newly loaded chunks
              mapM_ (\c -> meshSingleChunk physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef c) newChunks
              -- Remove meshes for unloaded chunks
              pruneChunkMeshes world device meshCacheRef

            -- Render
            player <- readIORef playerRef
            let currentFrame = frameIdx `mod` maxFrames
            let cmdBuf = cmdBuffers V.! currentFrame
            let sync   = syncObjects V.! currentFrame
            let ds     = dsSets V.! currentFrame

            sc' <- readIORef scRef
            dayNightVal <- readIORef dayNightRef
            weatherVal <- readIORef weatherRef
            let Vk.Extent2D{width = extW, height = extH} = scExtent sc'
            let cam = cameraFromPlayer player
                aspect = fromIntegral extW / fromIntegral extH
                V3 sx sy sz = getSunDirection dayNightVal
                ubo = UniformBufferObject
                  { uboModel        = transpose identity
                  , uboView         = transpose $ cameraViewMatrix cam
                  , uboProjection   = transpose $ cameraProjectionMatrix aspect 0.1 1000 cam
                  , uboSunDirection = V4 sx sy sz 0
                  , uboAmbientLight = getAmbientLight dayNightVal * weatherAmbientMultiplier weatherVal
                  , _uboPad1        = 0
                  , _uboPad2        = 0
                  , _uboPad3        = 0
                  }
            updateUBO device (uniformBufs V.! currentFrame) ubo

            meshCache <- readIORef meshCacheRef
            -- Frustum culling: only draw chunks visible to the camera
            let vp = cameraProjectionMatrix aspect 0.1 1000 cam !*! cameraViewMatrix cam
                frustum = extractFrustum vp
                chunkDraws = [ draw
                             | (V2 cx cz, draw) <- HM.toList meshCache
                             , let minCorner = V3 (fromIntegral cx * 16) 0 (fromIntegral cz * 16)
                                   maxCorner = V3 (fromIntegral (cx + 1) * 16) 256 (fromIntegral (cz + 1) * 16)
                             , isAABBInFrustum frustum minCorner maxCorner
                             ]

            -- Compute sky color from day/night cycle, adjusted for weather
            let skyMul = weatherSkyMultiplier weatherVal
                V4 r g b a = getSkyColor dayNightVal
                V4 skyR skyG skyB skyA = V4 (r * skyMul) (g * skyMul) (b * skyMul) a

            -- Per-frame raycast for block target highlight
            do let eyePos = plPos player + V3 0 1.62 0
                   dir = dirFromPlayer player
                   bqCb bx by bz = do
                     bt <- worldGetBlock world (V3 bx by bz)
                     pure (World.Block.isSolid bt)
               mHit <- raycastBlock bqCb eyePos dir maxReach
               writeIORef targetBlockRef (fmap rhBlockPos mHit)

            -- Update HUD vertices with current inventory, mining progress, and health
            inv <- readIORef inventoryRef
            mining <- readIORef miningRef
            player' <- readIORef playerRef
            let miningProgress = case mining of
                  Just (_, p) -> p
                  Nothing     -> 0
            mode <- readIORef gameModeRef
            cursorItem <- readIORef cursorItemRef
            craftGrid <- readIORef craftingGridRef
            showDebug <- readIORef debugOverlayRef
            fpsVal <- readIORef fpsDisplayRef
            chunks <- readTVarIO (worldChunks world)
            targetBlock <- readIORef targetBlockRef
            sleepMsg <- readIORef sleepMessageRef
            let sleepMsgText = case sleepMsg of
                  Just (t, msg) | t > 0 -> Just msg
                  _                     -> Nothing
            case sleepMsg of
              Just (t, msg) -> writeIORef sleepMessageRef (if t - dt > 0 then Just (t - dt, msg) else Nothing)
              Nothing       -> pure ()
            debugInfo <- if showDebug
                  then do
                    let V3 px' _ pz' = plPos player'
                        biomeName = show (biomeAt seed (realToFrac px') (realToFrac pz'))
                    targetBlockName <- case targetBlock of
                      Just (V3 bx by bz) -> show <$> worldGetBlock world (V3 bx by bz)
                      Nothing -> pure "NONE"
                    entCount <- entityCount entityWorld
                    let weatherStr = case wsType weatherVal of
                          Clear -> "CLEAR"
                          Rain  -> "RAIN (" ++ showF2 (wsIntensity weatherVal) ++ ")"
                        gameModeStr = if plFlying player' then "FLYING" else "PLAYING"
                        dnc = dayNightVal
                        todStr = show (getTimeOfDay dnc)
                        dayTime = dncTime dnc
                    pure $ Just DebugInfo
                      { dbgPos         = plPos player'
                      , dbgYaw         = plYaw player'
                      , dbgPitch       = plPitch player'
                      , dbgFps         = fpsVal
                      , dbgChunkCount  = HM.size chunks
                      , dbgBiome       = biomeName
                      , dbgTargetBlock = targetBlockName
                      , dbgLightLevel  = "N/A"
                      , dbgEntityCount = entCount
                      , dbgWeather     = weatherStr
                      , dbgGameMode    = gameModeStr
                      , dbgHealth      = plHealth player'
                      , dbgHunger      = plHunger player'
                      , dbgTimeOfDay   = todStr
                      , dbgDayTime     = dayTime
                      }
                  else pure Nothing
            -- Get chest inventory if a chest is open
            mChestInv <- case mode of
              ChestOpen -> do
                mPos <- readIORef chestPosRef
                case mPos of
                  Just cPos -> getChestInventory blockEntityMapRef cPos
                  Nothing   -> pure Nothing
              _ -> pure Nothing
            -- Get dispenser inventory if a dispenser is open
            mDispInv <- case mode of
              DispenserOpen -> do
                mPos <- readIORef dispenserPosRef
                case mPos of
                  Just dPos -> getDispenserInventory blockEntityMapRef dPos
                  Nothing   -> pure Nothing
              _ -> pure Nothing
            (mx, my) <- readIORef mousePosRef
            (winW, winH) <- getWindowSize wh
            let mouseNdcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
                mouseNdcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
            furnaceState <- readIORef furnaceStateRef
            particles <- readIORef particleSystemRef
            -- Damage flash: read and decrement
            damageFlash <- readIORef damageFlashRef
            when (damageFlash > 0) $
              writeIORef damageFlashRef (max 0 (damageFlash - dt))
            let particleVerts = if mode == Playing then renderParticles particles vp else []
            spawnPos <- readIORef spawnPointRef
            let hudVerts = buildHudVertices inv miningProgress (plHealth player') (plHunger player') (plAirSupply player') mode cursorItem craftGrid mChestInv mDispInv furnaceState debugInfo (fmap (\tb -> (tb, vp)) targetBlock) sleepMsgText damageFlash mouseNdcX mouseNdcY (plPos player') spawnPos dayNightVal
                    VS.++ VS.fromList particleVerts
                hudVC = VS.length hudVerts `div` 6
            writeIORef hudVertCountRef hudVC
            when (hudVC > 0) $ do
              ptr <- Vk.mapMemory device (baMemory hudBuf) 0 (fromIntegral $ VS.length hudVerts * 4) Vk.zero
              VS.unsafeWith hudVerts $ \srcPtr ->
                copyBytes (castPtr ptr) srcPtr (VS.length hudVerts * 4)
              Vk.unmapMemory device (baMemory hudBuf)

            hudVC' <- readIORef hudVertCountRef
            fbs <- readIORef fbRef
            needsRecreate <- drawFrame vc sc' pc fbs cmdBuf sync chunkDraws ds (skyR, skyG, skyB, skyA) (Just (hudPipeline, hudBuf, hudVC'))

            resized <- readIORef (whResized wh)
            when (needsRecreate || resized) $ do
              writeIORef (whResized wh) False
              winSize <- getWindowSize wh
              let (w, h) = winSize
              when (w > 0 && h > 0) $ do
                scOld <- readIORef scRef
                scNew <- recreateSwapchain vc scOld winSize
                writeIORef scRef scNew
                -- Recreate depth buffer for new size
                oldDepth <- readIORef depthRef
                destroyDepthResources device oldDepth
                newDepth <- createDepthResources physDevice device (scExtent scNew)
                writeIORef depthRef newDepth
                -- Recreate framebuffers
                oldFbs <- readIORef fbRef
                destroyFramebuffers device oldFbs
                newFbs <- createFramebuffers device renderPass scNew (drImageView newDepth)
                writeIORef fbRef newFbs

            writeIORef frameRef (frameIdx + 1)
            loop
    loop `finally` do
      -- Cleanup (runs even if game loop throws an exception)
      putStrLn "Cleaning up..."
      Vk.deviceWaitIdle device
      cleanupSoundSystem soundSystem
      destroySyncObjects device syncObjects
      Vk.destroyCommandPool device cmdPool Nothing
      -- Destroy all chunk meshes
      meshCache <- readIORef meshCacheRef
      mapM_ (\(vb, ib, _) -> destroyBuffer device vb >> destroyBuffer device ib) (HM.elems meshCache)
      V.mapM_ (destroyBuffer device) uniformBufs
      destroyTextureImage device texAtlas
      Vk.destroyDescriptorPool device dsPool Nothing
      Vk.destroyDescriptorSetLayout device dsLayout Nothing
      fbs <- readIORef fbRef
      destroyFramebuffers device fbs
      readIORef depthRef >>= destroyDepthResources device
      destroyPipelineContext device pc
      destroyHudPipeline device hudPipeline
      destroyBuffer device hudBuf
      scFinal <- readIORef scRef
      destroySwapchain device scFinal
      destroyVulkanContext vc
      putStrLn "Goodbye!"

-- | Run physics ticks consuming accumulated time
playerLoop :: PlayerInput -> BlockHeightQuery -> BlockQuery -> BlockQuery -> IORef Float -> Float -> IORef Player -> IO ()
playerLoop input blockQuery waterQuery ladderQuery accumRef accum playerRef
  | accum < tickRate = writeIORef accumRef accum
  | otherwise = do
      player <- readIORef playerRef
      player' <- updatePlayer tickRate input blockQuery waterQuery ladderQuery player
      -- Void damage: kill player below Y=0
      let V3 _ py _ = plPos player'
          player'' = if py < 0 then damagePlayer 4 player' else player'
      writeIORef playerRef player''
      playerLoop (input { piToggleFly = False }) blockQuery waterQuery ladderQuery accumRef (accum - tickRate) playerRef

-- | Convert player state to Camera
cameraFromPlayer :: Player -> Camera
cameraFromPlayer player =
  let yawR   = plYaw player * pi / 180
      pitchR = plPitch player * pi / 180
      front  = V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)
      fov    = if plSprinting player then 55 else 45
  in defaultCamera
    { camPosition = plPos player + V3 0 1.62 0  -- eye height
    , camFront    = front
    , camYaw      = plYaw player
    , camPitch    = plPitch player
    , camFov      = fov
    }

-- | Get look direction from player
dirFromPlayer :: Player -> V3 Float
dirFromPlayer player =
  let yawR   = plYaw player * pi / 180
      pitchR = plPitch player * pi / 180
  in normalize $ V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)

-- | Find the entity closest to a given position (partial: requires non-empty list)
closestTo :: V3 Float -> [Entity] -> Entity
closestTo pos = foldr1 (\a b -> if distance (entPosition a) pos < distance (entPosition b) pos then a else b)

-- | Show a float with a given number of decimal places
showFloatN :: Int -> Float -> String
showFloatN decimals f =
  let factor = 10 ^ decimals
      n = round (f * fromIntegral factor) :: Int
      whole = div (abs n) factor
      frac  = mod (abs n) factor
      sign  = if f < 0 && n /= 0 then "-" else ""
      pad   = replicate (decimals - length (show frac)) '0'
  in sign ++ show whole ++ "." ++ pad ++ show frac

-- | Show a float with 1 decimal place
showF1 :: Float -> String
showF1 = showFloatN 1

-- | Show a float with 2 decimal places
showF2 :: Float -> String
showF2 = showFloatN 2

-- | Check if a GLFW key is pressed
isKeyDown :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyDown win key = do
  state <- GLFW.getKey win key
  pure $ state == GLFW.KeyState'Pressed

-- | Build GPU meshes for all loaded chunks (initial load)
rebuildAllChunkMeshes
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
rebuildAllChunkMeshes world physDevice device cmdPool queue cacheRef = do
  chunks <- HM.elems <$> readTVarIO (worldChunks world)
  mapM_ (meshSingleChunk physDevice device cmdPool queue cacheRef) chunks

-- | Settle gravity-affected blocks in all currently loaded chunks
settleAllLoadedChunks :: World -> IO ()
settleAllLoadedChunks world = do
  chunks <- HM.elems <$> readTVarIO (worldChunks world)
  mapM_ (settleChunkGravity world) chunks

-- | Build GPU mesh for a single chunk and insert into cache
meshSingleChunk
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Chunk -> IO ()
meshSingleChunk physDevice device cmdPool queue cacheRef chunk = do
  -- Propagate light for this chunk
  lm <- newLightMap
  propagateBlockLight chunk lm
  propagateSkyLight chunk lm
  -- Mesh with light data
  mesh <- meshChunkWithLight chunk lm
  let ic = VS.length (mdIndices mesh)
      pos = chunkPos chunk
      V2 cx cz = pos
      offsetX = fromIntegral cx * fromIntegral chunkWidth
      offsetZ = fromIntegral cz * fromIntegral chunkDepth
      -- Offset vertices to world space
      worldVerts = VS.map (\v ->
        v { bvPosition = bvPosition v + V3 offsetX 0 offsetZ }
        ) (mdVertices mesh)

  -- Destroy old mesh for this chunk if it exists
  cache <- readIORef cacheRef
  case HM.lookup pos cache of
    Just (oldVb, oldIb, _) -> do
      destroyBuffer device oldVb
      destroyBuffer device oldIb
    Nothing -> pure ()

  if VS.null worldVerts
    then modifyIORef' cacheRef (HM.delete pos)
    else do
      vb <- createVertexBuffer physDevice device cmdPool queue worldVerts
      ib <- createIndexBuffer physDevice device cmdPool queue (mdIndices mesh)
      modifyIORef' cacheRef (HM.insert pos (vb, ib, ic))

-- | Rebuild mesh for the chunk containing world coordinates (wx, wz)
rebuildChunkAt
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> IO ()
rebuildChunkAt world physDevice device cmdPool queue cacheRef wx wz = do
  let cx = wx `div` chunkWidth
      cz = wz `div` chunkDepth
  mChunk <- getChunk world (V2 cx cz)
  case mChunk of
    Nothing -> pure ()
    Just chunk -> meshSingleChunk physDevice device cmdPool queue cacheRef chunk

-- | Rebuild the chunk at (wx, wz) plus its 4 cardinal neighbors.
--   Light propagation is per-chunk, so neighbors must be re-meshed when
--   a light-emitting block changes near a chunk boundary.
rebuildChunkWithNeighbors
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> IO ()
rebuildChunkWithNeighbors world physDevice device cmdPool queue cacheRef wx wz = do
  let cx = wx `div` chunkWidth
      cz = wz `div` chunkDepth
      rebuild ncx ncz = rebuildChunkAt world physDevice device cmdPool queue cacheRef (ncx * chunkWidth) (ncz * chunkDepth)
  forM_ [ (cx, cz), (cx - 1, cz), (cx + 1, cz), (cx, cz - 1), (cx, cz + 1) ] $
    uncurry rebuild

-- | Remove cached meshes for chunks that are no longer loaded
pruneChunkMeshes
  :: World -> Vk.Device
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
pruneChunkMeshes world device cacheRef = do
  loadedChunks <- readTVarIO (worldChunks world)
  cache <- readIORef cacheRef
  let toRemove = HM.filterWithKey (\k _ -> not (HM.member k loadedChunks)) cache
  mapM_ (\(vb, ib, _) -> destroyBuffer device vb >> destroyBuffer device ib) (HM.elems toRemove)
  modifyIORef' cacheRef (\c -> HM.filterWithKey (\k _ -> HM.member k loadedChunks) c)

-- | Write UBO data to mapped uniform buffer
updateUBO :: Vk.Device -> BufferAllocation -> UniformBufferObject -> IO ()
updateUBO device buf ubo = do
  ptr <- Vk.mapMemory device (baMemory buf) 0 (fromIntegral $ sizeOf ubo) Vk.zero
  poke (castPtr ptr) ubo
  Vk.unmapMemory device (baMemory buf)

-- | Read accumulated dirty chunk positions, deduplicate, and remesh each
remeshDirtyChunks
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IORef [ChunkPos] -> IO ()
remeshDirtyChunks world physDevice device cmdPool queue cacheRef dirtyRef = do
  dirty <- readIORef dirtyRef
  let unique = HM.keys (HM.fromList [(cp, ()) | cp <- dirty])
  forM_ unique $ \cp -> do
    mCh <- getChunk world cp
    case mCh of
      Nothing -> pure ()
      Just ch -> meshSingleChunk physDevice device cmdPool queue cacheRef ch

-- | Rebuild chunk meshes in the area affected by an explosion
rebuildExplosionChunks
  :: World -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> Int -> Int -> Int -> IO ()
rebuildExplosionChunks world physDevice device cmdPool queue cacheRef centerX centerZ radius = do
  let bx0 = centerX - radius; bz0 = centerZ - radius
      bx1 = centerX + radius; bz1 = centerZ + radius
      cx0 = bx0 `div` chunkWidth; cz0 = bz0 `div` chunkDepth
      cx1 = bx1 `div` chunkWidth; cz1 = bz1 `div` chunkDepth
  forM_ [cx0 .. cx1] $ \chx ->
    forM_ [cz0 .. cz1] $ \chz -> do
      mChunk <- getChunk world (V2 chx chz)
      case mChunk of
        Nothing -> pure ()
        Just ch -> meshSingleChunk physDevice device cmdPool queue cacheRef ch

-- | Inventory slot layout constants
invGridX0, invGridY0, invSlotW, invSlotH, invSlotPad :: Float
invGridX0 = -0.55   -- left edge of inventory grid in NDC
invGridY0 = -0.5    -- top edge
invSlotW  = 0.1
invSlotH  = 0.1
invSlotPad = 0.008

-- | Check if NDC coordinates hit an inventory slot. Returns slot index 0-35.
hitInventorySlot :: Float -> Float -> Maybe Int
hitInventorySlot nx ny =
  let -- Hotbar: row 0 (slots 0-8), main inventory: rows 1-3 (slots 9-35)
      col = floor ((nx - invGridX0) / invSlotW) :: Int
      row = floor ((ny - invGridY0) / invSlotH) :: Int
  in if col >= 0 && col < 9 && row >= 0 && row < 4
     then Just (if row == 0 then col else 9 + (row - 1) * 9 + col)
     else Nothing

-- | Crafting slot types
data CraftSlot = CraftGrid !Int !Int | CraftOutput | CraftInvSlot !Int
  deriving stock (Show, Eq)

-- | Check if NDC coordinates hit a crafting slot
hitCraftingSlot :: Float -> Float -> Maybe CraftSlot
hitCraftingSlot nx ny
  -- 3x3 crafting grid area
  | nx >= -0.3 && nx <= 0.0 && ny >= -0.35 && ny <= -0.05 =
      let col = floor ((nx - (-0.3)) / 0.1) :: Int
          row = floor ((ny - (-0.35)) / 0.1) :: Int
      in if col >= 0 && col < 3 && row >= 0 && row < 3
         then Just (CraftGrid row col)
         else Nothing
  -- Output slot
  | nx >= 0.15 && nx <= 0.25 && ny >= -0.25 && ny <= -0.15 = Just CraftOutput
  -- Inventory slots below
  | otherwise = fmap CraftInvSlot (hitInventorySlot nx (ny - 0.6))

-- | Explode blocks in a sphere around a position, with a chance to drop items.
explodeAt :: World -> V3 Float -> Int -> DroppedItems -> IO ()
explodeAt world (V3 centerX centerY centerZ) radius droppedItemsRef = do
  let cx = floor centerX :: Int
      cy = floor centerY :: Int
      cz = floor centerZ :: Int
      r  = fromIntegral radius :: Float
  forM_ [cx - radius .. cx + radius] $ \x ->
    forM_ [cy - radius .. cy + radius] $ \y ->
      forM_ [cz - radius .. cz + radius] $ \z -> do
        let dist = sqrt (fromIntegral ((x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int) + (z - cz) ^ (2 :: Int))) :: Float
        when (dist <= r) $ do
          bt <- worldGetBlock world (V3 x y z)
          when (bt /= Air && bt /= Bedrock && bt /= Water && bt /= Lava) $ do
            worldSetBlock world (V3 x y z) Air
            -- 30% chance to drop the block's items
            roll <- randomRIO (0.0, 1.0 :: Float)
            when (roll < 0.3) $ do
              let drops = blockDrops bt
              forM_ drops $ \(item, count) ->
                spawnDrop droppedItemsRef item count
                  (V3 (fromIntegral x + 0.5) (fromIntegral y + 0.5) (fromIntegral z + 0.5))
-- | Chest slot types
data ChestSlotType = ChestSlot !Int | ChestInvSlot !Int
  deriving stock (Show, Eq)

-- | Check if NDC coordinates hit a chest UI slot
-- Chest grid: 3 rows of 9 slots starting at chestGridY0 = -0.35
-- Player inventory: 4 rows of 9 starting at chestInvY0 = 0.1
hitChestSlot :: Float -> Float -> Maybe ChestSlotType
hitChestSlot nx ny
  -- Chest slots (3 rows of 9)
  | ny >= chestY0 && ny < chestY0 + 3 * invSlotH
  , nx >= invGridX0 && nx < invGridX0 + 9 * invSlotW =
      let col = floor ((nx - invGridX0) / invSlotW) :: Int
          row = floor ((ny - chestY0) / invSlotH) :: Int
      in if col >= 0 && col < 9 && row >= 0 && row < 3
         then Just (ChestSlot (row * 9 + col))
         else Nothing
  -- Player inventory slots (4 rows of 9)
  | ny >= playerY0 && ny < playerY0 + 4 * invSlotH
  , nx >= invGridX0 && nx < invGridX0 + 9 * invSlotW =
      let col = floor ((nx - invGridX0) / invSlotW) :: Int
          row = floor ((ny - playerY0) / invSlotH) :: Int
      in if col >= 0 && col < 9 && row >= 0 && row < 4
         then Just (ChestInvSlot (if row == 0 then col else 9 + (row - 1) * 9 + col))
         else Nothing
  | otherwise = Nothing
  where
    chestY0  = -0.35 :: Float
    playerY0 = 0.1 :: Float

-- | Dispenser slot types
data DispenserSlotType = DispenserSlot !Int | DispenserInvSlot !Int
  deriving stock (Show, Eq)

-- | Check if NDC coordinates hit a dispenser UI slot
-- Dispenser grid: 3 rows of 3 slots starting at dispGridY0 = -0.35, centered at dispGridX0 = -0.2
-- Player inventory: 4 rows of 9 starting at dispInvY0 = 0.1
hitDispenserSlot :: Float -> Float -> Maybe DispenserSlotType
hitDispenserSlot nx ny
  -- Dispenser slots (3 rows of 3)
  | ny >= dispY0 && ny < dispY0 + 3 * invSlotH
  , nx >= dispX0 && nx < dispX0 + 3 * invSlotW =
      let col = floor ((nx - dispX0) / invSlotW) :: Int
          row = floor ((ny - dispY0) / invSlotH) :: Int
      in if col >= 0 && col < 3 && row >= 0 && row < 3
         then Just (DispenserSlot (row * 3 + col))
         else Nothing
  -- Player inventory slots (4 rows of 9)
  | ny >= dispPlayerY0 && ny < dispPlayerY0 + 4 * invSlotH
  , nx >= invGridX0 && nx < invGridX0 + 9 * invSlotW =
      let col = floor ((nx - invGridX0) / invSlotW) :: Int
          row = floor ((ny - dispPlayerY0) / invSlotH) :: Int
      in if col >= 0 && col < 9 && row >= 0 && row < 4
         then Just (DispenserInvSlot (if row == 0 then col else 9 + (row - 1) * 9 + col))
         else Nothing
  | otherwise = Nothing
  where
    dispX0 = -0.2 :: Float
    dispY0 = -0.35 :: Float
    dispPlayerY0 = 0.1 :: Float

-- | Furnace slot targets
data FurnaceSlot = FurnaceInputSlot | FurnaceFuelSlot | FurnaceOutputSlot | FurnaceInvSlot !Int
  deriving stock (Show, Eq)

-- | Check if NDC coordinates hit a furnace slot
hitFurnaceSlot :: Float -> Float -> Maybe FurnaceSlot
hitFurnaceSlot nx ny
  -- Input slot (top-left)
  | nx >= -0.25 && nx <= -0.15 && ny >= -0.30 && ny <= -0.20 = Just FurnaceInputSlot
  -- Fuel slot (bottom-left)
  | nx >= -0.25 && nx <= -0.15 && ny >= -0.12 && ny <= -0.02 = Just FurnaceFuelSlot
  -- Output slot (right)
  | nx >= 0.1 && nx <= 0.23 && ny >= -0.24 && ny <= -0.11 = Just FurnaceOutputSlot
  -- Inventory slots below
  | otherwise = fmap FurnaceInvSlot (hitInventorySlot nx (ny - 0.6))

-- | Parse mob type from entity tag string
readMobType :: String -> MobType
readMobType "Zombie"   = Zombie
readMobType "Skeleton" = Skeleton
readMobType "Creeper"  = Creeper
readMobType "Spider"   = Spider
readMobType "Pig"      = Pig
readMobType "Cow"      = Cow
readMobType "Sheep"    = Sheep
readMobType "Chicken"  = Chicken
readMobType "Wolf"     = Wolf
readMobType "TamedWolf" = Wolf
readMobType "TamedWolfSitting" = Wolf
readMobType _          = Pig

-- | Whether a block is flammable (can catch fire from adjacent fire blocks)
isFlammable :: BlockType -> Bool
isFlammable OakLog    = True
isFlammable OakPlanks = True
isFlammable OakLeaves = True
isFlammable Wool      = True
isFlammable _         = False

-- | Check if an OakLog exists within a given Manhattan distance of a world position
checkLogNearby :: World -> V3 Int -> Int -> IO Bool
checkLogNearby world (V3 wx wy wz) radius = go offsets
  where
    offsets = [ V3 dx dy dz
              | dx <- [-radius..radius]
              , dy <- [-radius..radius]
              , dz <- [-radius..radius]
              , abs dx + abs dy + abs dz <= radius
              ]
    go [] = pure False
    go (d : rest) = do
      bt <- worldGetBlock world (V3 wx wy wz + d)
      if bt == OakLog then pure True else go rest

-- | Build HUD vertices from current state
-- debugInfo: Just DebugInfo when F3 overlay is active
-- targetInfo: Just (blockPos, viewProjectionMatrix) for wireframe highlight
-- sleepMsgText: Just "message" when a bed-related message should be shown
buildHudVertices :: Inventory -> Float -> Int -> Int -> Float -> GameMode -> Maybe ItemStack -> CraftingGrid -> Maybe Inventory -> Maybe Inventory -> FurnaceState -> Maybe DebugInfo -> Maybe (V3 Int, M44 Float) -> Maybe String -> Float -> Float -> Float -> V3 Float -> V3 Float -> DayNightCycle -> VS.Vector Float
buildHudVertices inv miningProgress health hunger airSupply mode cursorItem craftGrid mChestInv mDispInv furnaceState debugInfo targetInfo sleepMsgText damageFlash mouseX mouseY playerPos spawnPos dayNight = VS.fromList $
  case mode of
    MainMenu -> menuVerts
    Paused   -> pauseVerts
    Playing  -> crosshairVerts ++ hotbarBgVerts ++ slotVerts ++ selectorVerts ++ miningBarVerts ++ healthVerts ++ hungerVerts ++ bubbleVerts ++ handVerts ++ debugVerts ++ highlightVerts ++ sleepMsgVerts ++ damageFlashVerts ++ compassClockVerts
    InventoryOpen -> invScreenVerts ++ cursorVerts
    CraftingOpen  -> craftScreenVerts ++ cursorVerts
    ChestOpen     -> chestScreenVerts ++ cursorVerts
    FurnaceOpen   -> furnaceScreenVerts ++ cursorVerts
    DispenserOpen -> dispenserScreenVerts ++ cursorVerts
    EnchantingOpen -> enchantScreenVerts ++ cursorVerts
    DeathScreen   -> deathScreenVerts
  where
    -- Crosshair: white + at center
    cs = 0.015 :: Float  -- size
    ct = 0.003 :: Float  -- thickness
    w = (1.0, 1.0, 1.0, 0.8 :: Float)
    quad x0 y0 x1 y1 (r, g, b, a) =
      [x0, y0, r, g, b, a,  x1, y0, r, g, b, a,  x1, y1, r, g, b, a
      ,x0, y0, r, g, b, a,  x1, y1, r, g, b, a,  x0, y1, r, g, b, a]

    crosshairVerts = quad (-cs) (-ct) cs ct w ++ quad (-ct) (-cs) ct cs w

    -- Hotbar: 9 slots at bottom center (Vulkan Y: -1=top, +1=bottom)
    slotW = 0.09 :: Float  -- width per slot in NDC
    slotH = 0.09 :: Float
    hotbarY = 1.0 - slotH - 0.02  -- just above bottom edge
    hotbarX0 = -(slotW * 4.5)     -- center 9 slots
    slotPad = 0.005 :: Float       -- gap between slots

    hotbarBgVerts = quad (hotbarX0 - slotPad) (hotbarY - slotPad)
                        (hotbarX0 + 9 * slotW + slotPad) (1.0)
                        (0.15, 0.15, 0.15, 0.7)

    -- Block icons for each slot's contents (3x3 mini-pattern)
    slotVerts = concatMap makeSlot [0..8]
    makeSlot i =
      let x0 = hotbarX0 + fromIntegral i * slotW + slotPad * 2
          y0 = hotbarY + slotPad * 2
          sw = slotW - 4 * slotPad
          sh = slotH - 4 * slotPad
          pixW = sw / 3
          pixH = sh / 3
      in case getSlot inv i of
        Nothing -> []
        Just (ItemStack item cnt) ->
          let colors = itemMiniIcon item
              iconVerts = concatMap (\(row, col, c) ->
                   quad (x0 + fromIntegral col * pixW) (y0 + fromIntegral row * pixH)
                        (x0 + fromIntegral (col + 1) * pixW) (y0 + fromIntegral (row + 1) * pixH) c
                   ) colors
              countText = if cnt > 1
                then renderText (x0 + sw - 0.025) (y0 + sh - 0.02) 0.6 (1,1,1,1) (show cnt)
                else []
          in iconVerts ++ countText

    -- Highlight selected slot
    sel = invSelected inv
    selX = hotbarX0 + fromIntegral sel * slotW
    selectorVerts = quad selX hotbarY (selX + slotW) (hotbarY + slotH) (1.0, 1.0, 1.0, 0.3)

    -- Mining progress bar: thin bar below crosshair
    miningBarVerts
      | miningProgress <= 0 = []
      | otherwise =
          let barW = 0.15
              barH = 0.012
              barY = 0.03  -- below crosshair
              fillW = barW * min 1.0 miningProgress
          in quad (-barW) barY ((-barW) + fillW * 2) (barY + barH) (0.9, 0.9, 0.2, 0.9)

    -- Health hearts: red squares above hotbar
    healthVerts = concatMap makeHeart [0..9]
    makeHeart i =
      let heartW = 0.035
          heartH = 0.035
          heartY = hotbarY - heartH - 0.01
          heartX = hotbarX0 + fromIntegral i * (heartW + 0.005)
          halfHeart = (i * 2 + 1) == health
          fullHeart = (i * 2 + 2) <= health
          color
            | fullHeart = (0.85, 0.1, 0.1, 1.0)
            | halfHeart = (0.85, 0.1, 0.1, 0.5)
            | otherwise = (0.3, 0.1, 0.1, 0.4)
      in quad heartX heartY (heartX + heartW) (heartY + heartH) color

    -- Hunger drumsticks: brown squares on the right side above hotbar
    hungerVerts = concatMap makeDrumstick [0..9]
    makeDrumstick i =
      let dw = 0.035
          dh = 0.035
          dY = hotbarY - dh - 0.01
          -- Right-aligned: start from right edge of hotbar
          dX = hotbarX0 + 9 * slotW - fromIntegral (i + 1) * (dw + 0.005) + 0.005
          halfDrum = (i * 2 + 1) == hunger
          fullDrum = (i * 2 + 2) <= hunger
          color
            | fullDrum = (0.7, 0.5, 0.15, 1.0)   -- full drumstick (brown)
            | halfDrum = (0.7, 0.5, 0.15, 0.5)   -- half
            | otherwise = (0.3, 0.2, 0.05, 0.4)  -- empty
      in quad dX dY (dX + dw) (dY + dh) color

    -- Air bubble bar: shown above hunger bar (right side) when underwater
    bubbleVerts
      | airSupply >= maxAirSupply = []
      | otherwise = concatMap makeBubble [0..9]
    makeBubble i =
      let bw = 0.035
          bh = 0.035
          -- Position above the hunger bar (hunger is at hotbarY - dh - 0.01)
          bY = hotbarY - bh - 0.01 - bh - 0.01  -- one row above hunger
          -- Right-aligned like hunger bar
          bX = hotbarX0 + 9 * slotW - fromIntegral (i + 1 :: Int) * (bw + 0.005) + 0.005
          -- Each bubble = 1.5 seconds of air supply
          bubbleThreshold = fromIntegral (9 - i) * 1.5  -- bubble i pops when air <= this
          fullBubble = airSupply > bubbleThreshold
          partialBubble = airSupply > bubbleThreshold - 1.5 && not fullBubble
          color
            | fullBubble    = (0.3, 0.6, 0.9, 1.0)   -- full bubble (blue)
            | partialBubble = (0.3, 0.6, 0.9, 0.4)   -- popping bubble (faded)
            | otherwise     = (0.15, 0.25, 0.35, 0.2) -- empty slot (very faded)
      in quad bX bY (bX + bw) (bY + bh) color

    -- First-person hand/arm in lower-right
    handVerts =
      let -- Swing animation: offset hand when mining
          swingOff = if miningProgress > 0 then sin (miningProgress * 6.28) * 0.08 else 0
          -- Arm base position (lower-right, Vulkan NDC)
          armX = 0.45 + swingOff
          armY = 0.3 - abs swingOff * 0.5
          -- Skin color
          skin = (0.85, 0.7, 0.55, 1.0)
          -- Item color on the "hand"
          heldColor = case selectedItem inv of
            Just (ItemStack item _) -> itemColor item
            Nothing -> skin
          -- Arm (tall rectangle)
          arm = quad armX armY (armX + 0.2) 1.0 skin
          -- Item block (small square at top of arm)
          itemBlock = quad (armX + 0.02) (armY - 0.12) (armX + 0.18) armY heldColor
      in arm ++ itemBlock

    -- F3 debug overlay: extended debug information
    debugVerts = case debugInfo of
      Nothing -> []
      Just di ->
        let V3 px py pz = dbgPos di
            dc = (1.0, 1.0, 1.0, 0.9)  -- white text
            bgc = (0.0, 0.0, 0.0, 0.5)  -- semi-transparent background
            sc = 0.7 :: Float
            lh = 0.07 :: Float  -- line height
            x0 = -0.98 :: Float -- left margin
            y0 = -0.95 :: Float -- top (Vulkan NDC: -1 = top)
            lines' = [ "FPS: " ++ show (dbgFps di)
                     , "XYZ: " ++ showF1 px ++ " / " ++ showF1 py ++ " / " ++ showF1 pz
                     , "YAW: " ++ showF1 (dbgYaw di) ++ "  PITCH: " ++ showF1 (dbgPitch di)
                     , "CHUNKS: " ++ show (dbgChunkCount di)
                     , "BIOME: " ++ dbgBiome di
                     , "TARGET: " ++ dbgTargetBlock di
                     , "LIGHT: " ++ dbgLightLevel di
                     , "ENTITIES: " ++ show (dbgEntityCount di)
                     , "WEATHER: " ++ dbgWeather di
                     , "MODE: " ++ dbgGameMode di
                     , "HP: " ++ show (dbgHealth di) ++ "/20  HUNGER: " ++ show (dbgHunger di) ++ "/20"
                     , dbgTimeOfDay di ++ " (" ++ showF1 (dbgDayTime di) ++ ")"
                     ]
            bgW = 0.75 :: Float
            bgH = fromIntegral (length lines') * lh + 0.02
            bg = quad x0 y0 (x0 + bgW) (y0 + bgH) bgc
            textVerts = concatMap (\(i, line) ->
              renderText (x0 + 0.01) (y0 + 0.01 + fromIntegral i * lh) sc dc line
              ) (zip [0 :: Int ..] lines')
        in bg ++ textVerts

    -- Block target highlight: project 3D block edges to 2D NDC and render wireframe
    highlightVerts = case targetInfo of
      Nothing -> []
      Just (V3 bx by bz, vpMat) ->
        let -- Block corners in world space (slightly expanded to avoid z-fighting)
            e = 0.002 :: Float  -- expansion
            x0 = fromIntegral bx - e
            y0 = fromIntegral by - e
            z0 = fromIntegral bz - e
            x1 = fromIntegral bx + 1 + e
            y1 = fromIntegral by + 1 + e
            z1 = fromIntegral bz + 1 + e
            -- Project a 3D point to 2D NDC via the VP matrix
            -- Vulkan clip correction: Y-flip is in the projection matrix,
            -- so projected Y is already in Vulkan NDC (-1=top, +1=bottom)
            projectPt (V3 wx wy wz) =
              let V4 cx cy cz cw = vpMat !* V4 wx wy wz 1
              in if cw > 0.01
                 then Just (cx / cw, cy / cw, cz / cw)
                 else Nothing  -- behind camera
            -- 8 corners of the block
            corners =
              [ V3 x0 y0 z0, V3 x1 y0 z0, V3 x1 y0 z1, V3 x0 y0 z1  -- bottom
              , V3 x0 y1 z0, V3 x1 y1 z0, V3 x1 y1 z1, V3 x0 y1 z1  -- top
              ]
            projected = map projectPt corners
            -- 12 edges of a cube (index pairs)
            edges = [ (0,1),(1,2),(2,3),(3,0)   -- bottom face
                    , (4,5),(5,6),(6,7),(7,4)   -- top face
                    , (0,4),(1,5),(2,6),(3,7)   -- vertical edges
                    ]
            -- Draw a thin quad between two NDC points (line thickness in NDC)
            lineColor = (0.1, 0.1, 0.1, 0.6)
            lineT = 0.003 :: Float  -- line thickness in NDC
            drawEdge (i, j) =
              case (projected !! i, projected !! j) of
                (Just (ax, ay, _), Just (bx', by', _)) ->
                  -- Only draw if both points are in front of camera and on-screen
                  if abs ax < 1.5 && abs ay < 1.5 && abs bx' < 1.5 && abs by' < 1.5
                  then
                    let dx = bx' - ax
                        dy = by' - ay
                        len = sqrt (dx * dx + dy * dy)
                        -- Perpendicular direction for thickness
                        (nx, ny) = if len > 0.001
                                   then (-dy * lineT / len, dx * lineT / len)
                                   else (lineT, 0)
                        -- Four corners of the line quad
                        (r, g, b, a) = lineColor
                    in [ ax + nx, ay + ny, r, g, b, a
                       , bx' + nx, by' + ny, r, g, b, a
                       , bx' - nx, by' - ny, r, g, b, a
                       , ax + nx, ay + ny, r, g, b, a
                       , bx' - nx, by' - ny, r, g, b, a
                       , ax - nx, ay - ny, r, g, b, a
                       ]
                  else []
                _ -> []
        in concatMap drawEdge edges

    -- Bed-related message (sleep at night / monsters nearby)
    sleepMsgVerts = case sleepMsgText of
      Just msg ->
          let msgBg = quad (-0.55) 0.25 0.55 0.38 (0.0, 0.0, 0.0, 0.6)
              msgText = renderTextCentered 0.28 1.0 (1, 1, 1, 1) msg
          in msgBg ++ msgText
      Nothing -> []

    -- Damage flash: red overlay that fades out
    damageFlashVerts
      | damageFlash > 0 = quad (-1) (-1) 1 1 (0.8, 0.0, 0.0, damageFlash * 0.5)
      | otherwise = []

    -- Compass/clock info text above hotbar when selected
    compassClockVerts = case getSlot inv sel of
      Just (ItemStack CompassItem _) ->
        let V3 px _ pz = playerPos
            V3 spx _ spz = spawnPos
            dx = spx - px
            dz = spz - pz
            dir
              | abs dx < 1 && abs dz < 1 = "AT SPAWN"
              | abs dx >= abs dz && dx > 0 = "SPAWN: E"
              | abs dx >= abs dz           = "SPAWN: W"
              | dz > 0                     = "SPAWN: S"
              | otherwise                  = "SPAWN: N"
            infoY = hotbarY - 0.09
        in renderTextCentered infoY 0.8 (0.9, 0.3, 0.3, 1) dir
      Just (ItemStack ClockItem _) ->
        let tod = getTimeOfDay dayNight
            t = dncTime dayNight
            todStr = show tod ++ " (" ++ showF2 t ++ ")"
            infoY = hotbarY - 0.09
        in renderTextCentered infoY 0.8 (0.9, 0.8, 0.3, 1) todStr
      _ -> []

    -- Death screen: red overlay, "YOU DIED" text, score, and respawn button
    deathScreenVerts =
      -- Red tinted overlay
      quad (-1) (-1) 1 1 (0.5, 0.0, 0.0, 0.6)
      -- "YOU DIED" text (large)
      ++ renderTextCentered (-0.2) 2.0 (1, 0.2, 0.2, 1) "YOU DIED"
      -- "Score: 0" (no XP system yet)
      ++ renderTextCentered 0.0 1.0 (0.8, 0.8, 0.8, 1) "SCORE: 0"
      -- Respawn button background
      ++ quad (-0.25) 0.15 0.25 0.30 (0.3, 0.3, 0.3, 0.9)
      -- Respawn button text
      ++ renderTextCentered 0.19 1.0 (1, 1, 1, 1) "RESPAWN"

    -- Inventory screen: dark overlay + 4x9 slot grid
    invScreenVerts =
      -- Full-screen dark overlay
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Title
      ++ renderTextCentered (-0.85) 1.0 (1, 1, 1, 1) "INVENTORY"
      -- === TOP ROW: Armor (left) + 2x2 Crafting (center-right) ===
      -- Armor slots background
      ++ quad (-0.55) (-0.82) (-0.30) (-0.40) (0.35, 0.3, 0.35, 0.9)
      ++ renderText (-0.53) (-0.85) 0.5 (0.8, 0.8, 0.8, 0.8) "ARMOR"
      ++ concatMap renderArmorSlot [0..3]
      -- 2x2 crafting grid background
      ++ quad (-0.05) (-0.82) 0.22 (-0.58) (0.35, 0.35, 0.3, 0.9)
      ++ renderText (-0.03) (-0.85) 0.5 (0.8, 0.8, 0.8, 0.8) "CRAFT"
      ++ concatMap renderInvCraftSlot [(r, c) | r <- [0..1], c <- [0..1]]
      -- Arrow
      ++ quad 0.25 (-0.73) 0.30 (-0.69) (1, 1, 1, 0.7)
      -- Output slot
      ++ quad 0.33 (-0.77) 0.47 (-0.63) (0.25, 0.25, 0.2, 0.9)
      ++ renderInvCraftOutput
      -- === BOTTOM: Inventory grid (4 rows x 9 cols) ===
      ++ quad (invGridX0 - 0.02) (invGridY0 - 0.02)
              (invGridX0 + 9 * invSlotW + 0.02) (invGridY0 + 4 * invSlotH + 0.02)
              (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderInvSlot [0..35]
      where
        renderInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = invGridY0 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        -- 2x2 crafting grid positions (above inventory)
        craft2x2X0 = -0.03 :: Float
        craft2x2Y0 = -0.80 :: Float
        craft2x2Sz = 0.10 :: Float

        renderInvCraftSlot (row, col) =
          let x = craft2x2X0 + fromIntegral col * craft2x2Sz + 0.01
              y = craft2x2Y0 + fromIntegral row * craft2x2Sz + 0.01
              sw = craft2x2Sz - 0.02; sh = craft2x2Sz - 0.02
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getCraftingSlot craftGrid row col of
            Nothing -> slotBg
            Just item ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        renderInvCraftOutput =
          let x = 0.35; y = -0.75; sz = 0.10
              slotBg = quad x y (x + sz) (y + sz) (0.2, 0.2, 0.15, 0.9)
          in case tryCraft craftGrid of
            CraftSuccess item count ->
              let colors = itemMiniIcon item
                  pixW = sz / 3; pixH = sz / 3
                  iconVerts = concatMap (\(r, c, clr) ->
                       quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                            (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors
                  countText = if count > 1
                    then renderText (x + sz - 0.03) (y + sz - 0.025) 0.6 (1,1,1,1) (show count)
                    else []
              in slotBg ++ iconVerts ++ countText
            CraftFailure -> slotBg

        armorLabels = ["H", "C", "L", "B"]
        renderArmorSlot idx =
          let x = -0.53; y = -0.78 + fromIntegral idx * 0.10
              sw = 0.08; sh = 0.08
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
              label = renderText (x + 0.02) (y + 0.02) 0.4 (0.5, 0.5, 0.5, 0.5) (armorLabels !! idx)
          in slotBg ++ label

    -- Crafting screen: grid + output + inventory below
    craftScreenVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- 3x3 crafting grid
      ++ quad (-0.32) (-0.37) 0.02 (-0.03) (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderCraftSlot [(r, c) | r <- [0..2], c <- [0..2]]
      -- Arrow
      ++ quad 0.05 (-0.22) 0.12 (-0.18) (1, 1, 1, 0.7)
      -- Output slot
      ++ renderCraftOutput
      -- Inventory grid below
      ++ quad (invGridX0 - 0.02) (0.08) (invGridX0 + 9 * invSlotW + 0.02) (0.08 + 4 * invSlotH + 0.02) (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderCraftInvSlot [0..35]
      where
        renderCraftSlot (row, col) =
          let x = -0.3 + fromIntegral col * 0.1 + 0.005
              y = -0.35 + fromIntegral row * 0.1 + 0.005
              sw = 0.09; sh = 0.09
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getCraftingSlot craftGrid row col of
            Nothing -> slotBg
            Just item ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        renderCraftOutput =
          let x = 0.15; y = -0.27; sw = 0.12; sh = 0.12
              slotBg = quad x y (x + sw) (y + sh) (0.2, 0.2, 0.15, 0.9)
          in case tryCraft craftGrid of
            CraftFailure -> slotBg
            CraftSuccess item _ ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        renderCraftInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = 0.1 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Chest screen: 27 chest slots (3 rows of 9) + 36 player inventory slots below
    chestScreenVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Title label area
      ++ renderTextCentered (-0.45) 1.0 (1, 1, 1, 1) "CHEST"
      -- Chest slots: 3 rows of 9 at top
      ++ quad (invGridX0 - 0.02) (chestGridY0 - 0.02)
              (invGridX0 + 9 * invSlotW + 0.02) (chestGridY0 + 3 * invSlotH + 0.02)
              (0.35, 0.3, 0.2, 0.9)
      ++ concatMap renderChestSlot [0..26]
      -- Player inventory grid below (same layout as regular inventory)
      ++ quad (invGridX0 - 0.02) (chestInvY0 - 0.02)
              (invGridX0 + 9 * invSlotW + 0.02) (chestInvY0 + 4 * invSlotH + 0.02)
              (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderChestInvSlot [0..35]
      where
        chestGridY0 = -0.35 :: Float  -- top edge of chest grid
        chestInvY0  = 0.1 :: Float    -- top edge of player inventory

        renderChestSlot idx =
          let row = idx `div` 9
              col = idx `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = chestGridY0 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.2, 0.18, 0.12, 0.8)
          in case mChestInv of
            Nothing -> slotBg
            Just chestInv -> case getChestSlot chestInv idx of
              Nothing -> slotBg
              Just (ItemStack item _) ->
                let colors = itemMiniIcon item
                    pixW = sw / 3; pixH = sh / 3
                in slotBg ++ concatMap (\(r, c, clr) ->
                     quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                          (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        renderChestInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = chestInvY0 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.2, 0.18, 0.12, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Furnace screen: input slot, fuel slot, output slot, progress arrows, inventory below
    furnaceScreenVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Title
      ++ renderTextCentered (-0.42) 0.9 (1, 1, 1, 1) "FURNACE"
      -- Background panel
      ++ quad (-0.35) (-0.35) 0.35 0.02 (0.3, 0.3, 0.3, 0.9)
      -- Input slot (top-left)
      ++ renderFurnaceSlot furnInputX furnInputY (getFurnaceInput furnaceState)
      -- Fuel slot (bottom-left)
      ++ renderFurnaceSlot furnFuelX furnFuelY (getFurnaceFuel furnaceState)
      -- Output slot (right, larger)
      ++ renderFurnaceOutputSlot
      -- Smelt progress arrow (between input and output)
      ++ furnaceSmeltArrow
      -- Fuel burn indicator (between fuel and input)
      ++ furnaceFuelIndicator
      -- Inventory grid below
      ++ quad (invGridX0 - 0.02) 0.08 (invGridX0 + 9 * invSlotW + 0.02) (0.08 + 4 * invSlotH + 0.02) (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderFurnaceInvSlot [0..35]
      where
        furnInputX = -0.25
        furnInputY = -0.30
        furnFuelX  = -0.25
        furnFuelY  = -0.12
        furnOutX   = 0.1
        furnOutY   = -0.24
        furnSlotSz = 0.1

        renderFurnaceSlot sx sy mStack =
          let slotBg = quad sx sy (sx + furnSlotSz) (sy + furnSlotSz) (0.15, 0.15, 0.15, 0.8)
          in case mStack of
            Nothing -> slotBg
            Just (ItemStack item _cnt) ->
              let colors = itemMiniIcon item
                  pixW = furnSlotSz / 3; pixH = furnSlotSz / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (sx + fromIntegral c * pixW) (sy + fromIntegral r * pixH)
                        (sx + fromIntegral (c+1) * pixW) (sy + fromIntegral (r+1) * pixH) clr) colors

        renderFurnaceOutputSlot =
          let sz = 0.13
              slotBg = quad furnOutX furnOutY (furnOutX + sz) (furnOutY + sz) (0.2, 0.2, 0.15, 0.9)
          in case getFurnaceOutput furnaceState of
            Nothing -> slotBg
            Just (ItemStack item cnt) ->
              let colors = itemMiniIcon item
                  pixW = sz / 3; pixH = sz / 3
                  iconVerts = concatMap (\(r, c, clr) ->
                       quad (furnOutX + fromIntegral c * pixW) (furnOutY + fromIntegral r * pixH)
                            (furnOutX + fromIntegral (c+1) * pixW) (furnOutY + fromIntegral (r+1) * pixH) clr) colors
                  countText = if cnt > 1
                    then renderText (furnOutX + sz - 0.03) (furnOutY + sz - 0.025) 0.6 (1,1,1,1) (show cnt)
                    else []
              in slotBg ++ iconVerts ++ countText

        -- Smelt progress arrow: fills left-to-right
        furnaceSmeltArrow =
          let arrowX = -0.10
              arrowY = -0.25
              arrowW = 0.15
              arrowH = 0.04
              progress = if fsSmeltTime furnaceState > 0
                then case getFurnaceInput furnaceState of
                  Just (ItemStack inputItem _) -> case findRecipe inputItem of
                    Just r  -> fsSmeltTime furnaceState / srTime r
                    Nothing -> 0
                  Nothing -> 0
                else 0
              bg = quad arrowX arrowY (arrowX + arrowW) (arrowY + arrowH) (0.4, 0.4, 0.4, 0.7)
              fill = if progress > 0
                     then quad arrowX arrowY (arrowX + arrowW * min 1 progress) (arrowY + arrowH) (1, 1, 1, 0.9)
                     else []
          in bg ++ fill

        -- Fuel burn indicator: fills bottom-to-top
        furnaceFuelIndicator =
          let indX = -0.22
              indY = -0.07
              indW = 0.04
              indH = 0.06
              progress = if fsMaxFuelTime furnaceState > 0
                         then fsFuelTime furnaceState / fsMaxFuelTime furnaceState
                         else 0
              bg = quad indX indY (indX + indW) (indY + indH) (0.3, 0.15, 0.1, 0.7)
              fill = if progress > 0
                     then let fillH = indH * min 1 progress
                          in quad indX (indY + indH - fillH) (indX + indW) (indY + indH) (1.0, 0.6, 0.1, 0.9)
                     else []
          in bg ++ fill

        renderFurnaceInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = 0.1 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Dispenser screen: 3x3 grid of 9 slots + player inventory below
    dispenserScreenVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Title label area
      ++ renderTextCentered (-0.45) 1.0 (1, 1, 1, 1) "DISPENSER"
      -- Dispenser slots: 3 rows of 3 at top
      ++ quad (dispGridX0 - 0.02) (dispGridY0 - 0.02)
              (dispGridX0 + 3 * invSlotW + 0.02) (dispGridY0 + 3 * invSlotH + 0.02)
              (0.35, 0.3, 0.3, 0.9)
      ++ concatMap renderDispSlot [0..8]
      -- Player inventory grid below
      ++ quad (invGridX0 - 0.02) (dispInvY0 - 0.02)
              (invGridX0 + 9 * invSlotW + 0.02) (dispInvY0 + 4 * invSlotH + 0.02)
              (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderDispInvSlot [0..35]
      where
        dispGridX0 = -0.2 :: Float   -- centered 3-wide grid
        dispGridY0 = -0.35 :: Float  -- top edge of dispenser grid
        dispInvY0  = 0.1 :: Float    -- top edge of player inventory

        renderDispSlot idx =
          let row = idx `div` 3
              col = idx `mod` 3
              x = dispGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = dispGridY0 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.2, 0.18, 0.15, 0.8)
          in case mDispInv of
            Nothing -> slotBg
            Just dispInv -> case getDispenserSlot dispInv idx of
              Nothing -> slotBg
              Just (ItemStack item _) ->
                let colors = itemMiniIcon item
                    pixW = sw / 3; pixH = sh / 3
                in slotBg ++ concatMap (\(r, c, clr) ->
                     quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                          (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

        renderDispInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = dispInvY0 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.2, 0.18, 0.12, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Enchanting screen
    enchantScreenVerts =
      -- Dark semi-transparent background
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Title
      ++ renderTextCentered (-0.42) 0.9 (0.7, 0.3, 1.0, 1.0) "ENCHANTING TABLE"
      -- Background panel
      ++ quad (-0.35) (-0.35) 0.65 0.15 (0.15, 0.05, 0.25, 0.9)
      -- Item slot (left side)
      ++ quad enchItemX0 enchItemY0 (enchItemX0 + enchItemSz) (enchItemY0 + enchItemSz) (0.15, 0.15, 0.15, 0.8)
      -- XP level display (below item slot)
      ++ renderText (-0.26) (-0.02) 0.6 (0.5, 1.0, 0.5, 1.0) ("XP: " ++ show (0 :: Int))
      -- 3 enchantment option buttons (right side)
      ++ enchantOptionVerts
      -- Inventory grid below
      ++ quad (invGridX0 - 0.02) 0.20 (invGridX0 + 9 * invSlotW + 0.02) (0.20 + 4 * invSlotH + 0.02) (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderEnchantInvSlot [0..35]
      where
        enchItemX0 = -0.26 :: Float
        enchItemY0 = -0.16 :: Float
        enchItemSz = 0.12 :: Float

        optX0 = 0.05 :: Float
        optW  = 0.50 :: Float
        optH  = 0.10 :: Float
        optGap = 0.02 :: Float

        enchantOptionVerts = concatMap makeOption [0, 1, 2]
        makeOption i =
          let y0 = -0.25 + fromIntegral i * (optH + optGap)
              bg = quad optX0 y0 (optX0 + optW) (y0 + optH) (0.2, 0.1, 0.35, 0.9)
              label = renderText (optX0 + 0.02) (y0 + 0.02) 0.55 (0.9, 0.8, 1.0, 1.0)
                        ("Option " ++ show (i + 1 :: Int))
          in bg ++ label

        renderEnchantInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = 0.22 + fromIntegral row * invSlotH + invSlotPad
              sw = invSlotW - 2 * invSlotPad
              sh = invSlotH - 2 * invSlotPad
              slotBg = quad x y (x + sw) (y + sh) (0.15, 0.15, 0.15, 0.8)
          in case getSlot inv idx of
            Nothing -> slotBg
            Just (ItemStack item _) ->
              let colors = itemMiniIcon item
                  pixW = sw / 3; pixH = sh / 3
              in slotBg ++ concatMap (\(r, c, clr) ->
                   quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                        (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Cursor item (follows mouse position — simplified to center for now)
    cursorVerts = case cursorItem of
      Nothing -> []
      Just (ItemStack item _) ->
        let colors = itemMiniIcon item
            sw = 0.08; sh = 0.08
            x = mouseX - sw / 2; y = mouseY - sh / 2
            pixW = sw/3; pixH = sh/3
        in concatMap (\(r, c, clr) ->
             quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                  (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors

    -- Main menu screen
    menuVerts =
      -- Dark background
      quad (-1) (-1) 1 1 (0.1, 0.12, 0.15, 1.0)
      -- Title
      ++ quad (-0.4) (-0.6) 0.4 (-0.4) (0.2, 0.5, 0.3, 0.9)
      ++ renderTextCentered (-0.54) 1.5 (1, 1, 1, 1) "MINECRAFT"
      -- "New World" button (green)
      ++ quad (-0.3) (-0.12) 0.3 0.07 (0.3, 0.6, 0.35, 0.9)
      ++ quad (-0.28) (-0.1) 0.28 0.05 (0.25, 0.5, 0.3, 0.9)
      ++ renderTextCentered (-0.06) 1.0 (1, 1, 1, 1) "NEW WORLD"
      -- "Load World" button (blue)
      ++ quad (-0.3) 0.1 0.3 0.27 (0.35, 0.35, 0.6, 0.9)
      ++ quad (-0.28) 0.12 0.28 0.25 (0.3, 0.3, 0.5, 0.9)
      ++ renderTextCentered 0.15 1.0 (1, 1, 1, 1) "LOAD WORLD"
      -- "Quit" button (red)
      ++ quad (-0.3) 0.32 0.3 0.49 (0.6, 0.3, 0.3, 0.9)
      ++ quad (-0.28) 0.34 0.28 0.47 (0.5, 0.25, 0.25, 0.9)
      ++ renderTextCentered 0.37 1.0 (1, 1, 1, 1) "QUIT"

    -- Pause screen: semi-transparent overlay with Resume / Save+Quit / Quit
    pauseVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.65)
      ++ renderTextCentered (-0.35) 1.5 (1, 1, 1, 1) "PAUSED"
      -- Resume button (green)
      ++ quad (-0.3) (-0.2) 0.3 0.0 (0.2, 0.55, 0.25, 1.0)
      ++ quad (-0.28) (-0.18) 0.28 (-0.02) (0.15, 0.45, 0.2, 1.0)
      ++ renderTextCentered (-0.14) 1.0 (1, 1, 1, 1) "RESUME"
      -- Save & Quit to Menu (blue)
      ++ quad (-0.3) 0.05 0.3 0.25 (0.25, 0.25, 0.55, 1.0)
      ++ quad (-0.28) 0.07 0.28 0.23 (0.2, 0.2, 0.45, 1.0)
      ++ renderTextCentered 0.11 1.0 (1, 1, 1, 1) "SAVE AND QUIT"
      -- Quit Game (red)
      ++ quad (-0.3) 0.3 0.3 0.5 (0.55, 0.2, 0.2, 1.0)
      ++ quad (-0.28) 0.32 0.28 0.48 (0.45, 0.15, 0.15, 1.0)
      ++ renderTextCentered 0.37 1.0 (1, 1, 1, 1) "QUIT GAME"

-- | 6-connected neighbor offsets (shared by redstone-driven updates)
neighborDirs :: [V3 Int]
neighborDirs = [ V3 1 0 0, V3 (-1) 0 0
               , V3 0 1 0, V3 0 (-1) 0
               , V3 0 0 1, V3 0 0 (-1) ]

-- | Update iron doors adjacent to a redstone source position.
--   If powered > 0, open the door; if power drops to 0, close it.
updateIronDoors
  :: World -> RedstoneState -> V3 Int
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
updateIronDoors world rsState sourcePos physDevice device cmdPool queue meshCacheRef = do
  forM_ [ sourcePos + d | d <- neighborDirs ] $ \nPos@(V3 nx _ny nz) -> do
    blk <- worldGetBlock world nPos
    power <- getPower rsState nPos
    case blk of
      IronDoorClosed | power > 0 -> do
        worldSetBlock world nPos IronDoorOpen
        rebuildChunkAt world physDevice device cmdPool queue meshCacheRef nx nz
      IronDoorOpen | power == 0 -> do
        worldSetBlock world nPos IronDoorClosed
        rebuildChunkAt world physDevice device cmdPool queue meshCacheRef nx nz
      _ -> pure ()

-- | Update pistons adjacent to a redstone source position.
--   Pistons face +Y (push upward). Cannot push Bedrock or Obsidian. Max 12 blocks.
updatePistons
  :: World -> RedstoneState -> V3 Int
  -> Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
  -> IO ()
updatePistons world rsState sourcePos physDevice device cmdPool queue meshCacheRef = do
  forM_ [ sourcePos + d | d <- neighborDirs ] $ \nPos@(V3 nx _ny nz) -> do
    blk <- worldGetBlock world nPos
    case blk of
      Piston -> do
        power <- getPower rsState nPos
        let above = nPos + V3 0 1 0
        aboveBlk <- worldGetBlock world above
        if power > 0 && aboveBlk /= PistonHead
          then do
            chainLen <- tryPistonPush world nPos (V3 0 1 0) 12
            when (chainLen >= 0) $ do
              worldSetBlock world above PistonHead
              rebuildChunkAt world physDevice device cmdPool queue meshCacheRef nx nz
              -- Only rebuild chunks that actually had blocks displaced
              forM_ [1..chainLen + 1] $ \dy -> do
                let V3 rx _ rz = nPos + V3 0 (dy + 1) 0
                rebuildChunkAt world physDevice device cmdPool queue meshCacheRef rx rz
          else when (power == 0 && aboveBlk == PistonHead) $ do
            let aboveHead = above + V3 0 1 0
            headAboveBlk <- worldGetBlock world aboveHead
            worldSetBlock world above Air
            when (headAboveBlk /= Air && not (isImmovable headAboveBlk)) $ do
              worldSetBlock world above headAboveBlk
              worldSetBlock world aboveHead Air
            rebuildChunkAt world physDevice device cmdPool queue meshCacheRef nx nz
            let V3 rx _ rz = aboveHead
            rebuildChunkAt world physDevice device cmdPool queue meshCacheRef rx rz
      _ -> pure ()

-- | Check if a block cannot be pushed by a piston
isImmovable :: BlockType -> Bool
isImmovable Bedrock    = True
isImmovable Obsidian   = True
isImmovable PistonHead = True
isImmovable _          = False

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
  let neighbors = [ sourcePos + d | d <- [ V3 1 0 0, V3 (-1) 0 0
                                          , V3 0 1 0, V3 0 (-1) 0
                                          , V3 0 0 1, V3 0 0 (-1) ] ]
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

-- | Get a display color for an item (used for hotbar slot rendering)
  where fillSolid c = [(r,col,c) | r <- [0..2], col <- [0..2]]

-- | Build a SaveData record from current game state
buildSaveData :: Player -> Inventory -> DayNightCycle -> SaveData
buildSaveData player inv dayNight =
  let V3 px py pz = plPos player
  in SaveData
    { sdPlayerPos    = (px, py, pz)
    , sdPlayerYaw    = plYaw player
    , sdPlayerPitch  = plPitch player
    , sdPlayerFlying = plFlying player
    , sdWorldSeed    = 12345
    , sdDayTime      = dncTime dayNight
    , sdDayCount     = dncDayCount dayNight
    , sdHealth       = plHealth player
    , sdHunger       = plHunger player
    , sdFallDist     = plFallDist player
    , sdInventory    = inventoryToSlotList inv
    , sdSelectedSlot = invSelected inv
    }

-- | Reconstruct a Player from SaveData
playerFromSaveData :: SaveData -> Player
playerFromSaveData sd =
  let (px, py, pz) = sdPlayerPos sd
  in Player
    { plPos       = V3 px py pz
    , plVelocity  = V3 0 0 0
    , plYaw       = sdPlayerYaw sd
    , plPitch     = sdPlayerPitch sd
    , plOnGround  = False
    , plFlying    = sdPlayerFlying sd
    , plSprinting = False
    , plHealth    = sdHealth sd
    , plHunger    = sdHunger sd
    , plFallDist  = sdFallDist sd
    , plEatingTimer = 0.0
    , plArmorSlots = [Nothing, Nothing, Nothing, Nothing]
    , plAirSupply = maxAirSupply
    , plSaturation = defaultSaturation
    }

-- | Restore player, inventory, and day/night state from SaveData into IORefs
restoreFromSave :: IORef Player -> IORef Inventory -> IORef DayNightCycle -> SaveData -> IO ()
restoreFromSave playerRef inventoryRef dayNightRef sd = do
  writeIORef playerRef (playerFromSaveData sd)
  writeIORef inventoryRef (slotListToInventory (sdInventory sd) (sdSelectedSlot sd))
  writeIORef dayNightRef (newDayNightCycle { dncTime = sdDayTime sd, dncDayCount = sdDayCount sd })

-- | Clamp a value between a minimum and maximum
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

