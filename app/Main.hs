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
import World.Block (BlockType(..), BlockProperties(..), blockProperties, isSolid)
import World.Chunk
import World.Generation
import World.World
import Game.Player
import Game.Physics (BlockQuery)
import Game.Inventory
import Game.Item
import Game.Crafting
import Game.DayNight
import World.Fluid
import World.Light
import Entity.ECS
import Entity.Mob (MobType(..), MobInfo(..), updateMobAI, AIState(..), mobInfo, damageEntity)
import Entity.Spawn
import Game.Save
import Game.DroppedItem
import Game.Particle
import Game.TileEntity hiding (BlastFurnace)
import qualified Game.TileEntity as TE

import Control.Monad (unless, when, forM_)
import Data.Maybe (isNothing, catMaybes)
import Control.Concurrent.STM (readTVarIO, atomically, writeTVar)
import Control.Exception (finally)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import qualified Data.HashMap.Strict as HM
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
import System.Environment (getArgs)
import Data.Char (isDigit)

-- | Game UI mode
data GameMode = MainMenu | Playing | Paused | InventoryOpen | CraftingOpen | FurnaceOpen
  deriving stock (Show, Eq)

-- | Fixed timestep for physics (20 ticks per second, like Minecraft)
tickRate :: Float
tickRate = 1.0 / 20.0

-- | Max reach distance for block interaction
maxReach :: Float
maxReach = 5.0

-- | Default save directory root
savesRoot :: FilePath
savesRoot = "saves"

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
    saveDirRef <- newIORef (savesRoot </> "world1")
    let defaultSaveDir = savesRoot </> "world1"
    mSavedPlayer <- loadPlayer defaultSaveDir
    playerRef <- newIORef (case mSavedPlayer of
      Just p  -> p
      Nothing -> defaultPlayer spawnPos)
    inventoryRef <- newIORef emptyInventory
    gameModeRef <- newIORef MainMenu
    cursorItemRef <- newIORef (Nothing :: Maybe ItemStack)  -- item held by mouse cursor
    craftingGridRef <- newIORef (emptyCraftingGrid 3)
    debugOverlayRef <- newIORef False
    dayNightRef <- newIORef newDayNightCycle
    fluidState <- newFluidState
    droppedItems <- newDroppedItems

    -- Entity system
    entityWorld <- newEntityWorld
    spawnRngRef <- newIORef =<< System.Random.newStdGen
    spawnCooldownRef <- newIORef (0.0 :: Float)
    aiStatesRef <- newIORef (HM.empty :: HM.HashMap Int AIState)
    particleSys <- newParticleSystem
    damageFlashRef <- newIORef (0.0 :: Float)  -- remaining flash time
    lastHealthRef <- newIORef (20 :: Int)       -- track health changes for flash trigger
    bobTimerRef <- newIORef (0.0 :: Float)      -- camera walk bob timer
    tileEntityRef <- newTileEntityMap             -- furnaces, hoppers, etc.
    openFurnacePosRef <- newIORef (Nothing :: Maybe (V3 Int))  -- which furnace is open

    -- Give player some starting blocks
    let startInv = selectHotbar (foldl (\i (item, cnt) -> fst $ addItem i item cnt) emptyInventory
          [ (ToolItem Pickaxe Wood 59, 1)
          , (ToolItem Sword Wood 59, 1)
          , (BlockItem Stone, 64)
          , (BlockItem Dirt, 64)
          , (BlockItem OakPlanks, 64)
          , (BlockItem Torch, 16)
          , (BlockItem CraftingTable, 1)
          , (BlockItem Furnace, 1)
          , (BlockItem CoalOre, 16)
          ]) 2  -- select first block slot
    writeIORef inventoryRef startInv

    -- Try to load saved world, otherwise generate fresh
    loaded <- loadWorld defaultSaveDir world
    if loaded
      then do
        loadTileEntities defaultSaveDir tileEntityRef
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
    hudVertCountRef <- newIORef (0 :: Int)

    -- Per-chunk mesh cache: ChunkPos -> (vertBuf, idxBuf, indexCount)
    meshCacheRef <- newIORef (HM.empty :: HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int))
    rebuildAllChunkMeshes world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef

    -- Input state
    inputRef <- newIORef noInput
    lastCursorRef <- newIORef (Nothing :: Maybe (Double, Double))

    -- Track mouse position for UI clicks
    mousePosRef <- newIORef (0.0 :: Double, 0.0 :: Double)

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
        modifyIORef' inventoryRef (`selectHotbar` newSlot)

    -- Mining state
    miningRef <- newIORef (Nothing :: Maybe (V3 Int, Float))
    lmbHeldRef <- newIORef False

    -- Mouse button callback
    GLFW.setMouseButtonCallback (whWindow wh) $ Just $ \_win button action _mods -> do
      mode <- readIORef gameModeRef
      case mode of
        MainMenu -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowScreenSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
          -- "New World" button
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= -0.12 && ndcY <= 0.07) $ do
            newName <- nextSaveName
            let newDir = savesRoot </> newName
            writeIORef saveDirRef newDir
            -- Reset world chunks
            atomically $ writeTVar (worldChunks world) HM.empty
            -- Generate fresh chunks around spawn
            _ <- updateLoadedChunks world spawnPos
            -- Reset player and tile entities
            writeIORef playerRef (defaultPlayer spawnPos)
            writeIORef tileEntityRef HM.empty
            -- Rebuild chunk meshes
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
                let saveName = last saves
                    sd = savesRoot </> saveName
                writeIORef saveDirRef sd
                atomically $ writeTVar (worldChunks world) HM.empty
                ldOk <- loadWorld sd world
                when ldOk $ do
                  mPlayer <- loadPlayer sd
                  case mPlayer of
                    Just p -> writeIORef playerRef p
                    Nothing -> pure ()
                  loadTileEntities sd tileEntityRef
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
          (winW, winH) <- getWindowScreenSize wh
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
            sd <- readIORef saveDirRef
            savePlayer sd player
            saveWorld sd world
            saveTileEntities sd tileEntityRef
            putStrLn "World saved."
            writeIORef gameModeRef MainMenu
          -- Quit Game button
          when (ndcX >= -0.3 && ndcX <= 0.3 && ndcY >= 0.3 && ndcY <= 0.5) $
            GLFW.setWindowShouldClose (whWindow wh) True

        InventoryOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowScreenSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              -- Check which inventory slot was clicked
              mSlot = hitInventorySlot ndcX ndcY
          case mSlot of
            Nothing -> pure ()
            Just slotIdx -> do
              inv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot inv slotIdx
              -- Swap cursor and slot
              writeIORef inventoryRef (setSlot inv slotIdx cursor)
              writeIORef cursorItemRef slotContent

        CraftingOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowScreenSize wh
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
                Just (ItemStack item n) | n > 1 -> do
                  writeIORef craftingGridRef (setCraftingSlot grid row col (Just item))
                  writeIORef cursorItemRef (Just (ItemStack item (n - 1)))
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
                      writeIORef craftingGridRef (emptyCraftingGrid 3)
                    Just (ItemStack ci cc)
                      | ci == item && cc + count <= itemStackLimit item -> do
                          writeIORef cursorItemRef (Just (ItemStack item (cc + count)))
                          writeIORef craftingGridRef (emptyCraftingGrid 3)
                      | otherwise -> pure ()  -- cursor full or different item
                CraftFailure -> pure ()
            Just (CraftInvSlot idx) -> do
              inv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent = getSlot inv idx
              writeIORef inventoryRef (setSlot inv idx cursor)
              writeIORef cursorItemRef slotContent
            Nothing -> pure ()

        FurnaceOpen -> when (action == GLFW.MouseButtonState'Pressed && button == GLFW.MouseButton'1) $ do
          (mx, my) <- readIORef mousePosRef
          (winW, winH) <- getWindowScreenSize wh
          let ndcX = realToFrac mx / fromIntegral winW * 2.0 - 1.0 :: Float
              ndcY = realToFrac my / fromIntegral winH * 2.0 - 1.0 :: Float
              mSlot = hitFurnaceSlot ndcX ndcY
          mFurnacePos <- readIORef openFurnacePosRef
          case (mSlot, mFurnacePos) of
            (Just FurnaceInput, Just fPos) -> do
              cursor <- readIORef cursorItemRef
              mTE <- getTileEntity tileEntityRef fPos
              case mTE of
                Just (TEFurnace fs) -> do
                  let slotContent = fsInput fs
                      sameItem = case (cursor, fsInput fs) of
                        (Just (ItemStack ci _), Just (ItemStack fi _)) -> ci == fi
                        _ -> False
                      newProgress = if sameItem then fsSmeltProgress fs else 0
                  setTileEntity tileEntityRef fPos (TEFurnace fs { fsInput = cursor, fsSmeltProgress = newProgress })
                  writeIORef cursorItemRef slotContent
                _ -> pure ()
            (Just FurnaceFuel, Just fPos) -> do
              cursor <- readIORef cursorItemRef
              mTE <- getTileEntity tileEntityRef fPos
              case mTE of
                Just (TEFurnace fs) -> do
                  case cursor of
                    Just (ItemStack fItem _) | isNothing (fuelBurnTime fItem) -> pure ()
                    _ -> do
                      let slotContent = fsFuel fs
                      setTileEntity tileEntityRef fPos (TEFurnace fs { fsFuel = cursor })
                      writeIORef cursorItemRef slotContent
                _ -> pure ()
            (Just FurnaceOutput, Just fPos) -> do
              cursor <- readIORef cursorItemRef
              mTE <- getTileEntity tileEntityRef fPos
              case mTE of
                Just (TEFurnace fs) -> case (cursor, fsOutput fs) of
                  (Nothing, Just outStack) -> do
                    writeIORef cursorItemRef (Just outStack)
                    setTileEntity tileEntityRef fPos (TEFurnace fs { fsOutput = Nothing })
                  (Just (ItemStack ci cc), Just (ItemStack oi oc))
                    | ci == oi && cc + oc <= itemStackLimit ci -> do
                        writeIORef cursorItemRef (Just (ItemStack ci (cc + oc)))
                        setTileEntity tileEntityRef fPos (TEFurnace fs { fsOutput = Nothing })
                    | otherwise -> pure ()
                  _ -> pure ()
                _ -> pure ()
            (Just (FurnaceInvSlot idx), _) -> do
              inv <- readIORef inventoryRef
              cursor <- readIORef cursorItemRef
              let slotContent' = getSlot inv idx
              writeIORef inventoryRef (setSlot inv idx cursor)
              writeIORef cursorItemRef slotContent'
            _ -> pure ()

        Playing -> do
          when (button == GLFW.MouseButton'1) $
            writeIORef lmbHeldRef (action == GLFW.MouseButtonState'Pressed)
          -- Melee attack: when LMB pressed and holding a sword, damage nearest entity
          when (button == GLFW.MouseButton'1 && action == GLFW.MouseButtonState'Pressed) $ do
            inv <- readIORef inventoryRef
            case selectedItem inv of
              Just (ItemStack (ToolItem Sword material _) _) -> do
                player <- readIORef playerRef
                let attackDmg = tiAttackDamage (toolInfo material)
                nearby <- entitiesInRange entityWorld (plPos player) 3.0
                case nearby of
                  [] -> pure ()
                  _  -> do
                    let closest = foldr1 (\a b -> if distance (entPosition a) (plPos player)
                                                    < distance (entPosition b) (plPos player)
                                                  then a else b) nearby
                    updateEntity entityWorld (entId closest) (\e -> damageEntity e attackDmg)
                    putStrLn $ "Attacked " ++ entTag closest ++ " for " ++ show attackDmg ++ " damage!"
              _ -> pure ()

      when (button == GLFW.MouseButton'1 && action == GLFW.MouseButtonState'Released) $
        writeIORef miningRef Nothing  -- stop mining on release

      when (mode == Playing && action == GLFW.MouseButtonState'Pressed) $ do
        player <- readIORef playerRef
        let eyePos = plPos player + V3 0 1.62 0
            dir = dirFromPlayer player
            blockQueryCb bx by bz = do
              bt <- worldGetBlock world (V3 bx by bz)
              pure (World.Block.isSolid bt)

        mHit <- raycastBlock blockQueryCb eyePos dir maxReach
        case mHit of
          Nothing -> pure ()
          Just hit -> do
            let V3 bx by bz = rhBlockPos hit
            case button of
              GLFW.MouseButton'1 -> do  -- Left click: start mining
                writeIORef miningRef (Just (V3 bx by bz, 0.0))
              GLFW.MouseButton'2 -> do  -- Right click: interact or place
                -- Check if clicking on an interactive block
                hitBlock <- worldGetBlock world (V3 bx by bz)
                if hitBlock == CraftingTable
                  then do
                    writeIORef gameModeRef CraftingOpen
                    writeIORef craftingGridRef (emptyCraftingGrid 3)
                    GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                    writeIORef lastCursorRef Nothing
                else if hitBlock `elem` [Furnace, BlastFurnace, Smoker]
                  then do
                    writeIORef gameModeRef FurnaceOpen
                    writeIORef openFurnacePosRef (Just (V3 bx by bz))
                    -- Create tile entity if it doesn't exist yet
                    mTE <- getTileEntity tileEntityRef (V3 bx by bz)
                    when (isNothing mTE) $ do
                      let variant = case hitBlock of
                            BlastFurnace -> TE.BlastFurnace
                            Smoker       -> TE.SmokerFurnace
                            _            -> NormalFurnace
                      setTileEntity tileEntityRef (V3 bx by bz) (TEFurnace (newFurnaceState variant))
                    GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
                    writeIORef lastCursorRef Nothing
                  else do
                    -- Place block from hotbar
                    inv <- readIORef inventoryRef
                    case selectedItem inv of
                      Nothing -> pure ()
                      Just (ItemStack item _) -> case itemToBlock item of
                        Nothing -> pure ()
                        Just bt -> do
                          let V3 nx ny nz = rhFaceNormal hit
                              placePos = V3 (bx + nx) (by + ny) (bz + nz)
                          let (inv', removed) = removeItem inv item 1
                          when (removed > 0) $ do
                            writeIORef inventoryRef inv'
                            worldSetBlock world placePos bt
                            -- Create tile entity for furnace/hopper blocks
                            case bt of
                              Furnace -> setTileEntity tileEntityRef placePos
                                           (TEFurnace (newFurnaceState NormalFurnace))
                              BlastFurnace -> setTileEntity tileEntityRef placePos
                                                (TEFurnace (newFurnaceState TE.BlastFurnace))
                              Smoker -> setTileEntity tileEntityRef placePos
                                          (TEFurnace (newFurnaceState TE.SmokerFurnace))
                              Hopper -> do
                                let hopperDir = faceToHopperDirection (rhFaceNormal hit)
                                setTileEntity tileEntityRef placePos (TEHopper (newHopperState hopperDir))
                              _ -> pure ()
                            putStrLn $ "Placed " ++ show bt ++ " at " ++ show placePos
                            let V3 px' _ pz' = placePos
                            rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef px' pz'
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
              GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Normal
              writeIORef lastCursorRef Nothing
            GLFW.Key'1 -> modifyIORef' inventoryRef (`selectHotbar` 0)
            GLFW.Key'2 -> modifyIORef' inventoryRef (`selectHotbar` 1)
            GLFW.Key'3 -> modifyIORef' inventoryRef (`selectHotbar` 2)
            GLFW.Key'4 -> modifyIORef' inventoryRef (`selectHotbar` 3)
            GLFW.Key'5 -> modifyIORef' inventoryRef (`selectHotbar` 4)
            GLFW.Key'6 -> modifyIORef' inventoryRef (`selectHotbar` 5)
            GLFW.Key'7 -> modifyIORef' inventoryRef (`selectHotbar` 6)
            GLFW.Key'8 -> modifyIORef' inventoryRef (`selectHotbar` 7)
            GLFW.Key'9 -> modifyIORef' inventoryRef (`selectHotbar` 8)
            GLFW.Key'F5 -> do
              player <- readIORef playerRef
              sd <- readIORef saveDirRef
              savePlayer sd player
              saveWorld sd world
              saveTileEntities sd tileEntityRef
              putStrLn "Quick-saved!"
            GLFW.Key'F9 -> do
              sd <- readIORef saveDirRef
              mPlayer <- loadPlayer sd
              case mPlayer of
                Just p -> do
                  writeIORef playerRef p
                  loadTileEntities sd tileEntityRef
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
          _ -> do
            let closeInventoryUI = do
                  writeIORef gameModeRef Playing
                  writeIORef openFurnacePosRef Nothing
                  GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled
                  writeIORef lastCursorRef Nothing
                  -- Return cursor item to inventory if any
                  mCursor <- readIORef cursorItemRef
                  case mCursor of
                    Just (ItemStack item cnt) -> do
                      modifyIORef' inventoryRef (\inv -> fst $ addItem inv item cnt)
                      writeIORef cursorItemRef Nothing
                    Nothing -> pure ()
                  -- Return crafting grid items to inventory when closing crafting table
                  when (mode == CraftingOpen) $ do
                    cGrid <- readIORef craftingGridRef
                    let size = cgSize cGrid
                        gridItems = [ item | r <- [0..size-1], c <- [0..size-1]
                                           , Just item <- [getCraftingSlot cGrid r c] ]
                    modifyIORef' inventoryRef (\inv ->
                      foldl (\i item -> fst $ addItem i item 1) inv gridItems)
                    writeIORef craftingGridRef (emptyCraftingGrid 3)
            case key of
              GLFW.Key'Escape -> closeInventoryUI
              GLFW.Key'E      -> closeInventoryUI
              _               -> pure ()

    -- Timing
    frameRef <- newIORef (0 :: Int)
    lastTimeRef <- newIORef =<< maybe 0 id <$> GLFW.getTime
    accumRef <- newIORef (0.0 :: Float)

    -- Block queries for physics
    let blockQuery :: BlockQuery
        blockQuery bx by bz = do
          bt <- worldGetBlock world (V3 bx by bz)
          pure (World.Block.isSolid bt)

    let waterQuery :: BlockQuery
        waterQuery bx by bz = do
          bt <- worldGetBlock world (V3 bx by bz)
          pure (bt == Water)

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
            when (gameMode == Playing) $
              playerLoop input blockQuery waterQuery accumRef accum' playerRef

            -- Check for player death → respawn
            do p <- readIORef playerRef
               when (isPlayerDead p) $ do
                 putStrLn "You died! Respawning..."
                 writeIORef playerRef (respawnPlayer spawnPos p)

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
                            Just (ItemStack (ToolItem tt tm _) _)
                              | blockPreferredTool bt == Just tt -> toolMiningSpeed tt tm
                            _ -> 1.0  -- hand speed
                          progressPerSec = toolSpeed / hardness
                          newProgress = progress + progressPerSec * dt
                      if newProgress >= 1.0
                        then do
                          -- Block is broken!
                          worldSetBlock world blockPos Air
                          when (bt == Water || bt == Lava) $
                            removeFluid fluidState world blockPos
                          -- Drop items as entities in the world
                          let drops = blockDrops bt
                              V3 bxf byf bzf = fmap fromIntegral blockPos :: V3 Float
                          mapM_ (\(item, cnt) -> spawnDrop droppedItems item cnt (V3 bxf byf bzf)) drops
                          -- Spawn break particles
                          let (pr, pg, pb, pa) = itemColor (BlockItem bt)
                          spawnBlockBreakParticles particleSys (V3 bxf byf bzf) (pr, pg, pb, pa)
                          -- Drop tile entity contents if any
                          mTE <- getTileEntity tileEntityRef blockPos
                          case mTE of
                            Just (TEFurnace fs) -> do
                              let teDrops = catMaybes [fsInput fs, fsFuel fs, fsOutput fs]
                              mapM_ (\(ItemStack di dc) -> spawnDrop droppedItems di dc (V3 bxf byf bzf)) teDrops
                              removeTileEntity tileEntityRef blockPos
                            Just (TEHopper hs) -> do
                              let teDrops = catMaybes (V.toList (hsSlots hs))
                              mapM_ (\(ItemStack di dc) -> spawnDrop droppedItems di dc (V3 bxf byf bzf)) teDrops
                              removeTileEntity tileEntityRef blockPos
                            Nothing -> pure ()
                          -- Consume tool durability
                          inv' <- readIORef inventoryRef
                          let inv'' = case getSlot inv' (invSelected inv') of
                                Just (ItemStack (ToolItem tt tm dur) 1)
                                  | dur <= 1  -> setSlot inv' (invSelected inv') Nothing
                                  | otherwise -> setSlot inv' (invSelected inv') (Just (ItemStack (ToolItem tt tm (dur - 1)) 1))
                                _ -> inv'
                          writeIORef inventoryRef inv''
                          putStrLn $ "Broke " ++ show bt ++ " at " ++ show blockPos
                          rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
                          writeIORef miningRef Nothing
                        else writeIORef miningRef (Just (blockPos, newProgress))

            -- Update day/night cycle
            modifyIORef' dayNightRef (updateDayNight dt)

            -- Tick fluid simulation every physics update
            tickFluids fluidState world

            -- Tick tile entities (furnaces smelt, hoppers transfer)
            tickTileEntities dt tileEntityRef
            frameIdxTE <- readIORef frameRef
            when (frameIdxTE `mod` 8 == 0) $
              tickHoppers tileEntityRef (worldGetBlock world)

            -- Update dropped items (gravity, friction) and auto-collect nearby
            updateDroppedItems dt droppedItems
            do player' <- readIORef playerRef
               collected <- collectNearby droppedItems (plPos player') 2.5
               unless (null collected) $ do
                 inv' <- readIORef inventoryRef
                 let inv'' = foldl (\i (item, cnt) -> fst $ addItem i item cnt) inv' collected
                 writeIORef inventoryRef inv''

            -- Entity spawning (every ~2 seconds)
            frameIdx <- readIORef frameRef
            when (frameIdx `mod` 120 == 0) $ do
              player <- readIORef playerRef
              dayNight <- readIORef dayNightRef
              _ <- trySpawnMobs defaultSpawnRules entityWorld dayNight blockQuery (plPos player) spawnRngRef
              pure ()

            -- Update mob AI (every 3 frames)
            when (frameIdx `mod` 3 == 0) $ do
              player <- readIORef playerRef
              ents <- livingEntities entityWorld
              aiStates <- readIORef aiStatesRef
              forM_ ents $ \ent -> do
                let eid = entId ent
                    currentAI = HM.lookupDefault (AIIdle 2.0) eid aiStates
                (ent', newAI) <- updateMobAI dt ent (readMobType (entTag ent)) (plPos player) blockQuery currentAI spawnRngRef
                updateEntity entityWorld eid (const ent')
                modifyIORef' aiStatesRef (HM.insert eid newAI)
                -- Apply mob attack damage to player
                case (currentAI, newAI) of
                  (AIAttack _ cd, AIAttack _ 1.0) | cd <= 0 -> do
                    let dmg = floor $ miAttackDmg (mobInfo (readMobType (entTag ent)))
                    when (dmg > 0) $ do
                      modifyIORef' playerRef (damagePlayer dmg)
                      putStrLn $ entTag ent ++ " attacked you for " ++ show dmg ++ " damage!"
                  _ -> pure ()
                -- Check for mob death and spawn item drops
                when (entHealth ent' <= 0) $ do
                  destroyEntity entityWorld eid
                  modifyIORef' aiStatesRef (HM.delete eid)
                  let dropPos = entPosition ent'
                      mobDrop = case readMobType (entTag ent) of
                        Pig      -> Just (BlockItem OakPlanks, 1)
                        Cow      -> Just (BlockItem OakPlanks, 1)
                        Sheep    -> Just (BlockItem OakPlanks, 1)
                        Chicken  -> Just (BlockItem OakPlanks, 1)
                        Zombie   -> Just (BlockItem IronOre, 1)
                        Skeleton -> Just (BlockItem CoalOre, 1)
                        Creeper  -> Just (BlockItem Cobblestone, 1)
                        Spider   -> Just (BlockItem Cobblestone, 1)
                  case mobDrop of
                    Just (item, count) -> do
                      spawnDrop droppedItems item count dropPos
                      putStrLn $ entTag ent ++ " died and dropped " ++ show item
                    Nothing -> pure ()

            -- Auto-save every ~5 minutes (18000 frames at 60fps)
            when (frameIdx > 0 && frameIdx `mod` 18000 == 0) $ do
              player <- readIORef playerRef
              sd <- readIORef saveDirRef
              savePlayer sd player
              saveWorld sd world
              saveTileEntities sd tileEntityRef

            -- Update chunks periodically
            when (frameIdx `mod` 60 == 0) $ do
              player <- readIORef playerRef
              newChunks <- updateLoadedChunks world (plPos player)
              -- Mesh only newly loaded chunks
              mapM_ (\c -> meshSingleChunk physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef c) newChunks
              -- Remove meshes for unloaded chunks
              pruneChunkMeshes world device meshCacheRef

            -- Render
            player <- readIORef playerRef
            -- Camera bob effect when walking on ground
            let V3 vx _ vz = plVelocity player
                speed = sqrt (vx * vx + vz * vz)
                isMoving = speed > 0.5 && plOnGround player
            bobTimer <- readIORef bobTimerRef
            let bobRate = if plSprinting player then 14.0 else 10.0
                bobTimer' = if isMoving then bobTimer + rawDt * bobRate else 0
            writeIORef bobTimerRef bobTimer'
            let bobOffset = if isMoving then sin bobTimer' * 0.04 else 0
                baseCam = cameraFromPlayer player
                cam = baseCam { camPosition = camPosition baseCam + V3 0 bobOffset 0 }
            let currentFrame = frameIdx `mod` maxFrames
            let cmdBuf = cmdBuffers V.! currentFrame
            let sync   = syncObjects V.! currentFrame
            let ds     = dsSets V.! currentFrame

            sc' <- readIORef scRef
            dayNightVal <- readIORef dayNightRef
            let Vk.Extent2D{width = extW, height = extH} = scExtent sc'
            let aspect = fromIntegral extW / fromIntegral extH
                V3 sx sy sz = getSunDirection dayNightVal
                ubo = UniformBufferObject
                  { uboModel        = transpose identity
                  , uboView         = transpose $ cameraViewMatrix cam
                  , uboProjection   = transpose $ cameraProjectionMatrix aspect 0.1 1000 cam
                  , uboSunDirection = V4 sx sy sz 0
                  , uboAmbientLight = getAmbientLight dayNightVal
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

            -- Compute sky color from day/night cycle, with underwater tint
            player' <- readIORef playerRef
            let eyeWorld = plPos player' + V3 0 1.62 0
                V3 hbx hby hbz = fmap floor eyeWorld :: V3 Int
            headBlock <- worldGetBlock world (V3 hbx hby hbz)
            let isUnderwater = headBlock == Water
                V4 baseSkyR baseSkyG baseSkyB baseSkyA = getSkyColor dayNightVal
                (skyR, skyG, skyB, skyA) = if isUnderwater
                  then (0.05, 0.15, 0.35, 1.0)  -- dark blue underwater
                  else (baseSkyR, baseSkyG, baseSkyB, baseSkyA)

            -- Update HUD vertices with current inventory, mining progress, and health
            inv <- readIORef inventoryRef
            mining <- readIORef miningRef
            let miningProgress = case mining of
                  Just (_, p) -> p
                  Nothing     -> 0
            mode <- readIORef gameModeRef
            cursorItem <- readIORef cursorItemRef
            craftGrid <- readIORef craftingGridRef
            -- Compute block placement preview ghost
            placementGhostVerts <- if mode == Playing
              then do
                let plEye = plPos player' + V3 0 1.62 0
                    plDir = dirFromPlayer player'
                    solidCb bx' by' bz' = do
                      bt' <- worldGetBlock world (V3 bx' by' bz')
                      pure (World.Block.isSolid bt')
                mHit <- raycastBlock solidCb plEye plDir maxReach
                case mHit of
                  Nothing -> pure []
                  Just hit -> do
                    let V3 hx hy hz = rhBlockPos hit
                        V3 fnx fny fnz = rhFaceNormal hit
                        placePos = V3 (hx + fnx) (hy + fny) (hz + fnz)
                    -- Check if player has a placeable block selected
                    invNow <- readIORef inventoryRef
                    case selectedItem invNow of
                      Just (ItemStack item _) | itemIsBlock item ->
                        pure $ projectBlockWireframe vp placePos
                      _ -> pure []
              else pure []
            -- Update & render particles
            updateParticles particleSys rawDt
            particleVerts <- renderParticles particleSys vp
            -- F3 debug overlay
            showDebug <- readIORef debugOverlayRef
            let debugOverlayVerts = if showDebug && mode == Playing
                  then let V3 px py pz = plPos player'
                           fpsText = "FPS: " ++ show (round (1.0 / max rawDt 0.001) :: Int)
                           posText = "XYZ: " ++ showF1 px ++ " / " ++ showF1 py ++ " / " ++ showF1 pz
                           chunkText = "Chunk: " ++ show (floor px `div` 16 :: Int) ++ " " ++ show (floor pz `div` 16 :: Int)
                           facingText = "Facing: yaw=" ++ showF1 (plYaw player') ++ " pitch=" ++ showF1 (plPitch player')
                           white = (1.0, 1.0, 1.0, 1.0)
                           textScale = 0.7
                           lineH = 0.04
                           bg = hudQuad (-1.0) (-1.0) (-0.35) (-1.0 + lineH * 5) (0.0, 0.0, 0.0, 0.5)
                       in bg
                          ++ renderText (-0.98) (-0.98) textScale white fpsText
                          ++ renderText (-0.98) (-0.98 + lineH) textScale white posText
                          ++ renderText (-0.98) (-0.98 + lineH * 2) textScale white chunkText
                          ++ renderText (-0.98) (-0.98 + lineH * 3) textScale white facingText
                  else []
            -- Read furnace state if furnace UI is open
            mFurnace <- do
              mPos <- readIORef openFurnacePosRef
              case mPos of
                Just fPos -> do
                  mTE <- getTileEntity tileEntityRef fPos
                  pure $ case mTE of
                    Just (TEFurnace fs) -> Just fs
                    _                   -> Nothing
                Nothing -> pure Nothing
            -- Mouse position in NDC for cursor-follows-mouse and hover highlights
            (mx, my) <- readIORef mousePosRef
            (screenW, screenH) <- getWindowScreenSize wh
            let mouseNdcX = realToFrac mx / fromIntegral screenW * 2.0 - 1.0 :: Float
                mouseNdcY = realToFrac my / fromIntegral screenH * 2.0 - 1.0 :: Float
            let hudBase = buildHudVertices inv miningProgress (plHealth player') (plHunger player') mode cursorItem craftGrid mFurnace mouseNdcX mouseNdcY
                -- Add underwater overlay (full-screen semi-transparent blue tint)
                underwaterOverlay = if isUnderwater
                  then VS.fromList $ hudQuad (-1) (-1) 1 1 (0.05, 0.2, 0.55, 0.35)
                  else VS.empty
            -- Damage flash detection and rendering
            lastHealth <- readIORef lastHealthRef
            let curHealth = plHealth player'
            when (curHealth < lastHealth) $ writeIORef damageFlashRef 0.3
            writeIORef lastHealthRef curHealth
            flashTime <- readIORef damageFlashRef
            when (flashTime > 0) $ writeIORef damageFlashRef (max 0 (flashTime - rawDt))
            let damageOverlay = if flashTime > 0
                  then VS.fromList $ hudQuad (-1) (-1) 1 1 (0.8, 0.0, 0.0, 0.3 * (flashTime / 0.3))
                  else VS.empty
            let hudVerts = hudBase VS.++ underwaterOverlay VS.++ damageOverlay
                           VS.++ (if null particleVerts then VS.empty else VS.fromList particleVerts)
                           VS.++ (if null placementGhostVerts then VS.empty else VS.fromList placementGhostVerts)
                           VS.++ (if null debugOverlayVerts then VS.empty else VS.fromList debugOverlayVerts)
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
playerLoop :: PlayerInput -> BlockQuery -> BlockQuery -> IORef Float -> Float -> IORef Player -> IO ()
playerLoop input blockQuery waterQuery accumRef accum playerRef
  | accum < tickRate = writeIORef accumRef accum
  | otherwise = do
      player <- readIORef playerRef
      player' <- updatePlayer tickRate input blockQuery waterQuery player
      writeIORef playerRef player'
      playerLoop (input { piToggleFly = False }) blockQuery waterQuery accumRef (accum - tickRate) playerRef

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

-- | Check if an item is a placeable block
itemIsBlock :: Item -> Bool
itemIsBlock (BlockItem _) = True
itemIsBlock _             = False

-- | Show a float with 1 decimal place
showF1 :: Float -> String
showF1 f = let n = round (f * 10) :: Int
               (whole, frac) = n `divMod` 10
               sign = if n < 0 && whole == 0 then "-" else ""
           in sign ++ show whole ++ "." ++ show (abs frac)

-- | Project a block wireframe to screen-space HUD quads.
--   Draws 12 edges of a cube as thin quads in NDC.
projectBlockWireframe :: M44 Float -> V3 Int -> [Float]
projectBlockWireframe vpMat (V3 bx by bz) =
  let fx = fromIntegral bx :: Float
      fy = fromIntegral by :: Float
      fz = fromIntegral bz :: Float
      -- 8 corners of the block
      corners = [ V4 fx fy fz 1
                , V4 (fx+1) fy fz 1
                , V4 (fx+1) (fy+1) fz 1
                , V4 fx (fy+1) fz 1
                , V4 fx fy (fz+1) 1
                , V4 (fx+1) fy (fz+1) 1
                , V4 (fx+1) (fy+1) (fz+1) 1
                , V4 fx (fy+1) (fz+1) 1
                ]
      -- Project to NDC
      projected = map (projectPoint vpMat) corners
      -- 12 edges: pairs of corner indices
      edges = [(0,1),(1,2),(2,3),(3,0),(4,5),(5,6),(6,7),(7,4),(0,4),(1,5),(2,6),(3,7)]
      -- Draw each visible edge as a thin line quad
      lineColor = (1.0, 1.0, 1.0, 0.4)
  in concatMap (\(i,j) -> drawEdge lineColor (projected !! i) (projected !! j)) edges

-- | Project a 4D homogeneous point through VP matrix to (ndcX, ndcY, visible)
projectPoint :: M44 Float -> V4 Float -> (Float, Float, Bool)
projectPoint vpMat p =
  let V4 cx cy _cz cw = vpMat !* p
  in if cw <= 0.01
     then (0, 0, False)
     else (cx / cw, cy / cw, True)

-- | Draw a line between two projected points as a thin quad
drawEdge :: (Float, Float, Float, Float) -> (Float, Float, Bool) -> (Float, Float, Bool) -> [Float]
drawEdge _ (_, _, False) _ = []
drawEdge _ _ (_, _, False) = []
drawEdge (r, g, b, a) (x0, y0, _) (x1, y1, _)
  | abs x0 > 2 || abs y0 > 2 || abs x1 > 2 || abs y1 > 2 = []  -- off-screen
  | otherwise =
    let -- Perpendicular direction for line thickness
        dx = x1 - x0
        dy = y1 - y0
        len = sqrt (dx * dx + dy * dy)
        thickness = 0.003
    in if len < 0.001 then [] else
      let nx = (-dy) / len * thickness
          ny = dx / len * thickness
      in [ x0 + nx, y0 + ny, r, g, b, a
         , x0 - nx, y0 - ny, r, g, b, a
         , x1 - nx, y1 - ny, r, g, b, a
         , x0 + nx, y0 + ny, r, g, b, a
         , x1 - nx, y1 - ny, r, g, b, a
         , x1 + nx, y1 + ny, r, g, b, a
         ]

-- | Furnace slot types
data FurnaceSlot = FurnaceInput | FurnaceFuel | FurnaceOutput | FurnaceInvSlot !Int
  deriving stock (Show, Eq)

-- | Check if NDC coordinates hit a furnace slot
hitFurnaceSlot :: Float -> Float -> Maybe FurnaceSlot
hitFurnaceSlot nx ny
  -- Input slot (top center)
  | nx >= -0.15 && nx <= -0.03 && ny >= -0.35 && ny <= -0.23 = Just FurnaceInput
  -- Fuel slot (bottom left)
  | nx >= -0.15 && nx <= -0.03 && ny >= -0.05 && ny <= 0.07 = Just FurnaceFuel
  -- Output slot (right side)
  | nx >= 0.12 && nx <= 0.24 && ny >= -0.22 && ny <= -0.10 = Just FurnaceOutput
  -- Inventory slots below
  | otherwise = fmap FurnaceInvSlot (hitInventorySlot nx (ny - 0.67))

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
readMobType _          = Pig

-- | Generate a colored quad (2 triangles, 6 vertices × 6 floats each)
hudQuad :: Float -> Float -> Float -> Float -> (Float, Float, Float, Float) -> [Float]
hudQuad x0 y0 x1 y1 (r, g, b, a) =
  [ x0, y0, r, g, b, a
  , x1, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y1, r, g, b, a
  ]

-- | Build HUD vertices from current state
buildHudVertices :: Inventory -> Float -> Int -> Int -> GameMode -> Maybe ItemStack -> CraftingGrid -> Maybe FurnaceState -> Float -> Float -> VS.Vector Float
buildHudVertices inv miningProgress health hunger mode cursorItem craftGrid mFurnace mouseNdcX mouseNdcY = VS.fromList $
  case mode of
    MainMenu -> menuVerts
    Paused   -> pauseVerts
    Playing  -> crosshairVerts ++ hotbarBgVerts ++ slotVerts ++ selectorVerts ++ miningBarVerts ++ healthVerts ++ hungerVerts ++ handVerts
    InventoryOpen -> invScreenVerts ++ invHoverVerts ++ cursorVerts
    CraftingOpen  -> craftScreenVerts ++ craftHoverVerts ++ cursorVerts
    FurnaceOpen   -> furnaceScreenVerts ++ furnaceHoverVerts ++ cursorVerts
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

    -- Inventory screen: dark overlay + 4x9 slot grid
    invScreenVerts =
      -- Full-screen dark overlay
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Grid background
      ++ quad (invGridX0 - 0.02) (invGridY0 - 0.02)
              (invGridX0 + 9 * invSlotW + 0.02) (invGridY0 + 4 * invSlotH + 0.02)
              (0.3, 0.3, 0.3, 0.9)
      -- Individual slots
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

    -- Furnace screen: input/fuel/output slots + flame/arrow indicators + inventory
    furnaceScreenVerts =
      quad (-1) (-1) 1 1 (0, 0, 0, 0.5)
      -- Panel background
      ++ quad (-0.35) (-0.45) 0.35 0.08 (0.3, 0.3, 0.3, 0.9)
      -- Title label
      ++ renderTextCentered (-0.42) 0.9 (1,1,1,1) furnaceLabel
      -- Input slot background (top center-left)
      ++ quad (-0.15) (-0.35) (-0.03) (-0.23) (0.15, 0.15, 0.15, 0.8)
      ++ renderFurnaceItem (fsInput =<< mFurnace) (-0.15) (-0.35) 0.12
      -- Fuel slot background (below input)
      ++ quad (-0.15) (-0.05) (-0.03) 0.07 (0.15, 0.15, 0.15, 0.8)
      ++ renderFurnaceItem (fsFuel =<< mFurnace) (-0.15) (-0.05) 0.12
      -- Flame icon between input and fuel (fuel level indicator)
      ++ furnaceFlame
      -- Arrow icon (smelt progress indicator)
      ++ furnaceArrow
      -- Output slot background (right side, larger)
      ++ quad 0.12 (-0.22) 0.24 (-0.10) (0.2, 0.2, 0.15, 0.9)
      ++ renderFurnaceItem (fsOutput =<< mFurnace) 0.12 (-0.22) 0.12
      -- Inventory grid below
      ++ quad (invGridX0 - 0.02) 0.15 (invGridX0 + 9 * invSlotW + 0.02) (0.15 + 4 * invSlotH + 0.02) (0.3, 0.3, 0.3, 0.9)
      ++ concatMap renderFurnaceInvSlot [0..35]
      where
        furnaceLabel = case mFurnace of
          Just fs -> furnaceVariantName (fsType fs)
          Nothing -> "Furnace"

        renderFurnaceItem mStack x y sz = case mStack of
          Nothing -> []
          Just (ItemStack item cnt) ->
            let colors = itemMiniIcon item
                pixW = sz / 3; pixH = sz / 3
                iconVerts = concatMap (\(r, c, clr) ->
                  quad (x + fromIntegral c * pixW) (y + fromIntegral r * pixH)
                       (x + fromIntegral (c+1) * pixW) (y + fromIntegral (r+1) * pixH) clr) colors
                countText = if cnt > 1
                  then renderText (x + sz - 0.025) (y + sz - 0.02) 0.6 (1,1,1,1) (show cnt)
                  else []
            in iconVerts ++ countText

        -- Flame: yellow/orange indicator showing remaining fuel
        furnaceFlame =
          let flameX = -0.12; flameY = -0.20
              flameW = 0.06; flameH = 0.10
              fuelFrac = case mFurnace of
                Just fs | fsFuelTotal fs > 0 -> fsFuelRemaining fs / fsFuelTotal fs
                _                            -> 0
              -- Background (dark)
              bg = quad flameX flameY (flameX + flameW) (flameY + flameH) (0.2, 0.15, 0.1, 0.6)
              -- Fill from bottom up
              fillH = flameH * fuelFrac
              fill = if fuelFrac > 0
                then quad flameX (flameY + flameH - fillH) (flameX + flameW) (flameY + flameH) (1.0, 0.6, 0.1, 0.9)
                else []
          in bg ++ fill

        -- Arrow: white progress indicator showing smelt progress
        furnaceArrow =
          let arrX = 0.0; arrY = -0.20
              arrW = 0.10; arrH = 0.04
              progress = case mFurnace of
                Just fs -> fsSmeltProgress fs
                Nothing -> 0
              bg = quad arrX arrY (arrX + arrW) (arrY + arrH) (0.3, 0.3, 0.3, 0.6)
              fillW = arrW * min 1.0 progress
              fill = if progress > 0
                then quad arrX arrY (arrX + fillW) (arrY + arrH) (1.0, 1.0, 1.0, 0.9)
                else []
          in bg ++ fill

        renderFurnaceInvSlot idx =
          let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
              col = if idx < 9 then idx else (idx - 9) `mod` 9
              x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
              y = 0.17 + fromIntegral row * invSlotH + invSlotPad
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

    -- Hover highlight for inventory screen
    invHoverVerts = case hitInventorySlot mouseNdcX mouseNdcY of
      Nothing -> []
      Just idx ->
        let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
            col = if idx < 9 then idx else (idx - 9) `mod` 9
            x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
            y = invGridY0 + fromIntegral row * invSlotH + invSlotPad
            sw = invSlotW - 2 * invSlotPad
            sh = invSlotH - 2 * invSlotPad
        in quad x y (x + sw) (y + sh) (1, 1, 1, 0.2)

    -- Hover highlight for crafting screen
    craftHoverVerts = case hitCraftingSlot mouseNdcX mouseNdcY of
      Just (CraftGrid row col) ->
        let x = -0.3 + fromIntegral col * 0.1 + 0.005
            y = -0.35 + fromIntegral row * 0.1 + 0.005
        in quad x y (x + 0.09) (y + 0.09) (1, 1, 1, 0.2)
      Just CraftOutput ->
        quad 0.15 (-0.27) (0.15 + 0.12) (-0.27 + 0.12) (1, 1, 1, 0.2)
      Just (CraftInvSlot idx) ->
        let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
            col = if idx < 9 then idx else (idx - 9) `mod` 9
            x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
            y = 0.1 + fromIntegral row * invSlotH + invSlotPad
            sw = invSlotW - 2 * invSlotPad
            sh = invSlotH - 2 * invSlotPad
        in quad x y (x + sw) (y + sh) (1, 1, 1, 0.2)
      Nothing -> []

    -- Hover highlight for furnace screen
    furnaceHoverVerts = case hitFurnaceSlot mouseNdcX mouseNdcY of
      Just FurnaceInput ->
        quad (-0.15) (-0.35) (-0.03) (-0.23) (1, 1, 1, 0.2)
      Just FurnaceFuel ->
        quad (-0.15) (-0.05) (-0.03) 0.07 (1, 1, 1, 0.2)
      Just FurnaceOutput ->
        quad 0.12 (-0.22) 0.24 (-0.10) (1, 1, 1, 0.2)
      Just (FurnaceInvSlot idx) ->
        let row = if idx < 9 then 0 else 1 + (idx - 9) `div` 9
            col = if idx < 9 then idx else (idx - 9) `mod` 9
            x = invGridX0 + fromIntegral col * invSlotW + invSlotPad
            y = 0.17 + fromIntegral row * invSlotH + invSlotPad
            sw = invSlotW - 2 * invSlotPad
            sh = invSlotH - 2 * invSlotPad
        in quad x y (x + sw) (y + sh) (1, 1, 1, 0.2)
      Nothing -> []

    -- Cursor item follows mouse position
    cursorVerts = case cursorItem of
      Nothing -> []
      Just (ItemStack item _) ->
        let colors = itemMiniIcon item
            sw = 0.1; sh = 0.1; pixW = sw / 3; pixH = sh / 3
            x = mouseNdcX; y = mouseNdcY  -- top-left at cursor
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

-- | Get a display color for an item (used for hotbar slot rendering)
itemColor :: Item -> (Float, Float, Float, Float)
itemColor (BlockItem bt) = case bt of
  Stone       -> (0.5, 0.5, 0.5, 1.0)
  Dirt        -> (0.55, 0.35, 0.17, 1.0)
  Grass       -> (0.2, 0.65, 0.1, 1.0)
  Sand        -> (0.82, 0.75, 0.5, 1.0)
  OakLog      -> (0.4, 0.3, 0.15, 1.0)
  OakPlanks   -> (0.78, 0.65, 0.43, 1.0)
  OakLeaves   -> (0.15, 0.5, 0.12, 1.0)
  Cobblestone -> (0.45, 0.45, 0.45, 1.0)
  Water       -> (0.15, 0.4, 0.8, 0.8)
  Lava        -> (0.9, 0.35, 0.05, 1.0)
  Torch       -> (0.95, 0.8, 0.2, 1.0)
  Glass       -> (0.7, 0.85, 0.95, 0.5)
  IronOre     -> (0.7, 0.6, 0.5, 1.0)
  CoalOre     -> (0.25, 0.25, 0.25, 1.0)
  GoldOre     -> (1.0, 0.85, 0.0, 1.0)
  DiamondOre  -> (0.3, 0.8, 0.95, 1.0)
  Snow        -> (0.95, 0.95, 0.95, 1.0)
  BlastFurnace -> (0.55, 0.55, 0.6, 1.0)
  Smoker      -> (0.5, 0.4, 0.3, 1.0)
  Hopper      -> (0.4, 0.4, 0.45, 1.0)
  _           -> (0.6, 0.6, 0.6, 1.0)
itemColor (ToolItem Pickaxe _ _) = (0.7, 0.7, 0.8, 1.0)
itemColor (ToolItem Sword _ _)   = (0.8, 0.8, 0.9, 1.0)
itemColor (ToolItem Axe _ _)     = (0.6, 0.5, 0.3, 1.0)
itemColor (ToolItem Shovel _ _)  = (0.5, 0.4, 0.25, 1.0)
itemColor (ToolItem Hoe _ _)     = (0.5, 0.5, 0.3, 1.0)
itemColor (MaterialItem mt) = case mt of
  IronIngot  -> (0.85, 0.85, 0.85, 1.0)
  GoldIngot  -> (1.0, 0.85, 0.2, 1.0)
  Charcoal   -> (0.2, 0.2, 0.2, 1.0)
  CookedPork -> (0.7, 0.45, 0.3, 1.0)
  CookedBeef -> (0.65, 0.35, 0.2, 1.0)
  BrickItem  -> (0.7, 0.35, 0.25, 1.0)
  RawPork    -> (0.95, 0.7, 0.7, 1.0)
  RawBeef    -> (0.7, 0.15, 0.15, 1.0)

-- | 3x3 mini-icon for item (row, col, color) — used in hotbar slot rendering
itemMiniIcon :: Item -> [(Int, Int, (Float, Float, Float, Float))]
itemMiniIcon (ToolItem Pickaxe _ _) =
  [(0,0,m),(0,1,m),(0,2,m), (1,1,s),(1,2,b), (2,0,b),(2,2,s)]
  where m = (0.7,0.7,0.8,1); s = (0.5,0.35,0.15,1); b = (0,0,0,0)
itemMiniIcon (ToolItem Sword _ _) =
  [(0,2,m), (1,1,m), (2,0,s)]
  where m = (0.8,0.8,0.9,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Axe _ _) =
  [(0,1,m),(0,2,m), (1,0,s),(1,1,m), (2,0,s)]
  where m = (0.6,0.5,0.3,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Shovel _ _) =
  [(0,1,m), (1,1,s), (2,1,s)]
  where m = (0.5,0.4,0.25,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (ToolItem Hoe _ _) =
  [(0,1,m),(0,2,m), (1,1,s), (2,1,s)]
  where m = (0.5,0.5,0.3,1); s = (0.5,0.35,0.15,1)
itemMiniIcon (BlockItem bt) = blockMiniIcon bt
  where
    fill c = [(r,col,c) | r <- [0..2], col <- [0..2]]
    blockMiniIcon Stone = fill (0.5,0.5,0.5,1)
    blockMiniIcon Dirt  = fill (0.55,0.35,0.17,1)
    blockMiniIcon Grass =
      [(0,0,g),(0,1,g),(0,2,g), (1,0,gs),(1,1,gs),(1,2,gs), (2,0,d),(2,1,d),(2,2,d)]
      where g = (0.2,0.65,0.1,1); gs = (0.35,0.55,0.2,1); d = (0.55,0.35,0.17,1)
    blockMiniIcon Sand = fill (0.82,0.75,0.5,1)
    blockMiniIcon OakLog =
      [(0,0,bk),(0,1,lt),(0,2,bk), (1,0,bk),(1,1,lt),(1,2,bk), (2,0,bk),(2,1,lt),(2,2,bk)]
      where bk = (0.4,0.3,0.15,1); lt = (0.6,0.5,0.25,1)
    blockMiniIcon OakPlanks = fill (0.78,0.65,0.43,1)
    blockMiniIcon OakLeaves = fill (0.15,0.5,0.12,1)
    blockMiniIcon Cobblestone =
      [(0,0,dk),(0,1,lt),(0,2,dk), (1,0,lt),(1,1,dk),(1,2,lt), (2,0,dk),(2,1,lt),(2,2,dk)]
      where dk = (0.35,0.35,0.35,1); lt = (0.5,0.5,0.5,1)
    blockMiniIcon Water = fill (0.15,0.4,0.8,0.8)
    blockMiniIcon Lava  = fill (0.9,0.35,0.05,1)
    blockMiniIcon Torch =
      [(0,1,fl), (1,1,st), (2,1,st)]
      where fl = (0.95,0.8,0.2,1); st = (0.5,0.35,0.15,1)
    blockMiniIcon Glass = fill (0.7,0.85,0.95,0.5)
    blockMiniIcon IronOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,st),(1,1,st),(1,2,ore), (2,0,ore),(2,1,st),(2,2,st)]
      where st = (0.5,0.5,0.5,1); ore = (0.7,0.6,0.5,1)
    blockMiniIcon CoalOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,ore),(1,1,st),(1,2,st), (2,0,st),(2,1,st),(2,2,ore)]
      where st = (0.5,0.5,0.5,1); ore = (0.15,0.15,0.15,1)
    blockMiniIcon GoldOre =
      [(0,0,st),(0,1,st),(0,2,ore), (1,0,ore),(1,1,st),(1,2,st), (2,0,st),(2,1,ore),(2,2,st)]
      where st = (0.5,0.5,0.5,1); ore = (1,0.85,0,1)
    blockMiniIcon DiamondOre =
      [(0,0,st),(0,1,ore),(0,2,st), (1,0,st),(1,1,ore),(1,2,st), (2,0,ore),(2,1,st),(2,2,ore)]
      where st = (0.5,0.5,0.5,1); ore = (0.3,0.8,0.95,1)
    blockMiniIcon Snow = fill (0.95,0.95,0.95,1)
    blockMiniIcon _ = fill (itemColor (BlockItem bt))
itemMiniIcon (MaterialItem mt) = case mt of
  IronIngot  -> [(0,0,c),(0,1,c),(0,2,c), (1,0,c),(1,1,h),(1,2,c)]
    where c = (0.85,0.85,0.85,1); h = (0.95,0.95,0.95,1)
  GoldIngot  -> [(0,0,c),(0,1,c),(0,2,c), (1,0,c),(1,1,h),(1,2,c)]
    where c = (1.0,0.85,0.2,1); h = (1.0,0.95,0.5,1)
  Charcoal   -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c)]
    where c = (0.2,0.2,0.2,1)
  CookedPork -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,0,c),(2,1,c)]
    where c = (0.7,0.45,0.3,1)
  CookedBeef -> [(0,0,c),(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c),(2,2,c)]
    where c = (0.65,0.35,0.2,1)
  BrickItem  -> [(0,0,c),(0,1,c),(0,2,c), (2,0,c),(2,1,c),(2,2,c)]
    where c = (0.7,0.35,0.25,1)
  RawPork    -> [(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,0,c),(2,1,c)]
    where c = (0.95,0.7,0.7,1)
  RawBeef    -> [(0,0,c),(0,1,c), (1,0,c),(1,1,c),(1,2,c), (2,1,c),(2,2,c)]
    where c = (0.7,0.15,0.15,1)
