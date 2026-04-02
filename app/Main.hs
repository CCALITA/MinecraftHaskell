module Main (main) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Graphics.UI.GLFW as GLFW

import Engine.Window
import Engine.Camera
import Engine.Mesh (BlockVertex(..), MeshData(..), meshChunkWithLight)
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
import Game.DayNight
import World.Fluid
import World.Light
import Entity.ECS
import Entity.Mob (MobType(..), MobInfo(..), updateMobAI, AIState(..), mobInfo)
import Entity.Spawn
import Game.Save
import Game.DroppedItem

import Control.Monad (unless, when, forM_)
import Control.Concurrent.STM (readTVarIO)
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

-- | Fixed timestep for physics (20 ticks per second, like Minecraft)
tickRate :: Float
tickRate = 1.0 / 20.0

-- | Max reach distance for block interaction
maxReach :: Float
maxReach = 5.0

-- | Save directory
saveDir :: FilePath
saveDir = "saves/world1"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let config = defaultEngineConfig
  putStrLn $ "Starting " ++ ecWindowTitle config ++ "..."

  withWindow (ecWindowWidth config) (ecWindowHeight config) (ecWindowTitle config) $ \wh -> do
    -- Initialize Vulkan
    putStrLn "Initializing Vulkan..."
    vc <- createVulkanContext (whWindow wh) (ecEnableValidation config)
    let device = vcDevice vc
        physDevice = vcPhysicalDevice vc
        qfi = vcQueueFamilies vc

    -- Capture mouse
    GLFW.setCursorInputMode (whWindow wh) GLFW.CursorInputMode'Disabled

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
    let genCfg = defaultGenConfig { gcSeed = 12345 }
        renderDist = 4
    world <- newWorld genCfg renderDist

    -- Player (try to load from save, else default)
    let spawnPos = V3 0 80 0
    mSavedPlayer <- loadPlayer saveDir
    playerRef <- newIORef (case mSavedPlayer of
      Just p  -> p
      Nothing -> defaultPlayer spawnPos)
    inventoryRef <- newIORef emptyInventory
    dayNightRef <- newIORef newDayNightCycle
    fluidState <- newFluidState
    droppedItems <- newDroppedItems

    -- Entity system
    entityWorld <- newEntityWorld
    spawnRngRef <- newIORef =<< System.Random.newStdGen
    spawnCooldownRef <- newIORef (0.0 :: Float)
    aiStatesRef <- newIORef (HM.empty :: HM.HashMap Int AIState)

    -- Give player some starting blocks
    let startInv = selectHotbar (foldl (\i (item, cnt) -> fst $ addItem i item cnt) emptyInventory
          [ (ToolItem Pickaxe Wood 59, 1)
          , (ToolItem Sword Wood 59, 1)
          , (BlockItem Stone, 64)
          , (BlockItem Dirt, 64)
          , (BlockItem OakPlanks, 64)
          , (BlockItem Torch, 16)
          ]) 2  -- select first block slot
    writeIORef inventoryRef startInv

    -- Initial chunk loading
    _ <- updateLoadedChunks world spawnPos
    chunkCount <- loadedChunkCount world
    putStrLn $ "Loaded " ++ show chunkCount ++ " initial chunks"

    -- HUD: use a host-visible buffer that's updated each frame with inventory contents
    let hudMaxVerts = 200  -- enough for crosshair + hotbar + slots
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

    -- Mouse callback
    GLFW.setCursorPosCallback (whWindow wh) $ Just $ \_win xpos ypos -> do
      mLast <- readIORef lastCursorRef
      case mLast of
        Nothing -> writeIORef lastCursorRef (Just (xpos, ypos))
        Just (lastX, lastY) -> do
          let dx = realToFrac (xpos - lastX) :: Float
              dy = realToFrac (lastY - ypos) :: Float  -- inverted Y
          modifyIORef' inputRef $ \inp -> inp { piMouseDX = piMouseDX inp + dx
                                              , piMouseDY = piMouseDY inp + dy }
          writeIORef lastCursorRef (Just (xpos, ypos))

    -- Mining state: (target block pos, progress 0.0-1.0)
    miningRef <- newIORef (Nothing :: Maybe (V3 Int, Float))
    lmbHeldRef <- newIORef False

    -- Mouse button callback (place blocks on RMB, track LMB for mining)
    GLFW.setMouseButtonCallback (whWindow wh) $ Just $ \_win button action _mods -> do
      when (button == GLFW.MouseButton'1) $
        writeIORef lmbHeldRef (action == GLFW.MouseButtonState'Pressed)

      when (button == GLFW.MouseButton'1 && action == GLFW.MouseButtonState'Released) $
        writeIORef miningRef Nothing  -- stop mining on release

      when (action == GLFW.MouseButtonState'Pressed) $ do
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
              GLFW.MouseButton'2 -> do  -- Right click: place from hotbar
                inv <- readIORef inventoryRef
                case selectedItem inv of
                  Nothing -> pure ()
                  Just (ItemStack item _) -> case itemToBlock item of
                    Nothing -> pure ()  -- Can't place non-block items
                    Just bt -> do
                      let V3 nx ny nz = rhFaceNormal hit
                          placePos = V3 (bx + nx) (by + ny) (bz + nz)
                      let (inv', removed) = removeItem inv item 1
                      when (removed > 0) $ do
                        writeIORef inventoryRef inv'
                        worldSetBlock world placePos bt
                        putStrLn $ "Placed " ++ show bt ++ " at " ++ show placePos
                        let V3 px' _ pz' = placePos
                        rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef px' pz'
              _ -> pure ()

    -- Key callback for toggle (F = fly, 1-9 = hotbar, ESC = quit)
    GLFW.setKeyCallback (whWindow wh) $ Just $ \_win key _scancode action _mods ->
      when (action == GLFW.KeyState'Pressed) $ case key of
        GLFW.Key'Escape -> do
          player <- readIORef playerRef
          savePlayer saveDir player
          saveWorld saveDir world
          putStrLn "World saved."
          GLFW.setWindowShouldClose (whWindow wh) True
        GLFW.Key'F ->
          modifyIORef' inputRef $ \inp -> inp { piToggleFly = True }
        GLFW.Key'1 -> modifyIORef' inventoryRef (`selectHotbar` 0)
        GLFW.Key'2 -> modifyIORef' inventoryRef (`selectHotbar` 1)
        GLFW.Key'3 -> modifyIORef' inventoryRef (`selectHotbar` 2)
        GLFW.Key'4 -> modifyIORef' inventoryRef (`selectHotbar` 3)
        GLFW.Key'5 -> modifyIORef' inventoryRef (`selectHotbar` 4)
        GLFW.Key'6 -> modifyIORef' inventoryRef (`selectHotbar` 5)
        GLFW.Key'7 -> modifyIORef' inventoryRef (`selectHotbar` 6)
        GLFW.Key'8 -> modifyIORef' inventoryRef (`selectHotbar` 7)
        GLFW.Key'9 -> modifyIORef' inventoryRef (`selectHotbar` 8)
        _ -> pure ()

    -- Timing
    frameRef <- newIORef (0 :: Int)
    lastTimeRef <- newIORef =<< maybe 0 id <$> GLFW.getTime
    accumRef <- newIORef (0.0 :: Float)

    -- Block query for physics
    let blockQuery :: BlockQuery
        blockQuery bx by bz = do
          bt <- worldGetBlock world (V3 bx by bz)
          pure (World.Block.isSolid bt)

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

            -- Fixed timestep physics
            accum <- readIORef accumRef
            let accum' = accum + dt
            playerLoop input blockQuery accumRef accum' playerRef

            -- Check for player death → respawn
            do p <- readIORef playerRef
               when (isPlayerDead p) $ do
                 putStrLn "You died! Respawning..."
                 writeIORef playerRef (respawnPlayer spawnPos p)

            let physicsTickRan = accum' >= tickRate

            -- Clear per-frame mouse movement, but keep one-shot toggles queued
            -- until a physics tick has consumed them.
            writeIORef inputRef (endFrameInput physicsTickRan baseInput)

            -- Mining tick: advance progress while LMB held
            lmbHeld <- readIORef lmbHeldRef
            when lmbHeld $ do
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

            -- Update dropped items (gravity, friction) and auto-collect nearby
            updateDroppedItems dt droppedItems
            do player' <- readIORef playerRef
               collected <- collectNearby droppedItems (plPos player') 1.5
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

            -- Auto-save every ~5 minutes (18000 frames at 60fps)
            when (frameIdx > 0 && frameIdx `mod` 18000 == 0) $ do
              player <- readIORef playerRef
              savePlayer saveDir player
              saveWorld saveDir world

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
            let currentFrame = frameIdx `mod` maxFrames
            let cmdBuf = cmdBuffers V.! currentFrame
            let sync   = syncObjects V.! currentFrame
            let ds     = dsSets V.! currentFrame

            sc' <- readIORef scRef
            dayNightVal <- readIORef dayNightRef
            let Vk.Extent2D{width = extW, height = extH} = scExtent sc'
            let cam = cameraFromPlayer player
                aspect = fromIntegral extW / fromIntegral extH
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

            -- Compute sky color from day/night cycle
            let V4 skyR skyG skyB skyA = getSkyColor dayNightVal

            -- Update HUD vertices with current inventory, mining progress, and health
            inv <- readIORef inventoryRef
            mining <- readIORef miningRef
            player' <- readIORef playerRef
            let miningProgress = case mining of
                  Just (_, p) -> p
                  Nothing     -> 0
                hudVerts = buildHudVertices inv miningProgress (plHealth player')
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
playerLoop :: PlayerInput -> BlockQuery -> IORef Float -> Float -> IORef Player -> IO ()
playerLoop input blockQuery accumRef accum playerRef
  | accum < tickRate = writeIORef accumRef accum
  | otherwise = do
      player <- readIORef playerRef
      player' <- updatePlayer tickRate input blockQuery player
      writeIORef playerRef player'
      playerLoop (input { piToggleFly = False }) blockQuery accumRef (accum - tickRate) playerRef

-- | Convert player state to Camera
cameraFromPlayer :: Player -> Camera
cameraFromPlayer player =
  let yawR   = plYaw player * pi / 180
      pitchR = plPitch player * pi / 180
      front  = V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)
  in defaultCamera
    { camPosition = plPos player + V3 0 1.62 0  -- eye height
    , camFront    = front
    , camYaw      = plYaw player
    , camPitch    = plPitch player
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

-- | Tuple component accessors for HUD vertex data
fst4, snd4, thd4, fth4 :: (a, a, a, a) -> a
fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
fth4 (_, _, _, d) = d

-- | Build HUD vertices from current state
buildHudVertices :: Inventory -> Float -> Int -> VS.Vector Float
buildHudVertices inv miningProgress health = VS.fromList $ crosshairVerts ++ hotbarBgVerts ++ slotVerts ++ selectorVerts ++ miningBarVerts ++ healthVerts
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

    -- Colored squares for each slot's contents
    slotVerts = concatMap makeSlot [0..8]
    makeSlot i =
      let x = hotbarX0 + fromIntegral i * slotW + slotPad
          y = hotbarY + slotPad
          sw = slotW - 2 * slotPad
          sh = slotH - 2 * slotPad
      in case getSlot inv i of
        Nothing -> []  -- empty slot
        Just (ItemStack item _) ->
          let color = itemColor item
          in quad x y (x + sw) (y + sh) color

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
  _           -> (0.6, 0.6, 0.6, 1.0)
itemColor (ToolItem Pickaxe _ _) = (0.7, 0.7, 0.8, 1.0)
itemColor (ToolItem Sword _ _)   = (0.8, 0.8, 0.9, 1.0)
itemColor (ToolItem Axe _ _)     = (0.6, 0.5, 0.3, 1.0)
itemColor (ToolItem Shovel _ _)  = (0.5, 0.4, 0.25, 1.0)
itemColor (ToolItem Hoe _ _)     = (0.5, 0.5, 0.3, 1.0)
