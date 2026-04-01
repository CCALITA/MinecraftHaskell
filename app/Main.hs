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
import World.Block (BlockType(..), isSolid)
import World.Chunk
import World.Generation
import World.World
import Game.Player
import Game.Physics (BlockQuery)
import Game.Inventory
import Game.DayNight
import World.Fluid
import World.Light
import Entity.ECS
import Entity.Mob (MobType(..), updateMobAI, AIState(..))
import Entity.Spawn
import Game.Save

import Control.Monad (unless, when, forM_)
import Control.Concurrent.STM (readTVarIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.IORef
import Linear
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf, poke)
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

    -- Entity system
    entityWorld <- newEntityWorld
    spawnRngRef <- newIORef =<< System.Random.newStdGen
    spawnCooldownRef <- newIORef (0.0 :: Float)
    aiStatesRef <- newIORef (HM.empty :: HM.HashMap Int AIState)

    -- Give player some starting blocks
    let startInv = fst $ addItem (fst $ addItem (fst $ addItem emptyInventory Stone 64) Dirt 64) OakPlanks 64
    writeIORef inventoryRef startInv

    -- Initial chunk loading
    _ <- updateLoadedChunks world spawnPos
    chunkCount <- loadedChunkCount world
    putStrLn $ "Loaded " ++ show chunkCount ++ " initial chunks"

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

    -- Mouse button callback (break/place blocks)
    GLFW.setMouseButtonCallback (whWindow wh) $ Just $ \_win button action _mods ->
      when (action == GLFW.MouseButtonState'Pressed) $ do
        player <- readIORef playerRef
        let eyePos = plPos player + V3 0 1.62 0  -- eye height
            dir = dirFromPlayer player
            blockQuery bx by bz = do
              bt <- worldGetBlock world (V3 bx by bz)
              pure (World.Block.isSolid bt)

        mHit <- raycastBlock blockQuery eyePos dir maxReach
        case mHit of
          Nothing -> pure ()
          Just hit -> do
            let V3 bx by bz = rhBlockPos hit
            case button of
              GLFW.MouseButton'1 -> do  -- Left click: break
                brokenType <- worldGetBlock world (V3 bx by bz)
                worldSetBlock world (V3 bx by bz) Air
                -- Remove fluid if breaking water/lava
                when (brokenType == Water || brokenType == Lava) $
                  removeFluid fluidState world (V3 bx by bz)
                -- Add broken block to inventory
                when (brokenType /= Air && brokenType /= Water && brokenType /= Lava) $ do
                  inv <- readIORef inventoryRef
                  let (inv', _) = addItem inv brokenType 1
                  writeIORef inventoryRef inv'
                putStrLn $ "Broke " ++ show brokenType ++ " at " ++ show (V3 bx by bz)
                rebuildChunkAt world physDevice device cmdPool (vcGraphicsQueue vc) meshCacheRef bx bz
              GLFW.MouseButton'2 -> do  -- Right click: place from hotbar
                inv <- readIORef inventoryRef
                case selectedItem inv of
                  Nothing -> pure ()
                  Just (ItemStack bt _) -> do
                    let V3 nx ny nz = rhFaceNormal hit
                        placePos = V3 (bx + nx) (by + ny) (bz + nz)
                    let (inv', removed) = removeItem inv bt 1
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

    -- Main loop
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

            -- Reset mouse deltas and toggle flags
            writeIORef inputRef noInput

            -- Update day/night cycle
            modifyIORef' dayNightRef (updateDayNight dt)

            -- Tick fluid simulation every physics update
            tickFluids fluidState world

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
            let Vk.Extent2D{width = extW, height = extH} = scExtent sc'
            let cam = cameraFromPlayer player
                aspect = fromIntegral extW / fromIntegral extH
                ubo = UniformBufferObject
                  { uboModel      = transpose identity
                  , uboView       = transpose $ cameraViewMatrix cam
                  , uboProjection = transpose $ cameraProjectionMatrix aspect 0.1 1000 cam
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
            dayNight <- readIORef dayNightRef
            let V4 skyR skyG skyB skyA = getSkyColor dayNight

            fbs <- readIORef fbRef
            needsRecreate <- drawFrame vc sc' pc fbs cmdBuf sync chunkDraws ds (skyR, skyG, skyB, skyA)

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
    loop

    -- Cleanup
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
