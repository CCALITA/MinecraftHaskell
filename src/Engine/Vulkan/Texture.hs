module Engine.Vulkan.Texture
  ( TextureImage(..)
  , createTextureAtlas
  , createPlaceholderAtlas
  , destroyTextureImage
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Memory (createBuffer, destroyBuffer, BufferAllocation(..), copyBuffer)

import Data.Bits ((.|.), (.&.), shiftL)
import qualified Data.Bits
import Data.Word (Word8, Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (sizeOf)

-- | Texture image with sampler for use in descriptor sets
data TextureImage = TextureImage
  { tiImage     :: !Vk.Image
  , tiMemory    :: !Vk.DeviceMemory
  , tiImageView :: !Vk.ImageView
  , tiSampler   :: !Vk.Sampler
  } deriving stock (Show)

-- | Create a placeholder 256x256 texture atlas with patterned tiles for each block type
createPlaceholderAtlas
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IO TextureImage
createPlaceholderAtlas physDevice device cmdPool queue = do
  let width  = 256 :: Word32
      height = 256 :: Word32
      tileSize = 16 :: Int
      pixels = VS.generate (fromIntegral width * fromIntegral height * 4) $ \i ->
        let pixelIdx = i `div` 4
            channel  = i `mod` 4
            px = pixelIdx `mod` fromIntegral width
            py = pixelIdx `div` fromIntegral width
            tileX = px `div` tileSize
            tileY = py `div` tileSize
            tileIdx = tileX + tileY * (fromIntegral width `div` tileSize)
            localX = px `mod` tileSize
            localY = py `mod` tileSize
        in tilePixel tileIdx localX localY channel :: Word8

  createTextureFromPixels physDevice device cmdPool queue width height pixels

-- | Simple hash for pixel-level noise (deterministic)
pixHash :: Int -> Int -> Int -> Int
pixHash x y s =
  let h0 = (x * 374761393 + y * 668265263 + s * 1274126177) `mod` 0x7FFFFFFF
      h1 = ((h0 `xor` (h0 `shiftR` 13)) * 1103515245 + 12345) `mod` 0x7FFFFFFF
  in abs h1
  where
    xor = Data.Bits.xor
    shiftR = Data.Bits.shiftR

-- | Generate a pixel for a tile with patterns
tilePixel :: Int -> Int -> Int -> Int -> Word8
tilePixel tileIdx lx ly channel =
  let (r, g, b, a) = tileFull tileIdx lx ly
  in case channel of { 0 -> r; 1 -> g; 2 -> b; 3 -> a; _ -> 0 }

-- | Full RGBA for a pixel within a tile
tileFull :: Int -> Int -> Int -> (Word8, Word8, Word8, Word8)
tileFull tileIdx lx ly = case tileIdx of
  0  -> grassTop lx ly         -- grass top
  1  -> stonePattern lx ly     -- stone
  2  -> dirtPattern lx ly      -- dirt
  3  -> grassSide lx ly        -- grass side
  4  -> planksPattern lx ly    -- oak planks / sand
  5  -> gravelPattern lx ly    -- gravel
  6  -> logBark lx ly          -- oak log bark
  7  -> leavesPattern lx ly    -- leaves
  8  -> cobblePattern lx ly    -- cobblestone
  9  -> bedrockPattern lx ly   -- bedrock
  10 -> orePattern lx ly (200, 170, 130)  -- iron ore
  11 -> orePattern lx ly (40, 40, 40)     -- coal ore
  12 -> orePattern lx ly (255, 215, 0)    -- gold ore
  13 -> waterPattern lx ly     -- water
  14 -> lavaPattern lx ly      -- lava
  15 -> glassPattern lx ly     -- glass
  _  -> solidColor 128 128 128 -- fallback
  where
    solidColor r g b = (r, g, b, 255)

    -- Grass top: green with darker blade variations
    grassTop x y =
      let n = pixHash x y 42 `mod` 100
          base = if n < 30 then (30, 140, 20) else if n < 60 then (50, 170, 30) else (40, 155, 25)
          (br, bg, bb) = base
      in (br, bg, bb, 255)

    -- Stone: gray with crack-like darker lines
    stonePattern x y =
      let n = pixHash x y 100 `mod` 100
          crack = pixHash (x `div` 3) (y `div` 4) 101 `mod` 10 < 2
          base = if crack then 95 else if n < 20 then 115 else if n < 50 then 125 else 130
      in (base, base, base, 255)

    -- Dirt: brown with speckled variation
    dirtPattern x y =
      let n = pixHash x y 200 `mod` 100
          r = if n < 25 then 120 else if n < 50 then 135 else 140
          g = r * 60 `div` 140
          b = r * 35 `div` 140
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Grass side: dirt bottom with green strip on top
    grassSide x y
      | y <= 2    = grassTop x y
      | y == 3    = let n = pixHash x y 300 `mod` 3
                    in if n == 0 then grassTop x y else dirtPattern x y
      | otherwise = dirtPattern x y

    -- Wood planks: tan with horizontal grain lines
    planksPattern x y =
      let grain = pixHash x (y `div` 4) 400 `mod` 100
          base = if y `mod` 4 == 0 then 160 else if grain < 20 then 190 else 200
          r = base; g = base * 170 `div` 210; b = base * 110 `div` 210
      in (fromIntegral r, fromIntegral g, fromIntegral b, 255)

    -- Gravel: mix of gray pebble shapes
    gravelPattern x y =
      let n = pixHash x y 500 `mod` 100
          v = if n < 20 then 100 else if n < 50 then 130 else if n < 80 then 145 else 160
      in (v, v, v, 255)

    -- Log bark: brown with vertical lines
    logBark x y =
      let stripe = pixHash (x `div` 2) y 600 `mod` 100
          dark = x `mod` 3 == 0 || stripe < 15
          (r, g, b) = if dark then (70, 50, 25) else (100, 75, 40)
      in (r, g, b, 255)

    -- Leaves: transparent-ish green with holes
    leavesPattern x y =
      let n = pixHash x y 700 `mod` 100
          hole = n < 15
      in if hole then (20, 60, 15, 180) else (40 + fromIntegral (n `mod` 30), 120 + fromIntegral (n `mod` 40), 30, 255)

    -- Cobblestone: irregular gray blocks
    cobblePattern x y =
      let blockN = pixHash (x `div` 4) (y `div` 3) 800
          border = x `mod` 4 == 0 || y `mod` 3 == 0
          v = if border then 80 else fromIntegral (100 + blockN `mod` 40)
      in (v, v, v, 255)

    -- Bedrock: very dark with slight variation
    bedrockPattern x y =
      let n = pixHash x y 900 `mod` 30
          v = fromIntegral (30 + n)
      in (v, v, v, 255)

    -- Ore: stone base with colored spots
    orePattern x y (or', og, ob) =
      let isOreSpot = let n = pixHash (x `div` 3) (y `div` 3) 1000 `mod` 10
                      in n < 3 && (x + y) `mod` 3 /= 0
      in if isOreSpot then (or', og, ob, 255) else stonePattern x y

    -- Water: blue with subtle wave pattern
    waterPattern x y =
      let wave = pixHash x (y + x `div` 3) 1100 `mod` 30
          b = fromIntegral (160 + wave)
          g = fromIntegral (80 + wave `div` 2)
      in (30, g, b, 200)

    -- Lava: orange-red with bright spots
    lavaPattern x y =
      let n = pixHash x y 1200 `mod` 100
          bright = n < 20
          (r, g, b) = if bright then (255, 200, 50) else (220, 80 + fromIntegral (n `mod` 30), 10)
      in (r, g, b, 255)

    -- Glass: mostly transparent with faint border
    glassPattern x y =
      let border = x == 0 || y == 0 || x == 15 || y == 15
      in if border then (200, 220, 240, 120) else (220, 235, 250, 40)

-- | Create a texture from raw RGBA pixel data
createTextureFromPixels
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> Word32 -> Word32 -> VS.Vector Word8
  -> IO TextureImage
createTextureFromPixels physDevice device cmdPool queue width height pixels = do
  let imageSize = fromIntegral $ VS.length pixels

  -- Create staging buffer
  staging <- createBuffer physDevice device imageSize
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  -- Upload pixel data
  ptr <- Vk.mapMemory device (baMemory staging) 0 imageSize Vk.zero
  VS.unsafeWith pixels $ \srcPtr ->
    copyBytes (castPtr ptr) srcPtr (VS.length pixels)
  Vk.unmapMemory device (baMemory staging)

  -- Create image
  let imageInfo = Vk.ImageCreateInfo
        { Vk.next          = ()
        , Vk.flags         = Vk.zero
        , Vk.imageType     = Vk.IMAGE_TYPE_2D
        , Vk.format        = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.extent        = Vk.Extent3D width height 1
        , Vk.mipLevels     = 1
        , Vk.arrayLayers   = 1
        , Vk.samples       = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling        = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage         = Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT
        , Vk.sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.queueFamilyIndices = V.empty
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
  image <- Vk.createImage device imageInfo Nothing

  -- Allocate and bind memory
  memReqs <- Vk.getImageMemoryRequirements device image
  memProps <- Vk.getPhysicalDeviceMemoryProperties physDevice
  let Vk.MemoryRequirements{memoryTypeBits = reqMemTypeBits, size = reqSize} = memReqs
  let memTypeIdx = findMemType reqMemTypeBits Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT memProps
  let allocInfo = Vk.MemoryAllocateInfo
        { Vk.next            = ()
        , Vk.allocationSize  = reqSize
        , Vk.memoryTypeIndex = memTypeIdx
        }
  memory <- Vk.allocateMemory device allocInfo Nothing
  Vk.bindImageMemory device image memory 0

  -- Transition image layout and copy buffer to image
  transitionImageLayout device cmdPool queue image
    Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  copyBufferToImage device cmdPool queue (baBuffer staging) image width height
  transitionImageLayout device cmdPool queue image
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

  destroyBuffer device staging

  -- Create image view
  let viewInfo = Vk.ImageViewCreateInfo
        { Vk.next             = ()
        , Vk.flags            = Vk.zero
        , Vk.image            = image
        , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format           = Vk.FORMAT_R8G8B8A8_SRGB
        , Vk.components       = Vk.ComponentMapping
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY Vk.COMPONENT_SWIZZLE_IDENTITY
        , Vk.subresourceRange = Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
        }
  imageView <- Vk.createImageView device viewInfo Nothing

  -- Create sampler with nearest-neighbor filtering (Minecraft pixel art style)
  let samplerInfo = Vk.SamplerCreateInfo
        { Vk.next                   = ()
        , Vk.flags                  = Vk.zero
        , Vk.magFilter              = Vk.FILTER_NEAREST
        , Vk.minFilter              = Vk.FILTER_NEAREST
        , Vk.mipmapMode             = Vk.SAMPLER_MIPMAP_MODE_NEAREST
        , Vk.addressModeU           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.addressModeV           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.addressModeW           = Vk.SAMPLER_ADDRESS_MODE_REPEAT
        , Vk.mipLodBias             = 0
        , Vk.anisotropyEnable       = False
        , Vk.maxAnisotropy          = 1
        , Vk.compareEnable          = False
        , Vk.compareOp              = Vk.COMPARE_OP_ALWAYS
        , Vk.minLod                 = 0
        , Vk.maxLod                 = 0
        , Vk.borderColor            = Vk.BORDER_COLOR_INT_OPAQUE_BLACK
        , Vk.unnormalizedCoordinates = False
        }
  sampler <- Vk.createSampler device samplerInfo Nothing

  pure $ TextureImage image memory imageView sampler

-- | Create a full texture atlas from a PNG file
createTextureAtlas
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> FilePath
  -> IO TextureImage
createTextureAtlas _physDevice _device _cmdPool _queue _path =
  -- TODO: implement PNG loading with JuicyPixels
  error "createTextureAtlas: not yet implemented, use createPlaceholderAtlas"

-- | Destroy texture image resources
destroyTextureImage :: Vk.Device -> TextureImage -> IO ()
destroyTextureImage device ti = do
  Vk.destroySampler device (tiSampler ti) Nothing
  Vk.destroyImageView device (tiImageView ti) Nothing
  Vk.destroyImage device (tiImage ti) Nothing
  Vk.freeMemory device (tiMemory ti) Nothing

-- | Transition image layout using a one-shot command buffer
transitionImageLayout :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Image -> Vk.ImageLayout -> Vk.ImageLayout -> IO ()
transitionImageLayout device cmdPool queue image oldLayout newLayout = do
  let (srcAccess, dstAccess, srcStage, dstStage) = layoutTransitionMasks oldLayout newLayout
  let barrier = Vk.ImageMemoryBarrier
        { Vk.next                = ()
        , Vk.srcAccessMask       = srcAccess
        , Vk.dstAccessMask       = dstAccess
        , Vk.oldLayout           = oldLayout
        , Vk.newLayout           = newLayout
        , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
        , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
        , Vk.image               = image
        , Vk.subresourceRange    = Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
        }
  withOneShot device cmdPool queue $ \cmdBuf ->
    Vk.cmdPipelineBarrier cmdBuf srcStage dstStage Vk.zero
      V.empty V.empty (V.singleton (Vk.SomeStruct barrier))

-- | Copy buffer to image
copyBufferToImage :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Image -> Word32 -> Word32 -> IO ()
copyBufferToImage device cmdPool queue buffer image width height = do
  let region = Vk.BufferImageCopy
        { Vk.bufferOffset      = 0
        , Vk.bufferRowLength   = 0
        , Vk.bufferImageHeight = 0
        , Vk.imageSubresource  = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
        , Vk.imageOffset       = Vk.Offset3D 0 0 0
        , Vk.imageExtent       = Vk.Extent3D width height 1
        }
  withOneShot device cmdPool queue $ \cmdBuf ->
    Vk.cmdCopyBufferToImage cmdBuf buffer image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (V.singleton region)

-- | Run a one-shot command buffer
withOneShot :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> (Vk.CommandBuffer -> IO ()) -> IO ()
withOneShot device cmdPool queue action = do
  let allocInfo = Vk.CommandBufferAllocateInfo cmdPool Vk.COMMAND_BUFFER_LEVEL_PRIMARY 1
  cmdBuffers <- Vk.allocateCommandBuffers device allocInfo
  let cmdBuf = V.head cmdBuffers
  let beginInfo = Vk.CommandBufferBeginInfo
        { Vk.next = (), Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, Vk.inheritanceInfo = Nothing }
  Vk.beginCommandBuffer cmdBuf beginInfo
  action cmdBuf
  Vk.endCommandBuffer cmdBuf
  let submitInfo = Vk.SubmitInfo
        { Vk.next = (), Vk.waitSemaphores = V.empty, Vk.waitDstStageMask = V.empty
        , Vk.commandBuffers = V.singleton (Vk.commandBufferHandle cmdBuf), Vk.signalSemaphores = V.empty }
  Vk.queueSubmit queue (V.singleton (Vk.SomeStruct submitInfo)) Vk.zero
  Vk.queueWaitIdle queue
  Vk.freeCommandBuffers device cmdPool cmdBuffers

-- | Layout transition access masks and pipeline stages
layoutTransitionMasks
  :: Vk.ImageLayout -> Vk.ImageLayout
  -> (Vk.AccessFlags, Vk.AccessFlags, Vk.PipelineStageFlags, Vk.PipelineStageFlags)
layoutTransitionMasks Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL =
  (Vk.zero, Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT)
layoutTransitionMasks Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL =
  (Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.ACCESS_SHADER_READ_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT, Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
layoutTransitionMasks _ _ = error "Unsupported layout transition"

-- | Find memory type index
findMemType :: Word32 -> Vk.MemoryPropertyFlags -> Vk.PhysicalDeviceMemoryProperties -> Word32
findMemType typeFilter props memProps =
  let memTypes = Vk.memoryTypes memProps
      count = Vk.memoryTypeCount memProps
      go i
        | i >= fromIntegral count = error "Failed to find suitable memory type for texture"
        | typeFilter `testBit'` i
          && (Vk.propertyFlags (memTypes V.! fromIntegral i) .&. props) == props
          = i
        | otherwise = go (i + 1)
  in go 0
  where
    testBit' bits i = bits .&. (1 `shiftL` fromIntegral i) /= 0
