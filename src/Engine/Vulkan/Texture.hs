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

-- | Create a placeholder 256x256 texture atlas with colored squares for each block type
createPlaceholderAtlas
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> IO TextureImage
createPlaceholderAtlas physDevice device cmdPool queue = do
  let width  = 256 :: Word32
      height = 256 :: Word32
      tileSize = 16 :: Int  -- 16x16 tiles
      pixels = VS.generate (fromIntegral width * fromIntegral height * 4) $ \i ->
        let pixelIdx = i `div` 4
            channel  = i `mod` 4
            px = pixelIdx `mod` fromIntegral width
            py = pixelIdx `div` fromIntegral width
            tileX = px `div` tileSize
            tileY = py `div` tileSize
            tileIdx = tileX + tileY * (fromIntegral width `div` tileSize)
        in tileColor tileIdx channel :: Word8

  createTextureFromPixels physDevice device cmdPool queue width height pixels

-- | Generate a color for a tile based on its index and channel
tileColor :: Int -> Int -> Word8
tileColor tileIdx channel =
  let colors = -- R, G, B for each tile index
        [ (0,   200, 0)    -- 0: grass top (green)
        , (128, 128, 128)  -- 1: stone (gray)
        , (139, 90,  43)   -- 2: dirt (brown)
        , (100, 140, 50)   -- 3: grass side
        , (210, 190, 130)  -- 4: sand / planks (tan)
        , (140, 140, 140)  -- 5: gravel
        , (100, 80,  50)   -- 6: oak log bark
        , (50,  120, 50)   -- 7: leaves (dark green)
        , (100, 100, 100)  -- 8: cobblestone
        , (40,  40,  40)   -- 9: bedrock
        , (200, 170, 130)  -- 10: iron ore
        , (60,  60,  60)   -- 11: coal ore
        , (255, 215, 0)    -- 12: gold ore
        , (50,  100, 200)  -- 13: water (blue)
        , (255, 100, 0)    -- 14: lava (orange)
        , (180, 220, 255)  -- 15: glass (light blue)
        ]
      idx = tileIdx `mod` length colors
      (r, g, b) = colors !! idx
  in case channel of
       0 -> r
       1 -> g
       2 -> b
       3 -> 255  -- alpha
       _ -> 0

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
