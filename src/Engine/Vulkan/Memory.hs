module Engine.Vulkan.Memory
  ( BufferAllocation(..)
  , createBuffer
  , createVertexBuffer
  , createIndexBuffer
  , createUniformBuffer
  , destroyBuffer
  , copyBuffer
  , withStagingBuffer
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Data.Bits ((.&.), shiftL, (.|.))
import Data.Word (Word32)
import qualified Data.Vector as V
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.Marshal.Utils (copyBytes)
import qualified Data.Vector.Storable as VS

-- | A buffer with its associated device memory
data BufferAllocation = BufferAllocation
  { baBuffer :: !Vk.Buffer
  , baMemory :: !Vk.DeviceMemory
  , baSize   :: !Vk.DeviceSize
  } deriving stock (Show)

-- | Create a Vulkan buffer with allocated memory
createBuffer
  :: Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.DeviceSize
  -> Vk.BufferUsageFlags
  -> Vk.MemoryPropertyFlags
  -> IO BufferAllocation
createBuffer physDevice device size usage memProps = do
  let bufferInfo = Vk.BufferCreateInfo
        { Vk.next        = ()
        , Vk.flags       = Vk.zero
        , Vk.size        = size
        , Vk.usage       = usage
        , Vk.sharingMode = Vk.SHARING_MODE_EXCLUSIVE
        , Vk.queueFamilyIndices = V.empty
        }
  buffer <- Vk.createBuffer device bufferInfo Nothing

  memReqs <- Vk.getBufferMemoryRequirements device buffer
  let Vk.MemoryRequirements{memoryTypeBits = reqMemTypeBits, size = reqSize} = memReqs
  memTypeIdx <- findMemoryType physDevice reqMemTypeBits memProps

  let allocInfo = Vk.MemoryAllocateInfo
        { Vk.next            = ()
        , Vk.allocationSize  = reqSize
        , Vk.memoryTypeIndex = memTypeIdx
        }
  memory <- Vk.allocateMemory device allocInfo Nothing
  Vk.bindBufferMemory device buffer memory 0

  pure $ BufferAllocation buffer memory size

-- | Find suitable memory type index
findMemoryType :: Vk.PhysicalDevice -> Word32 -> Vk.MemoryPropertyFlags -> IO Word32
findMemoryType physDevice typeFilter properties = do
  memProps <- Vk.getPhysicalDeviceMemoryProperties physDevice
  let memTypes = Vk.memoryTypes memProps
      count    = Vk.memoryTypeCount memProps
      go i
        | i >= fromIntegral count = error "Failed to find suitable memory type"
        | testBit typeFilter i
          && (Vk.propertyFlags (memTypes V.! fromIntegral i) .&. properties) == properties
          = pure i
        | otherwise = go (i + 1)
  go 0
  where
    testBit bits i = bits .&. (1 `shiftL` fromIntegral i) /= 0

-- | Destroy a buffer and free its memory
destroyBuffer :: Vk.Device -> BufferAllocation -> IO ()
destroyBuffer device ba = do
  Vk.destroyBuffer device (baBuffer ba) Nothing
  Vk.freeMemory device (baMemory ba) Nothing

-- | Copy data between buffers using a one-shot command buffer
copyBuffer :: Vk.Device -> Vk.CommandPool -> Vk.Queue -> Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> IO ()
copyBuffer device cmdPool queue src dst size = do
  let allocInfo = Vk.CommandBufferAllocateInfo
        { Vk.commandPool        = cmdPool
        , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
  cmdBuffers <- Vk.allocateCommandBuffers device allocInfo
  let cmdBuf = V.head cmdBuffers

  let beginInfo = Vk.CommandBufferBeginInfo
        { Vk.next            = ()
        , Vk.flags           = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        , Vk.inheritanceInfo = Nothing
        }
  Vk.beginCommandBuffer cmdBuf beginInfo

  let copyRegion = Vk.BufferCopy { Vk.srcOffset = 0, Vk.dstOffset = 0, Vk.size = size }
  Vk.cmdCopyBuffer cmdBuf src dst (V.singleton copyRegion)

  Vk.endCommandBuffer cmdBuf

  let submitInfo = Vk.SubmitInfo
        { Vk.next             = ()
        , Vk.waitSemaphores   = V.empty
        , Vk.waitDstStageMask = V.empty
        , Vk.commandBuffers   = V.singleton (Vk.commandBufferHandle cmdBuf)
        , Vk.signalSemaphores = V.empty
        }
  Vk.queueSubmit queue (V.singleton (Vk.SomeStruct submitInfo)) Vk.zero
  Vk.queueWaitIdle queue
  Vk.freeCommandBuffers device cmdPool cmdBuffers

-- | Upload data via staging buffer to a GPU-local buffer
withStagingBuffer
  :: Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.CommandPool
  -> Vk.Queue
  -> Vk.DeviceSize
  -> Vk.BufferUsageFlags
  -> (Ptr () -> IO ())
  -> IO BufferAllocation
withStagingBuffer physDevice device cmdPool queue size dstUsage writeData = do
  staging <- createBuffer physDevice device size
    Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  ptr <- Vk.mapMemory device (baMemory staging) 0 size Vk.zero
  writeData ptr
  Vk.unmapMemory device (baMemory staging)

  gpuBuf <- createBuffer physDevice device size
    (dstUsage .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
    Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  copyBuffer device cmdPool queue (baBuffer staging) (baBuffer gpuBuf) size
  destroyBuffer device staging
  pure gpuBuf

-- | Create vertex buffer from storable vector via staging
createVertexBuffer
  :: (Storable a)
  => Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> VS.Vector a -> IO BufferAllocation
createVertexBuffer physDevice device cmdPool queue vertices = do
  let elemSize = sizeOf (VS.head vertices)
      size = fromIntegral $ VS.length vertices * elemSize
  withStagingBuffer physDevice device cmdPool queue size Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT $ \ptr ->
    VS.unsafeWith vertices $ \srcPtr ->
      copyBytes (castPtr ptr) srcPtr (fromIntegral size)

-- | Create index buffer from storable vector via staging
createIndexBuffer
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.CommandPool -> Vk.Queue
  -> VS.Vector Word32 -> IO BufferAllocation
createIndexBuffer physDevice device cmdPool queue indices = do
  let size = fromIntegral $ VS.length indices * sizeOf (0 :: Word32)
  withStagingBuffer physDevice device cmdPool queue size Vk.BUFFER_USAGE_INDEX_BUFFER_BIT $ \ptr ->
    VS.unsafeWith indices $ \srcPtr ->
      copyBytes (castPtr ptr) srcPtr (fromIntegral size)

-- | Create host-visible uniform buffer (updated every frame, no staging needed)
createUniformBuffer
  :: Vk.PhysicalDevice -> Vk.Device -> Vk.DeviceSize -> IO BufferAllocation
createUniformBuffer physDevice device size =
  createBuffer physDevice device size
    Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
    (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
