module Engine.Vulkan.Descriptor
  ( DescriptorContext(..)
  , createDescriptorSetLayout
  , createDescriptorPool
  , allocateDescriptorSets
  , updateDescriptorSet
  , destroyDescriptorContext
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Memory (BufferAllocation(..))
import Engine.Types (UniformBufferObject)

import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Storable (sizeOf)

-- | Descriptor resources
data DescriptorContext = DescriptorContext
  { dcSetLayout :: !Vk.DescriptorSetLayout
  , dcPool      :: !Vk.DescriptorPool
  , dcSets      :: !(V.Vector Vk.DescriptorSet)
  } deriving stock (Show)

-- | Create descriptor set layout for UBO binding at binding 0
createDescriptorSetLayout :: Vk.Device -> IO Vk.DescriptorSetLayout
createDescriptorSetLayout device = do
  let uboBinding = Vk.DescriptorSetLayoutBinding
        { Vk.binding            = 0
        , Vk.descriptorType     = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , Vk.descriptorCount    = 1
        , Vk.stageFlags         = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.immutableSamplers  = V.empty
        }

  -- Binding 1: texture sampler (for block atlas)
  let samplerBinding = Vk.DescriptorSetLayoutBinding
        { Vk.binding            = 1
        , Vk.descriptorType     = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
        , Vk.descriptorCount    = 1
        , Vk.stageFlags         = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.immutableSamplers  = V.empty
        }

  let createInfo = Vk.DescriptorSetLayoutCreateInfo
        { Vk.next     = ()
        , Vk.flags    = Vk.zero
        , Vk.bindings = V.fromList [uboBinding, samplerBinding]
        }
  Vk.createDescriptorSetLayout device createInfo Nothing

-- | Create descriptor pool for N frames in flight
createDescriptorPool :: Vk.Device -> Int -> IO Vk.DescriptorPool
createDescriptorPool device maxFrames = do
  let poolSizes =
        [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER (fromIntegral maxFrames)
        , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER (fromIntegral maxFrames)
        ]
  let createInfo = Vk.DescriptorPoolCreateInfo
        { Vk.next      = ()
        , Vk.flags     = Vk.zero
        , Vk.maxSets   = fromIntegral maxFrames
        , Vk.poolSizes = V.fromList poolSizes
        }
  Vk.createDescriptorPool device createInfo Nothing

-- | Allocate one descriptor set per frame in flight
allocateDescriptorSets :: Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout -> Int -> IO (V.Vector Vk.DescriptorSet)
allocateDescriptorSets device pool layout count = do
  let layouts = V.replicate count layout
  let allocInfo = Vk.DescriptorSetAllocateInfo
        { Vk.next              = ()
        , Vk.descriptorPool    = pool
        , Vk.setLayouts        = layouts
        }
  Vk.allocateDescriptorSets device allocInfo

-- | Update a descriptor set to point to the given uniform buffer
updateDescriptorSet :: Vk.Device -> Vk.DescriptorSet -> BufferAllocation -> IO ()
updateDescriptorSet device descriptorSet uniformBuf = do
  let bufferInfo = Vk.DescriptorBufferInfo
        { Vk.buffer = baBuffer uniformBuf
        , Vk.offset = 0
        , Vk.range  = fromIntegral $ sizeOf (undefined :: UniformBufferObject)
        }

  let writeDescriptor = Vk.WriteDescriptorSet
        { Vk.next            = ()
        , Vk.dstSet          = descriptorSet
        , Vk.dstBinding      = 0
        , Vk.dstArrayElement = 0
        , Vk.descriptorCount = 1
        , Vk.descriptorType  = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , Vk.bufferInfo      = V.singleton bufferInfo
        , Vk.imageInfo       = V.empty
        , Vk.texelBufferView = V.empty
        }

  Vk.updateDescriptorSets device (V.singleton (Vk.SomeStruct writeDescriptor)) V.empty

-- | Destroy descriptor resources
destroyDescriptorContext :: Vk.Device -> DescriptorContext -> IO ()
destroyDescriptorContext device dc = do
  Vk.destroyDescriptorPool device (dcPool dc) Nothing
  Vk.destroyDescriptorSetLayout device (dcSetLayout dc) Nothing
