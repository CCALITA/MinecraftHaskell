module Engine.Vulkan.Command
  ( FrameSyncObjects(..)
  , createCommandPool
  , allocateCommandBuffers
  , createSyncObjects
  , destroySyncObjects
  , recordCommandBuffer
  , drawFrame
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk

import Engine.Vulkan.Init (VulkanContext(..))
import Engine.Vulkan.Swapchain (SwapchainContext(..))
import Engine.Vulkan.Pipeline (PipelineContext(..))

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Data.Bits ((.|.))

-- | Per-frame synchronization objects
data FrameSyncObjects = FrameSyncObjects
  { fsoImageAvailable :: !Vk.Semaphore
  , fsoRenderFinished :: !Vk.Semaphore
  , fsoInFlight       :: !Vk.Fence
  } deriving stock (Show)

-- | Create a command pool for the graphics queue family
createCommandPool :: Vk.Device -> Word32 -> IO Vk.CommandPool
createCommandPool device queueFamily = do
  let createInfo = Vk.CommandPoolCreateInfo
        { Vk.flags            = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        , Vk.queueFamilyIndex = queueFamily
        }
  Vk.createCommandPool device createInfo Nothing

-- | Allocate command buffers from pool
allocateCommandBuffers :: Vk.Device -> Vk.CommandPool -> Int -> IO (Vector Vk.CommandBuffer)
allocateCommandBuffers device pool count = do
  let allocInfo = Vk.CommandBufferAllocateInfo
        { Vk.commandPool        = pool
        , Vk.level              = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = fromIntegral count
        }
  Vk.allocateCommandBuffers device allocInfo

-- | Create sync objects for each frame in flight
createSyncObjects :: Vk.Device -> Int -> IO (Vector FrameSyncObjects)
createSyncObjects device count =
  V.replicateM count $ do
    let semaphoreInfo = Vk.SemaphoreCreateInfo
          { Vk.next  = ()
          , Vk.flags = Vk.zero
          }
    let fenceInfo = Vk.FenceCreateInfo
          { Vk.next  = ()
          , Vk.flags = Vk.FENCE_CREATE_SIGNALED_BIT  -- Start signaled so first frame works
          }
    imageAvail   <- Vk.createSemaphore device semaphoreInfo Nothing
    renderFinish <- Vk.createSemaphore device semaphoreInfo Nothing
    inFlight     <- Vk.createFence device fenceInfo Nothing
    pure $ FrameSyncObjects imageAvail renderFinish inFlight

-- | Destroy sync objects
destroySyncObjects :: Vk.Device -> Vector FrameSyncObjects -> IO ()
destroySyncObjects device = V.mapM_ $ \fso -> do
  Vk.destroySemaphore device (fsoImageAvailable fso) Nothing
  Vk.destroySemaphore device (fsoRenderFinished fso) Nothing
  Vk.destroyFence device (fsoInFlight fso) Nothing

-- | Record a command buffer for rendering a frame
recordCommandBuffer
  :: Vk.CommandBuffer
  -> Vk.RenderPass
  -> Vk.Framebuffer
  -> Vk.Pipeline
  -> Vk.Extent2D
  -> IO ()
recordCommandBuffer cmdBuf renderPass framebuffer pipeline extent = do
  let beginInfo = Vk.CommandBufferBeginInfo
        { Vk.next            = ()
        , Vk.flags           = Vk.zero
        , Vk.inheritanceInfo = Nothing
        }

  Vk.beginCommandBuffer cmdBuf beginInfo

  let clearColor = Vk.Color (Vk.Float32 0.1 0.1 0.2 1.0)  -- Dark blue-ish

  let renderPassBegin = Vk.RenderPassBeginInfo
        { Vk.next           = ()
        , Vk.renderPass     = renderPass
        , Vk.framebuffer    = framebuffer
        , Vk.renderArea     = Vk.Rect2D (Vk.Offset2D 0 0) extent
        , Vk.clearValues    = V.singleton clearColor
        }

  Vk.cmdBeginRenderPass cmdBuf renderPassBegin Vk.SUBPASS_CONTENTS_INLINE
  Vk.cmdBindPipeline cmdBuf Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
  Vk.cmdDraw cmdBuf 3 1 0 0  -- 3 vertices, 1 instance (hardcoded triangle)
  Vk.cmdEndRenderPass cmdBuf
  Vk.endCommandBuffer cmdBuf

-- | Draw a single frame: acquire image, record commands, submit, present.
--   Returns True if swapchain needs recreation.
drawFrame
  :: VulkanContext
  -> SwapchainContext
  -> PipelineContext
  -> Vector Vk.Framebuffer
  -> Vk.CommandBuffer
  -> FrameSyncObjects
  -> IO Bool
drawFrame vc sc pc framebuffers cmdBuf syncObj = do
  let device = vcDevice vc
      maxWait = maxBound :: Word64

  -- Wait for previous frame to finish
  Vk.waitForFences device (V.singleton $ fsoInFlight syncObj) True maxWait
  Vk.resetFences device (V.singleton $ fsoInFlight syncObj)

  -- Acquire next swapchain image
  (result, imageIndex) <- Vk.acquireNextImageKHR
    device
    (scSwapchain sc)
    maxWait
    (fsoImageAvailable syncObj)
    Vk.zero

  case result of
    Vk.ERROR_OUT_OF_DATE_KHR -> pure True
    _ -> do
      -- Reset and record command buffer
      Vk.resetCommandBuffer cmdBuf Vk.zero
      recordCommandBuffer
        cmdBuf
        (pcRenderPass pc)
        (framebuffers V.! fromIntegral imageIndex)
        (pcPipeline pc)
        (scExtent sc)

      -- Submit command buffer
      let submitInfo = Vk.SubmitInfo
            { Vk.next                = ()
            , Vk.waitSemaphores      = V.singleton (fsoImageAvailable syncObj)
            , Vk.waitDstStageMask    = V.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            , Vk.commandBuffers      = V.singleton (Vk.commandBufferHandle cmdBuf)
            , Vk.signalSemaphores    = V.singleton (fsoRenderFinished syncObj)
            }
      Vk.queueSubmit (vcGraphicsQueue vc) (V.singleton (Vk.SomeStruct submitInfo)) (fsoInFlight syncObj)

      -- Present
      let presentInfo = Vk.PresentInfoKHR
            { Vk.next           = ()
            , Vk.waitSemaphores = V.singleton (fsoRenderFinished syncObj)
            , Vk.swapchains     = V.singleton (scSwapchain sc)
            , Vk.imageIndices   = V.singleton imageIndex
            , Vk.results        = nullPtr
            }
      presentResult <- Vk.queuePresentKHR (vcPresentQueue vc) presentInfo

      pure $ presentResult == Vk.ERROR_OUT_OF_DATE_KHR
            || presentResult == Vk.SUBOPTIMAL_KHR
  where
    nullPtr = error "unused"  -- PresentInfoKHR results field
