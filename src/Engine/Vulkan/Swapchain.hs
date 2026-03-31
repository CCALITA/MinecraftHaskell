module Engine.Vulkan.Swapchain
  ( SwapchainContext(..)
  , SwapchainSupportDetails(..)
  , createSwapchain
  , destroySwapchain
  , recreateSwapchain
  , querySwapchainSupport
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

import Engine.Vulkan.Init (VulkanContext(..), QueueFamilyIndices(..))

import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)

-- | Swapchain support details
data SwapchainSupportDetails = SwapchainSupportDetails
  { ssdCapabilities :: !Vk.SurfaceCapabilitiesKHR
  , ssdFormats      :: !(Vector Vk.SurfaceFormatKHR)
  , ssdPresentModes :: !(Vector Vk.PresentModeKHR)
  } deriving stock (Show)

-- | Swapchain and related resources
data SwapchainContext = SwapchainContext
  { scSwapchain   :: !Vk.SwapchainKHR
  , scImages      :: !(Vector Vk.Image)
  , scImageViews  :: !(Vector Vk.ImageView)
  , scFormat      :: !Vk.Format
  , scExtent      :: !Vk.Extent2D
  } deriving stock (Show)

-- | Query swapchain support details
querySwapchainSupport :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO SwapchainSupportDetails
querySwapchainSupport device surface = do
  capabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR device surface
  (_, formats)      <- Vk.getPhysicalDeviceSurfaceFormatsKHR device surface
  (_, presentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR device surface
  pure $ SwapchainSupportDetails capabilities formats presentModes

-- | Choose swapchain surface format (prefer SRGB B8G8R8A8)
chooseSurfaceFormat :: Vector Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSurfaceFormat formats =
  case V.find isPreferred formats of
    Just fmt -> fmt
    Nothing  -> V.head formats
  where
    isPreferred f =
      let Vk.SurfaceFormatKHR{format = fmt, colorSpace = cs} = f
      in fmt == Vk.FORMAT_B8G8R8A8_SRGB
         && cs == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

-- | Choose present mode (prefer mailbox for triple buffering, fallback to FIFO)
choosePresentMode :: Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
choosePresentMode modes
  | Vk.PRESENT_MODE_MAILBOX_KHR `V.elem` modes = Vk.PRESENT_MODE_MAILBOX_KHR
  | otherwise = Vk.PRESENT_MODE_FIFO_KHR

-- | Choose swap extent
chooseExtent :: Vk.SurfaceCapabilitiesKHR -> (Int, Int) -> Vk.Extent2D
chooseExtent capabilities (width, height) =
  let Vk.SurfaceCapabilitiesKHR{ currentExtent = current
                                , minImageExtent = minE
                                , maxImageExtent = maxE } = capabilities
      Vk.Extent2D{width = curW} = current
  in if curW /= maxBound
     then current
     else Vk.Extent2D
       { Vk.width  = clampW minE maxE (fromIntegral width)
       , Vk.height = clampH minE maxE (fromIntegral height)
       }
  where
    clampW minE maxE w = let Vk.Extent2D{width = minW} = minE
                             Vk.Extent2D{width = maxW} = maxE
                         in max minW (min maxW w)
    clampH minE maxE h = let Vk.Extent2D{height = minH} = minE
                             Vk.Extent2D{height = maxH} = maxE
                         in max minH (min maxH h)

-- | Create swapchain
createSwapchain :: VulkanContext -> (Int, Int) -> IO SwapchainContext
createSwapchain vc windowSize = do
  support <- querySwapchainSupport (vcPhysicalDevice vc) (vcSurface vc)

  let surfaceFormat = chooseSurfaceFormat (ssdFormats support)
      presentMode   = choosePresentMode (ssdPresentModes support)
      extent        = chooseExtent (ssdCapabilities support) windowSize
      caps          = ssdCapabilities support

  let Vk.SurfaceCapabilitiesKHR{ minImageCount = capsMinCount
                                , maxImageCount = capsMaxCount
                                , currentTransform = capsTransform } = caps

  let imageCount' = capsMinCount + 1
      imageCount  = if capsMaxCount > 0
                    then min imageCount' capsMaxCount
                    else imageCount'

  let qfi = vcQueueFamilies vc
      sameFamily = qfGraphicsFamily qfi == qfPresentFamily qfi

  let Vk.SurfaceFormatKHR{format = sfFormat, colorSpace = sfColorSpace} = surfaceFormat

  let createInfo = Vk.SwapchainCreateInfoKHR
        { Vk.next                  = ()
        , Vk.flags                 = Vk.zero
        , Vk.surface               = vcSurface vc
        , Vk.minImageCount         = imageCount
        , Vk.imageFormat           = sfFormat
        , Vk.imageColorSpace       = sfColorSpace
        , Vk.imageExtent           = extent
        , Vk.imageArrayLayers      = 1
        , Vk.imageUsage            = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        , Vk.imageSharingMode      = if sameFamily
                                     then Vk.SHARING_MODE_EXCLUSIVE
                                     else Vk.SHARING_MODE_CONCURRENT
        , Vk.queueFamilyIndices    = if sameFamily
                                     then V.empty
                                     else V.fromList [qfGraphicsFamily qfi, qfPresentFamily qfi]
        , Vk.preTransform          = capsTransform
        , Vk.compositeAlpha        = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , Vk.presentMode           = presentMode
        , Vk.clipped               = True
        , Vk.oldSwapchain          = Vk.zero
        }

  swapchain <- Vk.createSwapchainKHR (vcDevice vc) createInfo Nothing
  (_, images) <- Vk.getSwapchainImagesKHR (vcDevice vc) swapchain

  imageViews <- V.mapM (createImageView (vcDevice vc) sfFormat) images

  pure SwapchainContext
    { scSwapchain  = swapchain
    , scImages     = images
    , scImageViews = imageViews
    , scFormat     = sfFormat
    , scExtent     = extent
    }

-- | Create an image view for a swapchain image
createImageView :: Vk.Device -> Vk.Format -> Vk.Image -> IO Vk.ImageView
createImageView device format image = do
  let createInfo = Vk.ImageViewCreateInfo
        { Vk.next             = ()
        , Vk.flags            = Vk.zero
        , Vk.image            = image
        , Vk.viewType         = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format           = format
        , Vk.components       = Vk.ComponentMapping
            Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY
            Vk.COMPONENT_SWIZZLE_IDENTITY
        , Vk.subresourceRange = Vk.ImageSubresourceRange
            { Vk.aspectMask     = Vk.IMAGE_ASPECT_COLOR_BIT
            , Vk.baseMipLevel   = 0
            , Vk.levelCount     = 1
            , Vk.baseArrayLayer = 0
            , Vk.layerCount     = 1
            }
        }
  Vk.createImageView device createInfo Nothing

-- | Destroy swapchain and image views
destroySwapchain :: Vk.Device -> SwapchainContext -> IO ()
destroySwapchain device sc = do
  V.mapM_ (\iv -> Vk.destroyImageView device iv Nothing) (scImageViews sc)
  Vk.destroySwapchainKHR device (scSwapchain sc) Nothing

-- | Recreate swapchain (on window resize)
recreateSwapchain :: VulkanContext -> SwapchainContext -> (Int, Int) -> IO SwapchainContext
recreateSwapchain vc oldSc windowSize = do
  Vk.deviceWaitIdle (vcDevice vc)
  destroySwapchain (vcDevice vc) oldSc
  createSwapchain vc windowSize
