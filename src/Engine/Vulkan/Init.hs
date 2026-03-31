{-# LANGUAGE OverloadedLists #-}
module Engine.Vulkan.Init
  ( VulkanContext(..)
  , QueueFamilyIndices(..)
  , createVulkanContext
  , destroyVulkanContext
  , findQueueFamilies
  ) where

import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Extensions.VK_KHR_swapchain as Vk
import qualified Vulkan.Extensions.VK_EXT_debug_utils as Vk
import qualified Graphics.UI.GLFW as GLFW

import Control.Monad (unless, when, forM_)
import Control.Exception (throwIO)
import Data.Bits ((.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Storable (peek)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (peekCString)
import Data.Maybe (isJust)

-- | Queue family indices
data QueueFamilyIndices = QueueFamilyIndices
  { qfGraphicsFamily :: !Word32
  , qfPresentFamily  :: !Word32
  } deriving stock (Show, Eq)

-- | Core Vulkan context: instance, device, queues
data VulkanContext = VulkanContext
  { vcInstance       :: !Vk.Instance
  , vcPhysicalDevice :: !Vk.PhysicalDevice
  , vcDevice         :: !Vk.Device
  , vcGraphicsQueue  :: !Vk.Queue
  , vcPresentQueue   :: !Vk.Queue
  , vcSurface        :: !Vk.SurfaceKHR
  , vcQueueFamilies  :: !QueueFamilyIndices
  , vcDebugMessenger :: !(Maybe Vk.DebugUtilsMessengerEXT)
  } deriving stock (Show)

-- | Validation layers
validationLayers :: Vector ByteString
validationLayers = ["VK_LAYER_KHRONOS_validation"]

-- | Required device extensions
deviceExtensions :: Vector ByteString
deviceExtensions = [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]

-- | Create the full Vulkan context
createVulkanContext :: GLFW.Window -> Bool -> IO VulkanContext
createVulkanContext window enableValidation = do
  inst <- createInstance enableValidation
  debugMessenger <- if enableValidation
    then Just <$> setupDebugMessenger inst
    else pure Nothing
  surface <- createSurface inst window
  physDevice <- pickPhysicalDevice inst surface
  queueFamilies <- findQueueFamilies physDevice surface
  (device, graphicsQueue, presentQueue) <- createLogicalDevice physDevice queueFamilies enableValidation
  pure VulkanContext
    { vcInstance       = inst
    , vcPhysicalDevice = physDevice
    , vcDevice         = device
    , vcGraphicsQueue  = graphicsQueue
    , vcPresentQueue   = presentQueue
    , vcSurface        = surface
    , vcQueueFamilies  = queueFamilies
    , vcDebugMessenger = debugMessenger
    }

-- | Destroy Vulkan context in reverse order
destroyVulkanContext :: VulkanContext -> IO ()
destroyVulkanContext vc = do
  Vk.deviceWaitIdle (vcDevice vc)
  Vk.destroyDevice (vcDevice vc) Nothing
  forM_ (vcDebugMessenger vc) $ \dm ->
    Vk.destroyDebugUtilsMessengerEXT (vcInstance vc) dm Nothing
  Vk.destroySurfaceKHR (vcInstance vc) (vcSurface vc) Nothing
  Vk.destroyInstance (vcInstance vc) Nothing

-- | Create Vulkan instance with GLFW extensions
createInstance :: Bool -> IO Vk.Instance
createInstance enableValidation = do
  glfwExtensionsCStrs <- GLFW.getRequiredInstanceExtensions
  glfwExtensionsBSs <- mapM (\cs -> BS.packCString cs) glfwExtensionsCStrs
  let allExtensions = V.fromList glfwExtensionsBSs
        <> (if enableValidation
             then V.singleton Vk.EXT_DEBUG_UTILS_EXTENSION_NAME
             else V.empty)

  let appInfo = Vk.ApplicationInfo
        { Vk.applicationName    = Just "Minecraft Haskell"
        , Vk.applicationVersion = Vk.MAKE_API_VERSION 0 1 0
        , Vk.engineName         = Just "MCH Engine"
        , Vk.engineVersion      = Vk.MAKE_API_VERSION 0 1 0
        , Vk.apiVersion         = Vk.API_VERSION_1_3
        }

  let createInfo = Vk.InstanceCreateInfo
        { Vk.next                   = ()
        , Vk.flags                  = Vk.zero
        , Vk.applicationInfo        = Just appInfo
        , Vk.enabledLayerNames      = if enableValidation then validationLayers else V.empty
        , Vk.enabledExtensionNames  = allExtensions
        }

  Vk.createInstance createInfo Nothing

-- | Setup debug messenger for validation layer output
setupDebugMessenger :: Vk.Instance -> IO Vk.DebugUtilsMessengerEXT
setupDebugMessenger inst = do
  let createInfo = Vk.DebugUtilsMessengerCreateInfoEXT
        { Vk.flags           = Vk.zero
        , Vk.messageSeverity = Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                             .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
        , Vk.messageType     = Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                             .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                             .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
        , Vk.pfnUserCallback = Vk.zero  -- Use default callback
        , Vk.userData         = nullPtr
        }
  Vk.createDebugUtilsMessengerEXT inst createInfo Nothing

-- | Create Vulkan surface from GLFW window
createSurface :: Vk.Instance -> GLFW.Window -> IO Vk.SurfaceKHR
createSurface inst window = do
  let instPtr = castPtr (Vk.instanceHandle inst)
  alloca $ \surfacePtr -> do
    result <- GLFW.createWindowSurface instPtr window nullPtr surfacePtr
    when (result /= (0 :: Int32)) $
      throwIO $ userError $ "Failed to create window surface, error code: " ++ show result
    surfaceWord <- peek surfacePtr
    pure (Vk.SurfaceKHR surfaceWord)

-- | Pick a suitable physical device
pickPhysicalDevice :: Vk.Instance -> Vk.SurfaceKHR -> IO Vk.PhysicalDevice
pickPhysicalDevice inst surface = do
  (_, devices) <- Vk.enumeratePhysicalDevices inst
  when (V.null devices) $
    throwIO $ userError "No Vulkan-capable GPU found"

  -- Find first device that supports our requirements
  suitable <- V.filterM (isDeviceSuitable surface) devices
  when (V.null suitable) $
    throwIO $ userError "No suitable GPU found"

  -- Prefer discrete GPU
  let pickBest ds = do
        props <- mapM Vk.getPhysicalDeviceProperties (V.toList ds)
        let scored = zip (V.toList ds) props
        let discrete = [d | (d, p) <- scored, Vk.deviceType p == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU]
        pure $ case discrete of
          (d:_) -> d
          []    -> V.head ds
  pickBest suitable

-- | Check if a physical device is suitable
isDeviceSuitable :: Vk.SurfaceKHR -> Vk.PhysicalDevice -> IO Bool
isDeviceSuitable surface device = do
  indices <- findQueueFamiliesMaybe device surface
  extensionsSupported <- checkDeviceExtensionSupport device
  pure $ isJust indices && extensionsSupported

-- | Check device extension support
checkDeviceExtensionSupport :: Vk.PhysicalDevice -> IO Bool
checkDeviceExtensionSupport device = do
  (_, extensions) <- Vk.enumerateDeviceExtensionProperties device Nothing
  let available = V.map Vk.extensionName extensions
  pure $ all (`elem` V.toList available) (V.toList deviceExtensions)

-- | Find queue families, returns Nothing if incomplete
findQueueFamiliesMaybe :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO (Maybe QueueFamilyIndices)
findQueueFamiliesMaybe device surface = do
  queueFamilies <- Vk.getPhysicalDeviceQueueFamilyProperties device
  let indexed = V.indexed queueFamilies

  let graphicsFamily = V.find (\(_, qf) ->
        Vk.queueFlags qf .&. Vk.QUEUE_GRAPHICS_BIT /= Vk.zero
        ) indexed

  presentFamily <- case graphicsFamily of
    Nothing -> pure Nothing
    Just _  -> do
      results <- V.mapM (\(i, _) -> do
        supported <- Vk.getPhysicalDeviceSurfaceSupportKHR device (fromIntegral i) surface
        pure (i, supported)
        ) indexed
      pure $ fst <$> V.find snd results

  pure $ case (graphicsFamily, presentFamily) of
    (Just (gi, _), Just pi') -> Just $ QueueFamilyIndices (fromIntegral gi) (fromIntegral pi')
    _ -> Nothing

-- | Find queue families, throws if incomplete
findQueueFamilies :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO QueueFamilyIndices
findQueueFamilies device surface = do
  result <- findQueueFamiliesMaybe device surface
  case result of
    Just indices -> pure indices
    Nothing      -> throwIO $ userError "Failed to find required queue families"

-- | Create logical device and retrieve queues
createLogicalDevice :: Vk.PhysicalDevice -> QueueFamilyIndices -> Bool -> IO (Vk.Device, Vk.Queue, Vk.Queue)
createLogicalDevice physDevice queueFamilies enableValidation = do
  let uniqueFamilies = if qfGraphicsFamily queueFamilies == qfPresentFamily queueFamilies
        then [qfGraphicsFamily queueFamilies]
        else [qfGraphicsFamily queueFamilies, qfPresentFamily queueFamilies]

  let queueCreateInfos = V.fromList
        [ Vk.DeviceQueueCreateInfo
            { Vk.next              = ()
            , Vk.flags             = Vk.zero
            , Vk.queueFamilyIndex  = qf
            , Vk.queuePriorities   = [1.0]
            }
        | qf <- uniqueFamilies
        ]

  let deviceFeatures = Vk.zero :: Vk.PhysicalDeviceFeatures

  let createInfo = Vk.DeviceCreateInfo
        { Vk.next                   = ()
        , Vk.flags                  = Vk.zero
        , Vk.queueCreateInfos       = Vk.SomeStruct <$> queueCreateInfos
        , Vk.enabledLayerNames      = if enableValidation then validationLayers else V.empty
        , Vk.enabledExtensionNames  = deviceExtensions
        , Vk.enabledFeatures        = Just deviceFeatures
        }

  device <- Vk.createDevice physDevice createInfo Nothing
  graphicsQueue <- Vk.getDeviceQueue device (qfGraphicsFamily queueFamilies) 0
  presentQueue  <- Vk.getDeviceQueue device (qfPresentFamily queueFamilies) 0

  pure (device, graphicsQueue, presentQueue)
