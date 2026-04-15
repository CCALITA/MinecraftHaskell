module Engine.Window
  ( withWindow
  , pollWindowEvents
  , windowShouldClose
  , getWindowSize
  , getWindowScreenSize
  , WindowHandle(..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Exception (bracket_, throwIO)
import Control.Monad (unless)
import Data.IORef

-- | Opaque window handle wrapping GLFW window
data WindowHandle = WindowHandle
  { whWindow          :: !GLFW.Window
  , whFramebufferSize :: !(IORef (Int, Int))
  , whWindowSize      :: !(IORef (Int, Int))
  , whResized         :: !(IORef Bool)
  }

-- | Initialize GLFW, create a window with Vulkan support, run action, cleanup.
withWindow :: Int -> Int -> String -> (WindowHandle -> IO a) -> IO a
withWindow width height title action = do
  initSuccess <- GLFW.init
  unless initSuccess $
    throwIO $ userError "Failed to initialize GLFW"

  -- Request no OpenGL context — we use Vulkan
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  GLFW.windowHint $ GLFW.WindowHint'Resizable True

  mWindow <- GLFW.createWindow width height title Nothing Nothing
  case mWindow of
    Nothing -> do
      GLFW.terminate
      throwIO $ userError "Failed to create GLFW window"
    Just window -> do
      sizeRef    <- newIORef (width, height)
      winSizeRef <- newIORef (width, height)
      resizedRef <- newIORef False

      -- Set framebuffer resize callback
      GLFW.setFramebufferSizeCallback window $ Just $ \_win w h -> do
        writeIORef sizeRef (w, h)
        writeIORef resizedRef True

      -- Track window size in screen coordinates (for cursor-to-NDC conversion)
      GLFW.setWindowSizeCallback window $ Just $ \_win w h ->
        writeIORef winSizeRef (w, h)

      -- Set key callback: ESC to close
      GLFW.setKeyCallback window $ Just $ \_win key _scancode action' _mods ->
        case (key, action') of
          (GLFW.Key'Escape, GLFW.KeyState'Pressed) ->
            GLFW.setWindowShouldClose window True
          _ -> pure ()

      let handle = WindowHandle window sizeRef winSizeRef resizedRef
      result <- action handle

      GLFW.destroyWindow window
      GLFW.terminate
      pure result

-- | Poll for GLFW events
pollWindowEvents :: IO ()
pollWindowEvents = GLFW.pollEvents

-- | Check if window should close
windowShouldClose :: WindowHandle -> IO Bool
windowShouldClose wh = GLFW.windowShouldClose (whWindow wh)

-- | Get current framebuffer size (for rendering/swapchain)
getWindowSize :: WindowHandle -> IO (Int, Int)
getWindowSize wh = readIORef (whFramebufferSize wh)

-- | Get current window size in screen coordinates (for cursor-to-NDC conversion)
getWindowScreenSize :: WindowHandle -> IO (Int, Int)
getWindowScreenSize wh = readIORef (whWindowSize wh)
