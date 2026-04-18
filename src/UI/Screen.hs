module UI.Screen
  ( ScreenDef(..)
  , ScreenRegistry
  , newScreenRegistry
  , registerScreen
  , lookupScreen
  , allScreens
  ) where

import Data.IORef
import qualified Data.HashMap.Strict as HM

import Game.State (GameState)

-- | Definition of a UI screen (inventory, crafting table, pause menu, etc.).
-- Each screen provides render, input, and lifecycle callbacks.
data ScreenDef = ScreenDef
  { sdName        :: !String
  , sdRender      :: !(GameState -> [Float])       -- ^ HUD vertex data
  , sdHandleClick :: !(GameState -> Float -> Float -> IO ())  -- ^ NDC x,y
  , sdHandleKey   :: !(GameState -> Int -> IO ())   -- ^ key code
  , sdOnOpen      :: !(GameState -> IO ())
  , sdOnClose     :: !(GameState -> IO ())
  }

-- | Mutable registry mapping screen names to their definitions.
type ScreenRegistry = IORef (HM.HashMap String ScreenDef)

-- | Create an empty screen registry.
newScreenRegistry :: IO ScreenRegistry
newScreenRegistry = newIORef HM.empty

-- | Register a screen definition. Overwrites any existing screen with the
-- same name.
registerScreen :: ScreenRegistry -> ScreenDef -> IO ()
registerScreen reg sd =
  modifyIORef' reg (HM.insert (sdName sd) sd)

-- | Look up a screen by name. Returns 'Nothing' if not registered.
lookupScreen :: ScreenRegistry -> String -> IO (Maybe ScreenDef)
lookupScreen reg name = do
  m <- readIORef reg
  pure (HM.lookup name m)

-- | Return all registered screen definitions.
allScreens :: ScreenRegistry -> IO [ScreenDef]
allScreens reg = do
  m <- readIORef reg
  pure (HM.elems m)
