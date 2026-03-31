module UI.Menu
  ( MenuState(..)
  , MenuScreen(..)
  , MenuItem(..)
  , MenuAction(..)
  , mainMenuItems
  , pauseMenuItems
  , settingsMenuItems
  , handleMenuInput
  , isMenuOpen
  ) where

-- | Menu screens
data MenuScreen
  = MainMenu
  | PauseMenu
  | SettingsMenu
  | InventoryScreen
  | CraftingScreen
  | NoMenu               -- game is active, no menu open
  deriving stock (Show, Eq)

-- | Menu state
data MenuState = MenuState
  { msScreen    :: !MenuScreen
  , msSelected  :: !Int          -- currently highlighted item index
  , msPrevScreen :: !MenuScreen  -- for back navigation
  } deriving stock (Show, Eq)

-- | A menu item
data MenuItem = MenuItem
  { miLabel  :: !String
  , miAction :: !MenuAction
  } deriving stock (Show, Eq)

-- | Actions that menu items can trigger
data MenuAction
  = MAStartGame             -- start new single player world
  | MALoadGame              -- load saved world
  | MAJoinServer            -- connect to multiplayer server
  | MASettings              -- open settings
  | MAResumeGame            -- unpause
  | MASaveAndQuit           -- save world and return to main menu
  | MAQuitGame              -- exit application
  | MABack                  -- go back to previous menu
  | MAToggleOption !String  -- toggle a setting
  deriving stock (Show, Eq)

-- | Main menu items
mainMenuItems :: [MenuItem]
mainMenuItems =
  [ MenuItem "Singleplayer"  MAStartGame
  , MenuItem "Multiplayer"   MAJoinServer
  , MenuItem "Settings"      MASettings
  , MenuItem "Quit Game"     MAQuitGame
  ]

-- | Pause menu items
pauseMenuItems :: [MenuItem]
pauseMenuItems =
  [ MenuItem "Back to Game"     MAResumeGame
  , MenuItem "Settings"         MASettings
  , MenuItem "Save and Quit"    MASaveAndQuit
  ]

-- | Settings menu items
settingsMenuItems :: [MenuItem]
settingsMenuItems =
  [ MenuItem "Render Distance"  (MAToggleOption "renderDist")
  , MenuItem "FOV"              (MAToggleOption "fov")
  , MenuItem "Mouse Sensitivity" (MAToggleOption "sensitivity")
  , MenuItem "Fullscreen"       (MAToggleOption "fullscreen")
  , MenuItem "VSync"            (MAToggleOption "vsync")
  , MenuItem "Back"             MABack
  ]

-- | Handle input in menu (up/down/select)
--   Returns updated menu state and the action if selected
handleMenuInput :: MenuState -> MenuInput -> ([MenuItem] -> (MenuState, Maybe MenuAction))
handleMenuInput ms input items =
  let maxIdx = length items - 1
  in case input of
    MIUp ->
      (ms { msSelected = max 0 (msSelected ms - 1) }, Nothing)
    MIDown ->
      (ms { msSelected = min maxIdx (msSelected ms + 1) }, Nothing)
    MISelect ->
      if msSelected ms >= 0 && msSelected ms <= maxIdx
        then (ms, Just (miAction (items !! msSelected ms)))
        else (ms, Nothing)
    MIBack ->
      (ms { msScreen = msPrevScreen ms }, Nothing)

-- | Menu input types
data MenuInput = MIUp | MIDown | MISelect | MIBack
  deriving stock (Show, Eq)

-- | Check if any menu is currently open
isMenuOpen :: MenuState -> Bool
isMenuOpen ms = msScreen ms /= NoMenu
