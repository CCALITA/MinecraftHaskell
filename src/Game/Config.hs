module Game.Config
  ( GameConfig(..)
  , defaultConfig
  ) where

import Game.Item (Item(..))

-- | Centralized game configuration — pure, no IO.
data GameConfig = GameConfig
  { cfgRenderDistance   :: !Int          -- ^ Chunks to render around player
  , cfgTickRate         :: !Float        -- ^ Physics ticks per second
  , cfgMaxReach         :: !Float        -- ^ Block interaction range (blocks)
  , cfgFOV              :: !Float        -- ^ Field of view (degrees)
  , cfgMouseSensitivity :: !Float        -- ^ Mouse look sensitivity
  , cfgDayLength        :: !Float        -- ^ Full day/night cycle (seconds)
  , cfgMaxSpawnHostile  :: !Int          -- ^ Cap on hostile mobs in loaded area
  , cfgMaxSpawnPassive  :: !Int          -- ^ Cap on passive mobs in loaded area
  , cfgSpawnRadius      :: !Int          -- ^ Min distance from player to mob spawn
  , cfgDespawnDistance   :: !Int          -- ^ Distance at which mobs despawn
  , cfgAutoSaveInterval :: !Int          -- ^ Frames between auto-saves
  , cfgStartingItems    :: ![(Item, Int)]
  } deriving stock (Show, Eq)

-- | Sensible defaults matching current hardcoded values.
defaultConfig :: GameConfig
defaultConfig = GameConfig
  { cfgRenderDistance   = 4
  , cfgTickRate         = 20.0
  , cfgMaxReach         = 5.0
  , cfgFOV              = 45.0
  , cfgMouseSensitivity = 0.15
  , cfgDayLength        = 1200.0
  , cfgMaxSpawnHostile  = 20
  , cfgMaxSpawnPassive  = 10
  , cfgSpawnRadius      = 24
  , cfgDespawnDistance   = 128
  , cfgAutoSaveInterval = 18000
  , cfgStartingItems    = []
  }
