module Game.State
  ( GameState(..)
  , GameMode(..)
  , Projectile(..)
  , newGameState
  ) where

import Data.IORef
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Linear (V3(..))
import System.FilePath ((</>))
import System.Random (StdGen)
import qualified System.Random

import Game.Player (Player, PlayerInput, defaultPlayer, noInput)
import Game.Inventory (Inventory, ItemStack, emptyInventory)
import Game.Crafting (CraftingGrid, emptyCraftingGrid)
import Game.Furnace (FurnaceState, newFurnaceState)
import Game.Enchanting (EnchantmentType, EnchantmentMap, newEnchantmentMap)
import Game.PotionEffect (ActiveEffect)
import Game.DayNight (DayNightCycle, newDayNightCycle)
import World.Weather (WeatherState, newWeatherState)
import World.Redstone (RedstoneState, newRedstoneState)
import Game.BlockEntity (BlockEntityMap, newBlockEntityMap)
import World.Chunk (ChunkPos)
import Engine.Vulkan.Memory (BufferAllocation)
import Entity.Mob (AIState)

-- | Game UI mode
data GameMode = MainMenu | Playing | Paused | InventoryOpen | CraftingOpen | ChestOpen | FurnaceOpen | DispenserOpen | EnchantingOpen | DeathScreen
  deriving stock (Show, Eq)

-- | Arrow projectile fired by Skeletons
data Projectile = Projectile
  { projPos      :: !(V3 Float)
  , projVelocity :: !(V3 Float)
  , projAge      :: !Float
  , projDamage   :: !Int
  }

-- | Unified game state record holding all mutable IORefs.
-- Every field uses the @gs@ prefix per project conventions.
data GameState = GameState
  { -- Player & gameplay
    gsPlayer           :: !(IORef Player)
  , gsInventory        :: !(IORef Inventory)
  , gsGameMode         :: !(IORef GameMode)
  , gsCursorItem       :: !(IORef (Maybe ItemStack))
  , gsCraftingGrid     :: !(IORef CraftingGrid)
  , gsFurnaceState     :: !(IORef FurnaceState)
  , gsFurnacePos       :: !(IORef (Maybe (V3 Int)))
  , gsChestPos         :: !(IORef (Maybe (V3 Int)))
  , gsDispenserPos     :: !(IORef (Maybe (V3 Int)))
  , gsDayNight         :: !(IORef DayNightCycle)
  , gsWeather          :: !(IORef WeatherState)
  , gsSpawnPoint       :: !(IORef (V3 Float))
  , gsSleepMessage     :: !(IORef (Maybe Float))
  , gsDebugOverlay     :: !(IORef Bool)
  , gsTargetBlock      :: !(IORef (Maybe (V3 Int)))
  , gsRedstone         :: !(IORef RedstoneState)
  , gsBlockEntities    :: !BlockEntityMap
  , gsSaveDir          :: !(IORef FilePath)
    -- Mining & input
  , gsMining           :: !(IORef (Maybe (V3 Int, Float)))
  , gsLmbHeld          :: !(IORef Bool)
  , gsRmbHeld          :: !(IORef Bool)
  , gsInput            :: !(IORef PlayerInput)
  , gsLastCursor       :: !(IORef (Maybe (Double, Double)))
  , gsMousePos         :: !(IORef (Double, Double))
    -- Combat & effects
  , gsDamageFlash      :: !(IORef Float)
  , gsCactusDmgTimer   :: !(IORef Float)
  , gsPoisonTimer      :: !(IORef Float)
  , gsSpeedBuff        :: !(IORef Float)
  , gsActiveEffects    :: !(IORef [ActiveEffect])
  , gsProjectiles      :: !(IORef [Projectile])
  , gsSkeletonCd       :: !(IORef (HM.HashMap Int Float))
  , gsCreeperFuse      :: !(IORef (IM.IntMap Float))
    -- Entity AI & spawning
  , gsAiStates         :: !(IORef (HM.HashMap Int AIState))
  , gsSpawnRng         :: !(IORef StdGen)
  , gsSpawnCooldown    :: !(IORef Float)
    -- Misc gameplay
  , gsFishing          :: !(IORef (Maybe (V3 Float, Float, Bool)))
  , gsRiding           :: !(IORef (Maybe Int))
    -- Enchanting
  , gsEnchantItem      :: !(IORef (Maybe ItemStack))
  , gsEnchantOptions   :: !(IORef [(EnchantmentType, Int, Int)])
  , gsPlayerXP         :: !(IORef Int)
  , gsEnchantMap       :: !EnchantmentMap
    -- Rendering state
  , gsHudVertCount     :: !(IORef Int)
  , gsMeshCache        :: !(IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int)))
  , gsTransMeshCache   :: !(IORef (HM.HashMap ChunkPos (BufferAllocation, BufferAllocation, Int)))
    -- Frame timing
  , gsFrame            :: !(IORef Int)
  , gsLastTime         :: !(IORef Double)
  , gsAccum            :: !(IORef Float)
  , gsFpsCounter       :: !(IORef Int)
  , gsFpsTimer         :: !(IORef Float)
  , gsFpsDisplay       :: !(IORef Int)
  }

-- | Create a fresh GameState with default initial values.
-- The spawn position is provided so the player starts at the right location.
newGameState :: V3 Float -> IO GameState
newGameState spawnPos = do
  rng <- System.Random.newStdGen
  rs  <- newRedstoneState
  bem <- newBlockEntityMap
  em  <- newEnchantmentMap
  GameState
    <$> newIORef (defaultPlayer spawnPos)
    <*> newIORef emptyInventory
    <*> newIORef MainMenu
    <*> newIORef Nothing
    <*> newIORef (emptyCraftingGrid 3)
    <*> newIORef newFurnaceState
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef newDayNightCycle
    <*> newIORef newWeatherState
    <*> newIORef spawnPos
    <*> newIORef Nothing
    <*> newIORef False
    <*> newIORef Nothing
    <*> newIORef rs
    <*> pure bem
    <*> newIORef ("saves" </> "world1")
    -- Mining & input
    <*> newIORef Nothing
    <*> newIORef False
    <*> newIORef False
    <*> newIORef noInput
    <*> newIORef Nothing
    <*> newIORef (0.0, 0.0)
    -- Combat & effects
    <*> newIORef 0.0
    <*> newIORef 0.0
    <*> newIORef 0.0
    <*> newIORef 0.0
    <*> newIORef []  -- gsActiveEffects
    <*> newIORef []
    <*> newIORef HM.empty
    <*> newIORef IM.empty
    -- Entity AI & spawning
    <*> newIORef HM.empty
    <*> newIORef rng
    <*> newIORef 0.0
    -- Misc gameplay
    <*> newIORef Nothing
    <*> newIORef Nothing
    -- Enchanting
    <*> newIORef Nothing
    <*> newIORef []
    <*> newIORef 30
    <*> pure em
    -- Rendering state
    <*> newIORef 0
    <*> newIORef HM.empty
    <*> newIORef HM.empty
    -- Frame timing
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0.0
    <*> newIORef 0
    <*> newIORef 0.0
    <*> newIORef 0
