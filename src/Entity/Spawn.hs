module Entity.Spawn
  ( SpawnRules(..)
  , defaultSpawnRules
  , trySpawnMobs
  , canSpawnAt
  ) where

import Entity.ECS
import Entity.Mob (MobType(..), mobInfo, MobInfo(..), isHostile, isPassive)
import Game.DayNight (DayNightCycle, isNight)
import Game.Physics (BlockQuery)
import World.Weather (WeatherState, isRaining)
import Linear (V3(..), distance)
import System.Random (StdGen, randomR, randomRIO)
import Data.IORef

-- | Spawn rules configuration
data SpawnRules = SpawnRules
  { srMaxHostile  :: !Int     -- max hostile mobs in loaded area
  , srMaxPassive  :: !Int     -- max passive mobs in loaded area
  , srSpawnRadius :: !Float   -- min distance from player to spawn
  , srDespawnDist :: !Float   -- distance at which mobs despawn
  , srSpawnCooldown :: !Float -- seconds between spawn attempts
  } deriving stock (Show, Eq)

defaultSpawnRules :: SpawnRules
defaultSpawnRules = SpawnRules
  { srMaxHostile  = 20
  , srMaxPassive  = 10
  , srSpawnRadius = 24.0
  , srDespawnDist = 128.0
  , srSpawnCooldown = 2.0
  }

-- | Attempt to spawn mobs near the player.
--   Returns list of spawned entity IDs.
trySpawnMobs
  :: SpawnRules
  -> EntityWorld
  -> DayNightCycle
  -> WeatherState
  -> BlockQuery
  -> V3 Float         -- player position
  -> IORef StdGen     -- RNG
  -> IO [EntityId]
trySpawnMobs rules ew dayNight weather isSolid playerPos rngRef = do
  ents <- livingEntities ew
  let hostileCount = length $ filter (\e -> entTag e `elem` hostileTags) ents
      passiveCount = length $ filter (\e -> entTag e `elem` passiveTags) ents

  -- Despawn distant mobs first
  mapM_ (\e ->
    if distance (entPosition e) playerPos > srDespawnDist rules
      then destroyEntity ew (entId e)
      else pure ()
    ) ents

  spawned <- newIORef ([] :: [EntityId])

  -- Try to spawn hostile mobs at night or when raining
  if (isNight dayNight || isRaining weather) && hostileCount < srMaxHostile rules
    then do
      mPos <- findSpawnPosition rngRef isSolid playerPos (srSpawnRadius rules)
      case mPos of
        Nothing -> pure ()
        Just pos -> do
          mobType <- randomHostileMob rngRef
          let info = mobInfo mobType
          eid <- spawnEntity ew pos (miMaxHealth info) (show mobType)
          modifyIORef' spawned (eid :)
    else pure ()

  -- Try to spawn passive mobs (day or night, on surface)
  if passiveCount < srMaxPassive rules
    then do
      mPos <- findSpawnPosition rngRef isSolid playerPos (srSpawnRadius rules * 0.7)
      case mPos of
        Nothing -> pure ()
        Just pos -> do
          mobType <- randomPassiveMob rngRef
          let info = mobInfo mobType
          eid <- spawnEntity ew pos (miMaxHealth info) (show mobType)
          modifyIORef' spawned (eid :)
    else pure ()

  readIORef spawned

-- | Find a valid spawn position near the player
findSpawnPosition :: IORef StdGen -> BlockQuery -> V3 Float -> Float -> IO (Maybe (V3 Float))
findSpawnPosition rngRef isSolid (V3 px py pz) radius = go 10  -- 10 attempts
  where
    go 0 = pure Nothing
    go n = do
      gen <- readIORef rngRef
      let (angle, gen1) = randomR (0, 2 * pi :: Float) gen
          (dist, gen2)  = randomR (radius * 0.5, radius :: Float) gen1
          testX = px + cos angle * dist
          testZ = pz + sin angle * dist
      writeIORef rngRef gen2

      -- Find surface: scan down from player height + 10
      let scanY = floor py + 10 :: Int
      mSurface <- findSurface isSolid (floor testX) scanY (floor testZ)
      case mSurface of
        Nothing -> go (n - 1)
        Just surfaceY -> do
          let spawnPos = V3 testX (fromIntegral surfaceY + 0.1) testZ
          canSpawn <- canSpawnAt isSolid spawnPos
          if canSpawn
            then pure (Just spawnPos)
            else go (n - 1)

-- | Scan downward to find surface (solid block with air above)
findSurface :: BlockQuery -> Int -> Int -> Int -> IO (Maybe Int)
findSurface isSolid x y z
  | y < 1 = pure Nothing
  | otherwise = do
      solid <- isSolid x y z
      aboveOpen <- not <$> isSolid x (y + 1) z
      above2Open <- not <$> isSolid x (y + 2) z
      if solid && aboveOpen && above2Open
        then pure $ Just (y + 1)  -- spawn on top of solid block
        else findSurface isSolid x (y - 1) z

-- | Check if a position is valid for spawning (not inside blocks, has ground)
canSpawnAt :: BlockQuery -> V3 Float -> IO Bool
canSpawnAt isSolid (V3 x y z) = do
  let bx = floor x :: Int
      by = floor y :: Int
      bz = floor z :: Int
  feetClear <- not <$> isSolid bx by bz
  headClear <- not <$> isSolid bx (by + 1) bz
  hasGround <- isSolid bx (by - 1) bz
  pure $ feetClear && headClear && hasGround

-- | Pick a random hostile mob type
randomHostileMob :: IORef StdGen -> IO MobType
randomHostileMob rngRef = do
  gen <- readIORef rngRef
  let hostiles = [Zombie, Skeleton, Creeper, Spider]
      (idx, gen') = randomR (0, length hostiles - 1) gen
  writeIORef rngRef gen'
  pure $ hostiles !! idx

-- | Pick a random passive mob type
randomPassiveMob :: IORef StdGen -> IO MobType
randomPassiveMob rngRef = do
  gen <- readIORef rngRef
  let passives = [Pig, Cow, Sheep, Chicken]
      (idx, gen') = randomR (0, length passives - 1) gen
  writeIORef rngRef gen'
  pure $ passives !! idx

-- | Tag lists for counting
hostileTags :: [String]
hostileTags = ["Zombie", "Skeleton", "Creeper", "Spider"]

passiveTags :: [String]
passiveTags = ["Pig", "Cow", "Sheep", "Chicken"]
