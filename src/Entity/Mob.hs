module Entity.Mob
  ( MobType(..)
  , MobBehavior(..)
  , AIState(..)
  , MobInfo(..)
  , mobInfo
  , updateMobAI
  , damageEntity
  , isHostile
  , isPassive
  ) where

import Entity.ECS
import Entity.Pathfinding (findPath, PathNode(..))
import Game.Physics (BlockQuery)
import Linear (V3(..), normalize, distance, (^*), norm)
import System.Random (StdGen, randomR, mkStdGen)
import Data.IORef

-- | Types of mobs
data MobType
  = Zombie
  | Skeleton
  | Creeper
  | Spider
  | Pig
  | Cow
  | Sheep
  | Chicken
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | AI state machine
data AIState
  = AIIdle !Float           -- time remaining in idle
  | AIWander !(V3 Float)    -- target position to wander to
  | AIChase !EntityId       -- chasing a target (player entity)
  | AIAttack !EntityId !Float  -- attacking target, cooldown
  | AIFlee !(V3 Float) !Float  -- fleeing from point, time remaining
  deriving stock (Show, Eq)

-- | Behavior category
data MobBehavior = Hostile | Passive | Neutral
  deriving stock (Show, Eq)

-- | Mob properties
data MobInfo = MobInfo
  { miType       :: !MobType
  , miBehavior   :: !MobBehavior
  , miMaxHealth  :: !Float
  , miSpeed      :: !Float
  , miAttackDmg  :: !Float
  , miDetectRange :: !Float   -- range to detect player
  , miAttackRange :: !Float   -- range to attack
  } deriving stock (Show, Eq)

-- | Get mob info for a mob type
mobInfo :: MobType -> MobInfo
mobInfo = \case
  Zombie   -> MobInfo Zombie   Hostile 20 4.0  3.0 16.0 1.5
  Skeleton -> MobInfo Skeleton Hostile 20 4.0  2.0 16.0 15.0  -- ranged
  Creeper  -> MobInfo Creeper  Hostile 20 3.0  49.0 16.0 3.0  -- explodes
  Spider   -> MobInfo Spider   Neutral 16 5.0  2.0 16.0 1.5
  Pig      -> MobInfo Pig      Passive 10 2.5  0.0 0.0  0.0
  Cow      -> MobInfo Cow      Passive 10 2.5  0.0 0.0  0.0
  Sheep    -> MobInfo Sheep    Passive 8  2.5  0.0 0.0  0.0
  Chicken  -> MobInfo Chicken  Passive 4  2.0  0.0 0.0  0.0

isHostile :: MobType -> Bool
isHostile mt = miBehavior (mobInfo mt) == Hostile

isPassive :: MobType -> Bool
isPassive mt = miBehavior (mobInfo mt) == Passive

-- | Update mob AI for one tick.
--   Takes: dt, mob entity, player position, block query, AI state ref, RNG ref
--   Returns updated entity and AI state.
updateMobAI
  :: Float -> Entity -> MobType -> V3 Float -> BlockQuery
  -> AIState -> IORef StdGen
  -> IO (Entity, AIState)
updateMobAI dt entity mobType playerPos _blockQuery aiState rngRef = do
  let info = mobInfo mobType
      pos = entPosition entity
      distToPlayer = distance pos playerPos

  case aiState of
    AIIdle timeLeft
      | timeLeft <= 0 -> do
          -- Transition: idle → wander or chase
          if isHostile mobType && distToPlayer < miDetectRange info
            then pure (entity, AIChase 0)  -- 0 = player
            else do
              -- Pick random wander target
              target <- randomWanderTarget rngRef pos
              pure (entity, AIWander target)
      | otherwise ->
          pure (entity, AIIdle (timeLeft - dt))

    AIWander target ->
      let dist = distance pos target
      in if dist < 1.0
        then do
          -- Reached target, go idle
          idleTime <- randomIdleTime rngRef
          pure (entity, AIIdle idleTime)
        else do
          -- Move toward target
          let dir = normalize (target - pos)
              speed = miSpeed info
              newVel = dir ^* speed
              newPos = pos + newVel ^* dt
              entity' = entity { entPosition = newPos, entVelocity = newVel }
          -- Check if should chase player
          if isHostile mobType && distToPlayer < miDetectRange info
            then pure (entity', AIChase 0)
            else pure (entity', AIWander target)

    AIChase _targetId ->
      if distToPlayer > miDetectRange info * 1.5
        then do
          -- Lost target, go idle
          idleTime <- randomIdleTime rngRef
          pure (entity, AIIdle idleTime)
        else if distToPlayer < miAttackRange info
          then pure (entity, AIAttack 0 0)  -- start attacking
          else do
            -- Move toward player
            let dir = if distToPlayer > 0.1
                      then normalize (playerPos - pos)
                      else V3 0 0 0
                speed = miSpeed info
                newVel = dir ^* speed
                newPos = pos + newVel ^* dt
                -- Face the player
                yaw = atan2 (v3x dir) (v3z dir) * 180 / pi
                entity' = entity
                  { entPosition = newPos
                  , entVelocity = newVel
                  , entYaw = yaw
                  }
            pure (entity', AIChase 0)

    AIAttack _targetId cooldown
      | cooldown > 0 ->
          pure (entity, AIAttack 0 (cooldown - dt))
      | distToPlayer > miAttackRange info * 1.5 ->
          pure (entity, AIChase 0)
      | otherwise -> do
          -- Attack! (damage applied externally)
          pure (entity, AIAttack 0 1.0)  -- 1 second cooldown

    AIFlee fleeFrom timeLeft
      | timeLeft <= 0 -> do
          idleTime <- randomIdleTime rngRef
          pure (entity, AIIdle idleTime)
      | otherwise -> do
          let dir = if distance pos fleeFrom > 0.1
                    then normalize (pos - fleeFrom)
                    else V3 1 0 0
              speed = miSpeed info * 1.5  -- flee faster
              newVel = dir ^* speed
              newPos = pos + newVel ^* dt
              entity' = entity { entPosition = newPos, entVelocity = newVel }
          pure (entity', AIFlee fleeFrom (timeLeft - dt))

-- | Apply damage to an entity
damageEntity :: Entity -> Float -> Entity
damageEntity entity dmg =
  let newHp = max 0 (entHealth entity - dmg)
  in entity { entHealth = newHp, entAlive = newHp > 0 }

-- | Random wander target within 8 blocks
randomWanderTarget :: IORef StdGen -> V3 Float -> IO (V3 Float)
randomWanderTarget rngRef (V3 x y z) = do
  gen <- readIORef rngRef
  let (dx, gen1) = randomR (-8.0, 8.0 :: Float) gen
      (dz, gen2) = randomR (-8.0, 8.0 :: Float) gen1
  writeIORef rngRef gen2
  pure $ V3 (x + dx) y (z + dz)

-- | Random idle time between 2 and 6 seconds
randomIdleTime :: IORef StdGen -> IO Float
randomIdleTime rngRef = do
  gen <- readIORef rngRef
  let (t, gen') = randomR (2.0, 6.0 :: Float) gen
  writeIORef rngRef gen'
  pure t

-- | V3 component helpers
v3x, v3z :: V3 Float -> Float
v3x (V3 x _ _) = x
v3z (V3 _ _ z) = z
