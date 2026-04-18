module Entity.Component
  ( EntityComponent(..)
  , ComponentMap
  , newComponentMap
  , addComponent
  , removeComponent
  , getComponents
  , removeEntity
  , getHealth
  , isTamed
  , isMountOccupied
  , getProjectile
  , getPaintingYaw
  , getAI
  ) where

import Data.IORef
import qualified Data.IntMap.Strict as IM
import Entity.Mob (AIState)

-- | Typed entity components to replace string-tag system.
data EntityComponent
  = HealthComp !Float !Float        -- ^ current health, max health
  | AIComp !AIState                 -- ^ AI state machine
  | TamedComp !Bool                 -- ^ is sitting
  | MountableComp !Bool             -- ^ is occupied
  | ProjectileComp !Float !Float    -- ^ damage, age (seconds)
  | PaintingComp !Float             -- ^ wall face yaw (degrees)
  deriving stock (Show, Eq)

-- | Maps entity IDs to their list of components.
type ComponentMap = IM.IntMap [EntityComponent]

-- | Create a new empty component map wrapped in an IORef.
newComponentMap :: IO (IORef ComponentMap)
newComponentMap = newIORef IM.empty

-- | Add a component to an entity. Appends to the entity's component list.
addComponent :: IORef ComponentMap -> Int -> EntityComponent -> IO ()
addComponent ref eid comp =
  modifyIORef' ref (IM.alter go eid)
  where
    go Nothing   = Just [comp]
    go (Just cs) = Just (cs ++ [comp])

-- | Remove the first component matching a predicate from an entity.
removeComponent :: IORef ComponentMap -> Int -> (EntityComponent -> Bool) -> IO ()
removeComponent ref eid predicate =
  modifyIORef' ref (IM.adjust (dropFirst predicate) eid)
  where
    dropFirst _ []     = []
    dropFirst p (x:xs)
      | p x       = xs
      | otherwise  = x : dropFirst p xs

-- | Get all components for an entity. Returns empty list if entity has none.
getComponents :: IORef ComponentMap -> Int -> IO [EntityComponent]
getComponents ref eid = do
  m <- readIORef ref
  pure $ IM.findWithDefault [] eid m

-- | Remove all components for an entity.
removeEntity :: IORef ComponentMap -> Int -> IO ()
removeEntity ref eid =
  modifyIORef' ref (IM.delete eid)

-- | Find the first component matching a projection.
findComponent :: (EntityComponent -> Maybe a) -> [EntityComponent] -> Maybe a
findComponent _ []     = Nothing
findComponent f (c:cs) = case f c of
  Just v  -> Just v
  Nothing -> findComponent f cs

-- | Extract health from a component list, if present.
getHealth :: [EntityComponent] -> Maybe (Float, Float)
getHealth = findComponent $ \case
  HealthComp cur mx -> Just (cur, mx)
  _                 -> Nothing

-- | Check whether the entity is tamed (defaults to False).
isTamed :: [EntityComponent] -> Bool
isTamed cs = case findComponent extract cs of
  Just v  -> v
  Nothing -> False
  where
    extract (TamedComp v) = Just v
    extract _             = Nothing

-- | Check whether a mountable entity is currently occupied (defaults to False).
isMountOccupied :: [EntityComponent] -> Bool
isMountOccupied cs = case findComponent extract cs of
  Just v  -> v
  Nothing -> False
  where
    extract (MountableComp v) = Just v
    extract _                 = Nothing

-- | Extract projectile info (damage, age) if present.
getProjectile :: [EntityComponent] -> Maybe (Float, Float)
getProjectile = findComponent $ \case
  ProjectileComp dmg age -> Just (dmg, age)
  _                      -> Nothing

-- | Extract painting yaw if present.
getPaintingYaw :: [EntityComponent] -> Maybe Float
getPaintingYaw = findComponent $ \case
  PaintingComp yaw -> Just yaw
  _                -> Nothing

-- | Extract AI state if present.
getAI :: [EntityComponent] -> Maybe AIState
getAI = findComponent $ \case
  AIComp st -> Just st
  _         -> Nothing
