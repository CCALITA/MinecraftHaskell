module Entity.ECS
  ( EntityId
  , EntityWorld(..)
  , Entity(..)
  , Component(..)
  , newEntityWorld
  , spawnEntity
  , destroyEntity
  , getEntity
  , allEntities
  , livingEntities
  , updateEntity
  , entityCount
  , entitiesInRange
  ) where

import Linear (V3(..), distance)
import Data.IORef
import qualified Data.IntMap.Strict as IM

-- | Unique entity identifier
type EntityId = Int

-- | Component types that entities can have
data Component
  = CPosition !(V3 Float)
  | CVelocity !(V3 Float)
  | CHealth !Float !Float     -- current, max
  | CRotation !Float !Float   -- yaw, pitch (degrees)
  deriving stock (Show, Eq)

-- | An entity with its components
data Entity = Entity
  { entId       :: !EntityId
  , entPosition :: !(V3 Float)
  , entVelocity :: !(V3 Float)
  , entHealth   :: !Float
  , entMaxHealth :: !Float
  , entYaw      :: !Float
  , entPitch    :: !Float
  , entAlive    :: !Bool
  , entTag      :: !String      -- entity type tag (e.g. "zombie", "pig")
  } deriving stock (Show, Eq)

-- | World of entities
data EntityWorld = EntityWorld
  { ewEntities :: !(IORef (IM.IntMap Entity))
  , ewNextId   :: !(IORef EntityId)
  }

-- | Create new empty entity world
newEntityWorld :: IO EntityWorld
newEntityWorld = do
  ents <- newIORef IM.empty
  nextId <- newIORef 1
  pure EntityWorld { ewEntities = ents, ewNextId = nextId }

-- | Spawn a new entity, returns its ID
spawnEntity :: EntityWorld -> V3 Float -> Float -> String -> IO EntityId
spawnEntity ew pos maxHp tag = do
  eid <- readIORef (ewNextId ew)
  writeIORef (ewNextId ew) (eid + 1)
  let entity = Entity
        { entId       = eid
        , entPosition = pos
        , entVelocity = V3 0 0 0
        , entHealth   = maxHp
        , entMaxHealth = maxHp
        , entYaw      = 0
        , entPitch    = 0
        , entAlive    = True
        , entTag      = tag
        }
  modifyIORef' (ewEntities ew) (IM.insert eid entity)
  pure eid

-- | Remove an entity
destroyEntity :: EntityWorld -> EntityId -> IO ()
destroyEntity ew eid =
  modifyIORef' (ewEntities ew) (IM.delete eid)

-- | Get a specific entity
getEntity :: EntityWorld -> EntityId -> IO (Maybe Entity)
getEntity ew eid = do
  ents <- readIORef (ewEntities ew)
  pure $ IM.lookup eid ents

-- | Get all entities
allEntities :: EntityWorld -> IO [Entity]
allEntities ew = IM.elems <$> readIORef (ewEntities ew)

-- | Get all living entities
livingEntities :: EntityWorld -> IO [Entity]
livingEntities ew = filter entAlive <$> allEntities ew

-- | Update an entity by ID
updateEntity :: EntityWorld -> EntityId -> (Entity -> Entity) -> IO ()
updateEntity ew eid f =
  modifyIORef' (ewEntities ew) (IM.adjust f eid)

-- | Count of entities
entityCount :: EntityWorld -> IO Int
entityCount ew = IM.size <$> readIORef (ewEntities ew)

-- | Get all entities within a distance of a point
entitiesInRange :: EntityWorld -> V3 Float -> Float -> IO [Entity]
entitiesInRange ew center radius = do
  ents <- livingEntities ew
  pure $ filter (\e -> distance (entPosition e) center <= radius) ents
