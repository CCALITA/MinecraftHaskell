module Game.DroppedItem
  ( DroppedItem(..)
  , DroppedItems
  , newDroppedItems
  , spawnDrop
  , updateDroppedItems
  , collectNearby
  ) where

import Game.Item (Item(..))
import Linear (V3(..), distance)
import Data.IORef

-- | A dropped item in the world
data DroppedItem = DroppedItem
  { diItem     :: !Item
  , diCount    :: !Int
  , diPos      :: !(V3 Float)
  , diVelocity :: !(V3 Float)
  , diAge      :: !Float       -- seconds since spawn (despawn after 300s)
  } deriving stock (Show)

-- | Collection of dropped items
type DroppedItems = IORef [DroppedItem]

-- | Create empty dropped items list
newDroppedItems :: IO DroppedItems
newDroppedItems = newIORef []

-- | Spawn a dropped item at a position with a small upward bounce
spawnDrop :: DroppedItems -> Item -> Int -> V3 Float -> IO ()
spawnDrop ref item count pos = do
  let drop' = DroppedItem
        { diItem     = item
        , diCount    = count
        , diPos      = pos + V3 0.5 0.5 0.5  -- center of block
        , diVelocity = V3 0 3.0 0            -- small upward bounce
        , diAge      = 0
        }
  modifyIORef' ref (drop' :)

-- | Update dropped items: apply gravity, advance age, remove expired
updateDroppedItems :: Float -> DroppedItems -> IO ()
updateDroppedItems dt ref = modifyIORef' ref (map update . filter alive)
  where
    alive di = diAge di < 300  -- 5 minute despawn
    update di =
      let V3 vx vy vz = diVelocity di
          vy' = vy - 20.0 * dt  -- gravity
          V3 px py pz = diPos di
          py' = max 0 (py + vy' * dt)  -- simple floor at y=0
          vy'' = if py' <= 0 then 0 else vy'
      in di { diPos = V3 (px + vx * dt) py' (pz + vz * dt)
            , diVelocity = V3 (vx * 0.95) vy'' (vz * 0.95)  -- friction
            , diAge = diAge di + dt
            }

-- | Collect items near the player. Returns items to add to inventory.
collectNearby :: DroppedItems -> V3 Float -> Float -> IO [(Item, Int)]
collectNearby ref playerPos pickupRadius = do
  items <- readIORef ref
  let (near, far) = partition' (\di -> distance (diPos di) playerPos < pickupRadius) items
  writeIORef ref far
  pure [(diItem di, diCount di) | di <- near]

-- | Partition a list (avoid Data.List import)
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p (x:xs) =
  let (yes, no) = partition' p xs
  in if p x then (x:yes, no) else (yes, x:no)
