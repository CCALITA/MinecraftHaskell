module Game.Event
  ( GameEvent(..)
  , EventHandler
  , EventBus
  , newEventBus
  , subscribe
  , emit
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Linear (V3)
import World.Block (BlockType)
import Game.Item (Item)

-- | Typed game events for decoupling systems.
data GameEvent
  = EvBlockBroken !(V3 Int) !BlockType
  | EvBlockPlaced !(V3 Int) !BlockType
  | EvMobKilled !String !Int       -- ^ mob tag, entity id
  | EvItemCrafted !Item !Int       -- ^ item, quantity
  | EvItemSmelted !Item
  | EvPlayerDamaged !Int           -- ^ damage amount
  | EvPlayerDied
  | EvDimensionEntered !String     -- ^ dimension name
  | EvAchievementUnlocked !String  -- ^ achievement id
  deriving stock (Show, Eq)

-- | A callback invoked when a 'GameEvent' is emitted.
type EventHandler = GameEvent -> IO ()

-- | A mutable bus holding registered handlers.
data EventBus = EventBus !(IORef [EventHandler])

-- | Create a fresh event bus with no handlers.
newEventBus :: IO EventBus
newEventBus = EventBus <$> newIORef []

-- | Register a handler. It will be called for every future 'emit'.
subscribe :: EventBus -> EventHandler -> IO ()
subscribe (EventBus ref) handler =
  modifyIORef' ref (++ [handler])

-- | Dispatch an event to all registered handlers in subscription order.
emit :: EventBus -> GameEvent -> IO ()
emit (EventBus ref) ev = do
  handlers <- readIORef ref
  mapM_ ($ ev) handlers
