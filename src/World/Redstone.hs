module World.Redstone
  ( RedstoneState(..)
  , PowerLevel
  , RedstoneComponent(..)
  , newRedstoneState
  , setPower
  , getPower
  , propagateRedstone
  , updateRedstone
  , isRedstoneConductor
  ) where

import World.Block (BlockType(..))
import World.Chunk (Chunk(..), ChunkPos, chunkWidth, chunkDepth, chunkHeight, getBlock, blockIndex)
import Linear (V3(..))
import Data.Word (Word8)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- | Power level 0-15
type PowerLevel = Word8

-- | Redstone component types
data RedstoneComponent
  = RSWire                        -- redstone wire: propagates signal with decay
  | RSTorch                       -- redstone torch: inverts signal, emits 15 when unpowered
  | RSRepeater !Int               -- repeater with delay (1-4 ticks)
  | RSLever !Bool                 -- lever: on/off
  | RSButton !Float               -- button: on for N seconds
  | RSPressurePlate               -- pressure plate: on when entity standing
  deriving stock (Show, Eq)

-- | Per-block redstone power state.
--   Key is world-space (V3 Int) position.
data RedstoneState = RedstoneState
  { rsPowerMap    :: !(IORef (Map.Map (V3 Int) PowerLevel))
  , rsComponents  :: !(IORef (Map.Map (V3 Int) RedstoneComponent))
  }

-- | Create new empty redstone state
newRedstoneState :: IO RedstoneState
newRedstoneState = do
  pm <- newIORef Map.empty
  cm <- newIORef Map.empty
  pure RedstoneState { rsPowerMap = pm, rsComponents = cm }

-- | Set power level at a position
setPower :: RedstoneState -> V3 Int -> PowerLevel -> IO ()
setPower rs pos level =
  modifyIORef' (rsPowerMap rs) $ \m ->
    if level == 0
      then Map.delete pos m
      else Map.insert pos level m

-- | Get power level at a position
getPower :: RedstoneState -> V3 Int -> IO PowerLevel
getPower rs pos = do
  m <- readIORef (rsPowerMap rs)
  pure $ Map.findWithDefault 0 pos m

-- | Check if a block type conducts redstone signals
isRedstoneConductor :: BlockType -> Bool
isRedstoneConductor = \case
  Stone       -> True
  Cobblestone -> True
  Dirt        -> True
  Grass       -> True
  Sand        -> True
  Gravel      -> True
  OakPlanks   -> True
  StoneBrick  -> True
  Brick       -> True
  _           -> False

-- | Full redstone update: propagate from all sources.
--   Called each game tick.
updateRedstone :: RedstoneState -> IO ()
updateRedstone rs = do
  comps <- readIORef (rsComponents rs)
  -- Clear all power
  writeIORef (rsPowerMap rs) Map.empty

  -- Phase 1: find all power sources and seed initial power
  let sources = Map.foldlWithKey' collectSources Seq.empty comps
  -- Phase 2: BFS propagation
  propagateFromQueue rs sources

  where
    collectSources queue pos comp = case comp of
      RSTorch     -> queue |> (pos, 15 :: PowerLevel)
      RSLever on  -> if on then queue |> (pos, 15) else queue
      RSButton t  -> if t > 0 then queue |> (pos, 15) else queue
      RSRepeater _ -> queue  -- repeaters get power from input side
      RSPressurePlate -> queue  -- activated externally
      RSWire      -> queue     -- wires don't generate power
      _           -> queue

-- | Propagate redstone signal from given sources via BFS
propagateRedstone :: RedstoneState -> [(V3 Int, PowerLevel)] -> IO ()
propagateRedstone rs sources = do
  let queue = foldl (\q (pos, lvl) -> q |> (pos, lvl)) Seq.empty sources
  propagateFromQueue rs queue

propagateFromQueue :: RedstoneState -> Seq (V3 Int, PowerLevel) -> IO ()
propagateFromQueue rs queue = case Seq.viewl queue of
  Seq.EmptyL -> pure ()
  (pos, level) Seq.:< rest -> do
    currentPower <- getPower rs pos
    if level <= currentPower
      then propagateFromQueue rs rest  -- already has higher power
      else do
        setPower rs pos level
        -- Spread to neighbors with decay
        if level <= 1
          then propagateFromQueue rs rest
          else do
            comps <- readIORef (rsComponents rs)
            let nbrs = redstoneNeighbors pos
                newLevel = level - 1
                addNeighbor q nbrPos = case Map.lookup nbrPos comps of
                  Just RSWire -> q |> (nbrPos, newLevel)
                  _           -> q  -- only wires propagate with decay
                rest' = foldl addNeighbor rest nbrs
            propagateFromQueue rs rest'

-- | 6-connected neighbors for redstone
redstoneNeighbors :: V3 Int -> [V3 Int]
redstoneNeighbors (V3 x y z) =
  [ V3 (x+1) y z, V3 (x-1) y z
  , V3 x (y+1) z, V3 x (y-1) z
  , V3 x y (z+1), V3 x y (z-1)
  ]
