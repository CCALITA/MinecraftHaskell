module World.Fluid
  ( FluidState(..)
  , FluidType(..)
  , FluidBlock(..)
  , newFluidState
  , addFluidSource
  , removeFluid
  , getFluid
  , tickFluids
  , maxFluidLevel
  ) where

import World.Block (BlockType(..))
import World.World (World, worldGetBlock, worldSetBlock)
import Linear (V3(..))
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- | Maximum fluid level (source = 7, flows 7 → 0)
maxFluidLevel :: Int
maxFluidLevel = 7

-- | Fluid types
data FluidType = FluidWater | FluidLava
  deriving stock (Show, Eq, Ord)

-- | A fluid block
data FluidBlock = FluidBlock
  { fbType   :: !FluidType
  , fbLevel  :: !Int       -- 7 = source, 6..0 = flowing
  , fbSource :: !Bool      -- is this a source block?
  } deriving stock (Show, Eq)

-- | Fluid simulation state
data FluidState = FluidState
  { fsFluids   :: !(IORef (Map.Map (V3 Int) FluidBlock))
  , fsDirty    :: !(IORef (Seq (V3 Int)))   -- positions needing update
  }

-- | Create new fluid state
newFluidState :: IO FluidState
newFluidState = do
  fluids <- newIORef Map.empty
  dirty  <- newIORef Seq.empty
  pure FluidState { fsFluids = fluids, fsDirty = dirty }

-- | Place a fluid source block
addFluidSource :: FluidState -> World -> V3 Int -> FluidType -> IO ()
addFluidSource fs world pos ft = do
  let fb = FluidBlock ft maxFluidLevel True
      bt = fluidToBlock ft
  modifyIORef' (fsFluids fs) (Map.insert pos fb)
  worldSetBlock world pos bt
  markDirty fs pos

-- | Remove fluid at a position
removeFluid :: FluidState -> World -> V3 Int -> IO ()
removeFluid fs world pos = do
  modifyIORef' (fsFluids fs) (Map.delete pos)
  worldSetBlock world pos Air
  -- Mark neighbors dirty so they can recalculate
  mapM_ (markDirty fs) (fluidNeighbors pos)

-- | Get fluid at a position
getFluid :: FluidState -> V3 Int -> IO (Maybe FluidBlock)
getFluid fs pos = do
  fluids <- readIORef (fsFluids fs)
  pure $ Map.lookup pos fluids

-- | Mark a position as needing fluid update
markDirty :: FluidState -> V3 Int -> IO ()
markDirty fs pos = modifyIORef' (fsDirty fs) (|> pos)

-- | Process one tick of fluid simulation.
--   Fluids spread horizontally and downward.
--   Water: spread rate 1/tick, Lava: spread rate 1/3 ticks
tickFluids :: FluidState -> World -> IO ()
tickFluids fs world = do
  dirtyQueue <- readIORef (fsDirty fs)
  writeIORef (fsDirty fs) Seq.empty
  processDirty fs world dirtyQueue

processDirty :: FluidState -> World -> Seq (V3 Int) -> IO ()
processDirty fs world queue = case Seq.viewl queue of
  Seq.EmptyL -> pure ()
  pos Seq.:< rest -> do
    mFluid <- getFluid fs pos
    case mFluid of
      Nothing -> processDirty fs world rest
      Just fb -> do
        -- Try to flow down first
        let V3 x y z = pos
            below = V3 x (y - 1) z
        belowBlock <- worldGetBlock world below
        belowFluid <- getFluid fs below

        case belowFluid of
          Nothing | canFlowInto belowBlock -> do
            -- Flow downward (full level)
            let newFb = FluidBlock (fbType fb) maxFluidLevel False
            modifyIORef' (fsFluids fs) (Map.insert below newFb)
            worldSetBlock world below (fluidToBlock (fbType fb))
            markDirty fs below
          _ -> pure ()

        -- Horizontal spread (only if level > 0)
        if fbLevel fb > 0
          then do
            let newLevel = fbLevel fb - 1
                horizontalNbrs = [ V3 (x+1) y z, V3 (x-1) y z
                                 , V3 x y (z+1), V3 x y (z-1) ]
            mapM_ (tryFlowTo fs world (fbType fb) newLevel) horizontalNbrs
          else pure ()

        -- Check for lava + water interaction
        when (fbType fb == FluidLava) $
          checkLavaWaterInteraction fs world pos
        when (fbType fb == FluidWater) $
          checkWaterLavaInteraction fs world pos

        processDirty fs world rest

-- | Try to flow fluid into a neighboring position
tryFlowTo :: FluidState -> World -> FluidType -> Int -> V3 Int -> IO ()
tryFlowTo fs world ft level pos = do
  targetBlock <- worldGetBlock world pos
  existing <- getFluid fs pos

  case existing of
    Just fb | fbLevel fb >= level -> pure ()  -- already has equal or higher level
    Just fb | fbSource fb -> pure ()          -- don't overwrite sources
    _ | not (canFlowInto targetBlock) -> pure ()  -- can't flow into solid blocks
    _ -> do
      -- Check for fluid interaction
      case (ft, existing) of
        (FluidWater, Just (FluidBlock FluidLava _ True)) -> do
          -- Water meets lava source → obsidian (placeholder: use Stone)
          modifyIORef' (fsFluids fs) (Map.delete pos)
          worldSetBlock world pos Stone
        (FluidWater, Just (FluidBlock FluidLava _ False)) -> do
          -- Water meets flowing lava → cobblestone
          modifyIORef' (fsFluids fs) (Map.delete pos)
          worldSetBlock world pos Cobblestone
        (FluidLava, Just (FluidBlock FluidWater _ _)) -> do
          -- Lava meets water → cobblestone
          modifyIORef' (fsFluids fs) (Map.delete pos)
          worldSetBlock world pos Cobblestone
        _ -> do
          -- Normal flow
          let newFb = FluidBlock ft level False
          modifyIORef' (fsFluids fs) (Map.insert pos newFb)
          worldSetBlock world pos (fluidToBlock ft)
          when (level > 0) $ markDirty fs pos

-- | Check if a block type can be replaced by fluid
canFlowInto :: BlockType -> Bool
canFlowInto Air   = True
canFlowInto Water = True  -- water can flow into water
canFlowInto Lava  = True  -- lava can flow into lava
canFlowInto _     = False

-- | Convert fluid type to block type
fluidToBlock :: FluidType -> BlockType
fluidToBlock FluidWater = Water
fluidToBlock FluidLava  = Lava

-- | Check for lava next to water (creates cobblestone/obsidian)
checkLavaWaterInteraction :: FluidState -> World -> V3 Int -> IO ()
checkLavaWaterInteraction fs world pos = do
  let nbrs = fluidNeighbors pos
  mapM_ (\nbrPos -> do
    mFluid <- getFluid fs nbrPos
    case mFluid of
      Just (FluidBlock FluidWater _ _) -> do
        -- Lava source next to water → obsidian at lava pos (use Stone)
        modifyIORef' (fsFluids fs) (Map.delete pos)
        worldSetBlock world pos Stone
      _ -> pure ()
    ) nbrs

-- | Check for water next to lava
checkWaterLavaInteraction :: FluidState -> World -> V3 Int -> IO ()
checkWaterLavaInteraction fs world pos = do
  let nbrs = fluidNeighbors pos
  mapM_ (\nbrPos -> do
    mFluid <- getFluid fs nbrPos
    case mFluid of
      Just (FluidBlock FluidLava _ True) -> do
        -- Water next to lava source → obsidian at lava pos
        modifyIORef' (fsFluids fs) (Map.delete nbrPos)
        worldSetBlock world nbrPos Stone
      Just (FluidBlock FluidLava _ False) -> do
        -- Water next to flowing lava → cobblestone at lava pos
        modifyIORef' (fsFluids fs) (Map.delete nbrPos)
        worldSetBlock world nbrPos Cobblestone
      _ -> pure ()
    ) nbrs

-- | 6-connected neighbors
fluidNeighbors :: V3 Int -> [V3 Int]
fluidNeighbors (V3 x y z) =
  [ V3 (x+1) y z, V3 (x-1) y z
  , V3 x (y+1) z, V3 x (y-1) z
  , V3 x y (z+1), V3 x y (z-1)
  ]

-- | Control.Monad.when equivalent
when :: Bool -> IO () -> IO ()
when True action = action
when False _     = pure ()
