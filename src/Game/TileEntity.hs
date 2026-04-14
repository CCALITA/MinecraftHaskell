module Game.TileEntity
  ( TileEntity(..)
  , TileEntityMap
  , FurnaceState(..)
  , FurnaceVariant(..)
  , HopperState(..)
  , HopperDirection(..)
  , SmeltingRecipe(..)
  , RecipeCategory(..)
  , newTileEntityMap
  , getTileEntity
  , setTileEntity
  , removeTileEntity
  , newFurnaceState
  , newHopperState
  , tickTileEntities
  , tickHoppers
  , smeltingRecipes
  , findSmeltingRecipe
  , fuelBurnTime
  , furnaceVariantName
  , faceToHopperDirection
  , hopperPullFrom
  , hopperPushTo
  ) where

import Game.Item
import Game.Inventory (ItemStack(..))
import World.Block (BlockType)
import qualified World.Block as B

import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Linear (V3(..))
import qualified Data.Vector as V
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)

-- | Tile entity variants
data TileEntity
  = TEFurnace  !FurnaceState
  | TEHopper   !HopperState
  deriving stock (Show, Eq, Generic)

type TileEntityMap = IORef (HashMap (V3 Int) TileEntity)

-- | Furnace processing variants
data FurnaceVariant = NormalFurnace | BlastFurnace | SmokerFurnace
  deriving stock (Show, Eq, Enum, Bounded, Generic)

-- | Furnace state
data FurnaceState = FurnaceState
  { fsInput         :: !(Maybe ItemStack)
  , fsFuel          :: !(Maybe ItemStack)
  , fsOutput        :: !(Maybe ItemStack)
  , fsSmeltProgress :: !Float    -- 0.0 to 1.0
  , fsFuelRemaining :: !Float    -- seconds left of current fuel
  , fsFuelTotal     :: !Float    -- total burn time of current fuel piece
  , fsType          :: !FurnaceVariant
  } deriving stock (Show, Eq, Generic)

-- | Direction a hopper outputs items to
data HopperDirection
  = HopperDown
  | HopperNorth
  | HopperSouth
  | HopperEast
  | HopperWest
  deriving stock (Show, Eq, Enum, Bounded, Generic)

-- | Hopper state: 5 inventory slots + output direction
data HopperState = HopperState
  { hsSlots     :: !(V.Vector (Maybe ItemStack))  -- 5 slots
  , hsDirection :: !HopperDirection
  , hsCooldown  :: !Int                           -- ticks until next transfer
  } deriving stock (Show, Eq, Generic)

-- | Recipe categories for furnace variant filtering
data RecipeCategory = CatOre | CatFood | CatMisc
  deriving stock (Show, Eq, Generic)

-- | A smelting recipe
data SmeltingRecipe = SmeltingRecipe
  { srInput     :: !Item
  , srOutput    :: !Item
  , srSmeltTime :: !Float       -- seconds for normal furnace
  , srCategory  :: !RecipeCategory
  } deriving stock (Show, Eq)

-- | All smelting recipes
smeltingRecipes :: [SmeltingRecipe]
smeltingRecipes =
  [ SmeltingRecipe (BlockItem B.IronOre)      (MaterialItem IronIngot)  10.0 CatOre
  , SmeltingRecipe (BlockItem B.GoldOre)      (MaterialItem GoldIngot)  10.0 CatOre
  , SmeltingRecipe (BlockItem B.Sand)         (BlockItem B.Glass)       10.0 CatMisc
  , SmeltingRecipe (BlockItem B.OakLog)       (MaterialItem Charcoal)   10.0 CatMisc
  , SmeltingRecipe (BlockItem B.Clay)         (MaterialItem BrickItem)  10.0 CatMisc
  , SmeltingRecipe (BlockItem B.Cobblestone)  (BlockItem B.Stone)       10.0 CatMisc
  ]

-- | Find a smelting recipe for a given input item and furnace variant
findSmeltingRecipe :: FurnaceVariant -> Item -> Maybe SmeltingRecipe
findSmeltingRecipe variant input = go smeltingRecipes
  where
    categoryFilter = case variant of
      NormalFurnace -> const True
      BlastFurnace  -> (== CatOre)
      SmokerFurnace -> (== CatFood)
    go [] = Nothing
    go (r:rs)
      | srInput r == input && categoryFilter (srCategory r) = Just r
      | otherwise = go rs

-- | Fuel burn time in seconds. Nothing = not a fuel item.
fuelBurnTime :: Item -> Maybe Float
fuelBurnTime = \case
  BlockItem bt -> case bt of
    B.OakPlanks     -> Just 15.0
    B.OakLog        -> Just 15.0
    B.CraftingTable -> Just 15.0
    B.Chest         -> Just 15.0
    B.CoalOre       -> Just 80.0
    _               -> Nothing
  MaterialItem Charcoal -> Just 80.0
  _                     -> Nothing

-- | Create a new empty tile entity map
newTileEntityMap :: IO TileEntityMap
newTileEntityMap = newIORef HM.empty

-- | Get a tile entity at a world position
getTileEntity :: TileEntityMap -> V3 Int -> IO (Maybe TileEntity)
getTileEntity ref pos = HM.lookup pos <$> readIORef ref

-- | Set a tile entity at a world position
setTileEntity :: TileEntityMap -> V3 Int -> TileEntity -> IO ()
setTileEntity ref pos te = modifyIORef' ref (HM.insert pos te)

-- | Remove a tile entity at a world position
removeTileEntity :: TileEntityMap -> V3 Int -> IO ()
removeTileEntity ref pos = modifyIORef' ref (HM.delete pos)

-- | Create a new empty furnace state
newFurnaceState :: FurnaceVariant -> FurnaceState
newFurnaceState variant = FurnaceState
  { fsInput         = Nothing
  , fsFuel          = Nothing
  , fsOutput        = Nothing
  , fsSmeltProgress = 0
  , fsFuelRemaining = 0
  , fsFuelTotal     = 0
  , fsType          = variant
  }

-- | Create a new empty hopper state
newHopperState :: HopperDirection -> HopperState
newHopperState dir = HopperState
  { hsSlots     = V.replicate 5 Nothing
  , hsDirection = dir
  , hsCooldown  = 0
  }

-- | Display name for furnace variant (used in HUD label)
furnaceVariantName :: FurnaceVariant -> String
furnaceVariantName NormalFurnace = "Furnace"
furnaceVariantName BlastFurnace  = "Blast Furnace"
furnaceVariantName SmokerFurnace = "Smoker"

-- | Convert a face normal to hopper output direction
faceToHopperDirection :: V3 Int -> HopperDirection
faceToHopperDirection (V3 0 (-1) 0)  = HopperDown    -- clicked bottom
faceToHopperDirection (V3 0 0 1)     = HopperNorth    -- clicked north face
faceToHopperDirection (V3 0 0 (-1))  = HopperSouth    -- clicked south face
faceToHopperDirection (V3 1 0 0)     = HopperEast     -- clicked east face
faceToHopperDirection (V3 (-1) 0 0)  = HopperWest     -- clicked west face
faceToHopperDirection _              = HopperDown      -- default: down

-- | Tick all furnace tile entities (pure per-furnace, IO for map update)
tickTileEntities :: Float -> TileEntityMap -> IO ()
tickTileEntities dt ref = modifyIORef' ref (HM.map tickOne)
  where
    tickOne (TEFurnace fs) = TEFurnace (tickFurnace dt fs)
    tickOne other          = other

-- | Pure furnace tick: consume fuel, advance smelting, produce output
tickFurnace :: Float -> FurnaceState -> FurnaceState
tickFurnace dt fs
  -- No input or no valid recipe -> just burn remaining fuel
  | isNothing (fsInput fs) = fs { fsFuelRemaining = max 0 (fsFuelRemaining fs - dt) }
  | isNothing recipe = fs { fsFuelRemaining = max 0 (fsFuelRemaining fs - dt) }
  -- Has valid recipe
  | otherwise =
      let hasFuel = fsFuelRemaining fs > 0
          -- Try to consume new fuel if needed
          (fs', gotFuel) = if hasFuel
            then (fs, True)
            else tryConsumeFuel fs
      in if not gotFuel && not hasFuel
         then fs { fsSmeltProgress = 0 }  -- No fuel, reset progress
         else
           let speedMul = case fsType fs' of
                 NormalFurnace -> 1.0
                 BlastFurnace  -> 2.0
                 SmokerFurnace -> 2.0
               smeltTime = maybe 10.0 srSmeltTime recipe
               progressDelta = dt * speedMul / smeltTime
               newProgress = fsSmeltProgress fs' + progressDelta
               newFuelRem = max 0 (fsFuelRemaining fs' - dt)
           in if newProgress >= 1.0
              then -- Smelting complete: produce output
                   completeSmelt fs' { fsFuelRemaining = newFuelRem }
              else fs' { fsSmeltProgress = newProgress
                       , fsFuelRemaining = newFuelRem
                       }
  where
    recipe = fsInput fs >>= findSmeltingRecipe (fsType fs) . isItem

-- | Try to consume one fuel item from the fuel slot
tryConsumeFuel :: FurnaceState -> (FurnaceState, Bool)
tryConsumeFuel fs = case fsFuel fs of
  Nothing -> (fs, False)
  Just (ItemStack fuelItem cnt) ->
    case fuelBurnTime fuelItem of
      Nothing -> (fs, False)
      Just burnTime ->
        let newFuel = if cnt <= 1 then Nothing
                      else Just (ItemStack fuelItem (cnt - 1))
        in (fs { fsFuel = newFuel
               , fsFuelRemaining = burnTime
               , fsFuelTotal = burnTime
               }, True)

-- | Complete a smelt: move result to output, consume 1 input
completeSmelt :: FurnaceState -> FurnaceState
completeSmelt fs = case fsInput fs >>= findSmeltingRecipe (fsType fs) . isItem of
  Nothing -> fs
  Just recipe ->
    let outputItem = srOutput recipe
        -- Try to merge with existing output
        newOutput = case fsOutput fs of
          Nothing -> Just (ItemStack outputItem 1)
          Just (ItemStack existingItem cnt)
            | existingItem == outputItem && cnt < itemStackLimit outputItem ->
                Just (ItemStack outputItem (cnt + 1))
            | otherwise -> fsOutput fs  -- Output full, don't complete
        outputChanged = newOutput /= fsOutput fs || isNothing (fsOutput fs)
    in if not outputChanged && isJust (fsOutput fs)
       then fs  -- Output is full, stall
       else
         -- Consume 1 input
         let newInput = case fsInput fs of
               Just (ItemStack inItem cnt)
                 | cnt <= 1  -> Nothing
                 | otherwise -> Just (ItemStack inItem (cnt - 1))
               Nothing -> Nothing
         in fs { fsInput = newInput
               , fsOutput = newOutput
               , fsSmeltProgress = 0
               }

-- | Tick hoppers: pull from above, push to direction target
tickHoppers :: TileEntityMap -> (V3 Int -> IO BlockType) -> IO ()
tickHoppers ref _getBlock = do
  teMap <- readIORef ref
  let hopperEntries = [(pos, hs) | (pos, TEHopper hs) <- HM.toList teMap]
  newMap <- foldlM' (tickOneHopper ref) teMap hopperEntries
  writeIORef ref newMap

-- | Tick a single hopper
tickOneHopper :: TileEntityMap -> HashMap (V3 Int) TileEntity -> (V3 Int, HopperState) -> IO (HashMap (V3 Int) TileEntity)
tickOneHopper _ref teMap (pos, hs)
  | hsCooldown hs > 0 =
      pure $ HM.insert pos (TEHopper hs { hsCooldown = hsCooldown hs - 1 }) teMap
  | otherwise = do
      -- Pull from container above
      let abovePos = pos + V3 0 1 0
          teMap1 = case HM.lookup abovePos teMap of
            Just srcTE -> case hopperPullFrom hs srcTE of
              Just (hs', srcTE') -> HM.insert pos (TEHopper hs') (HM.insert abovePos srcTE' teMap)
              Nothing -> teMap
            Nothing -> teMap
      -- Get the updated hopper state
      let hs1 = case HM.lookup pos teMap1 of
            Just (TEHopper h) -> h
            _                 -> hs
      -- Push to target container
      let targetPos = hopperTargetPos pos (hsDirection hs1)
          teMap2 = case HM.lookup targetPos teMap1 of
            Just dstTE -> case hopperPushTo hs1 dstTE of
              Just (hs', dstTE') -> HM.insert pos (TEHopper hs' { hsCooldown = 8 }) (HM.insert targetPos dstTE' teMap1)
              Nothing -> HM.insert pos (TEHopper hs1 { hsCooldown = 8 }) teMap1
            Nothing -> HM.insert pos (TEHopper hs1 { hsCooldown = 8 }) teMap1
      pure teMap2

-- | Get the world position a hopper pushes to
hopperTargetPos :: V3 Int -> HopperDirection -> V3 Int
hopperTargetPos pos HopperDown  = pos + V3 0 (-1) 0
hopperTargetPos pos HopperNorth = pos + V3 0 0 1
hopperTargetPos pos HopperSouth = pos + V3 0 0 (-1)
hopperTargetPos pos HopperEast  = pos + V3 1 0 0
hopperTargetPos pos HopperWest  = pos + V3 (-1) 0 0

-- | Try to pull 1 item from a source tile entity into the hopper
hopperPullFrom :: HopperState -> TileEntity -> Maybe (HopperState, TileEntity)
hopperPullFrom hs (TEFurnace fs) =
  -- Pull from furnace output slot
  case fsOutput fs of
    Nothing -> Nothing
    Just (ItemStack item cnt) ->
      case hopperAddItem hs item of
        Nothing -> Nothing
        Just hs' ->
          let newOutput = if cnt <= 1 then Nothing
                          else Just (ItemStack item (cnt - 1))
          in Just (hs', TEFurnace fs { fsOutput = newOutput })
hopperPullFrom hs (TEHopper src) =
  -- Pull from first non-empty slot of source hopper
  case V.findIndex isJust (hsSlots src) of
    Nothing -> Nothing
    Just idx -> case hsSlots src V.! idx of
      Nothing -> Nothing
      Just (ItemStack item cnt) ->
        case hopperAddItem hs item of
          Nothing -> Nothing
          Just hs' ->
            let newSlot = if cnt <= 1 then Nothing
                          else Just (ItemStack item (cnt - 1))
                newSrc = src { hsSlots = hsSlots src V.// [(idx, newSlot)] }
            in Just (hs', TEHopper newSrc)

-- | Try to push 1 item from the hopper into a destination tile entity
hopperPushTo :: HopperState -> TileEntity -> Maybe (HopperState, TileEntity)
hopperPushTo hs (TEFurnace fs) =
  -- Push to furnace: fuel items go to fuel slot, others to input
  case firstHopperItem hs of
    Nothing -> Nothing
    Just (idx, ItemStack item _) ->
      if isJust (fuelBurnTime item)
        then -- Try fuel slot
          case fsFuel fs of
            Nothing ->
              Just (removeHopperItem hs idx, TEFurnace fs { fsFuel = Just (ItemStack item 1) })
            Just (ItemStack fItem cnt)
              | fItem == item && cnt < itemStackLimit item ->
                  Just (removeHopperItem hs idx, TEFurnace fs { fsFuel = Just (ItemStack item (cnt + 1)) })
            _ -> Nothing
        else -- Try input slot
          case fsInput fs of
            Nothing ->
              Just (removeHopperItem hs idx, TEFurnace fs { fsInput = Just (ItemStack item 1) })
            Just (ItemStack iItem cnt)
              | iItem == item && cnt < itemStackLimit item ->
                  Just (removeHopperItem hs idx, TEFurnace fs { fsInput = Just (ItemStack item (cnt + 1)) })
            _ -> Nothing
hopperPushTo hs (TEHopper dst) =
  case firstHopperItem hs of
    Nothing -> Nothing
    Just (idx, ItemStack item _) ->
      case hopperAddItem dst item of
        Nothing -> Nothing
        Just dst' -> Just (removeHopperItem hs idx, TEHopper dst')

-- | Add 1 of an item to the first available hopper slot
hopperAddItem :: HopperState -> Item -> Maybe HopperState
hopperAddItem hs item =
  -- First try to stack with existing matching items
  case V.findIndex (matchesItem item) (hsSlots hs) of
    Just idx -> case hsSlots hs V.! idx of
      Just (ItemStack i cnt) | cnt < itemStackLimit i ->
        Just hs { hsSlots = hsSlots hs V.// [(idx, Just (ItemStack i (cnt + 1)))] }
      _ -> tryEmptySlot
    Nothing -> tryEmptySlot
  where
    matchesItem i (Just (ItemStack si cnt)) = si == i && cnt < itemStackLimit i
    matchesItem _ _ = False
    tryEmptySlot = case V.findIndex isNothing (hsSlots hs) of
      Just idx -> Just hs { hsSlots = hsSlots hs V.// [(idx, Just (ItemStack item 1))] }
      Nothing -> Nothing

-- | Get the first non-empty item from a hopper (index + item stack)
firstHopperItem :: HopperState -> Maybe (Int, ItemStack)
firstHopperItem hs = case V.findIndex isJust (hsSlots hs) of
  Nothing -> Nothing
  Just idx -> case hsSlots hs V.! idx of
    Just is -> Just (idx, is)
    Nothing -> Nothing

-- | Remove 1 item from a hopper slot
removeHopperItem :: HopperState -> Int -> HopperState
removeHopperItem hs idx = case hsSlots hs V.! idx of
  Just (ItemStack item cnt)
    | cnt <= 1  -> hs { hsSlots = hsSlots hs V.// [(idx, Nothing)] }
    | otherwise -> hs { hsSlots = hsSlots hs V.// [(idx, Just (ItemStack item (cnt - 1)))] }
  Nothing -> hs

-- | Strict left fold over a list in IO
foldlM' :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlM' _ acc [] = pure acc
foldlM' f acc (x:xs) = do
  acc' <- f acc x
  acc' `seq` foldlM' f acc' xs
