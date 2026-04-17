module Game.Furnace
  ( FurnaceState(..)
  , SmeltRecipe(..)
  , newFurnaceState
  , tickFurnace
  , smeltRecipes
  , findRecipe
  , fuelBurnTime
  , getFurnaceInput
  , setFurnaceInput
  , getFurnaceFuel
  , setFurnaceFuel
  , getFurnaceOutput
  , setFurnaceOutput
  ) where

import Game.Item (Item(..), MaterialType(..), FoodType(..))
import Game.Inventory (ItemStack(..))
import World.Block (BlockType(..))

-- | State of a furnace block
data FurnaceState = FurnaceState
  { fsInput       :: !(Maybe ItemStack)  -- input slot
  , fsFuel        :: !(Maybe ItemStack)  -- fuel slot
  , fsOutput      :: !(Maybe ItemStack)  -- output slot
  , fsSmeltTime   :: !Float              -- current smelting progress (seconds elapsed)
  , fsFuelTime    :: !Float              -- remaining fuel burn time (seconds)
  , fsMaxFuelTime :: !Float              -- max fuel burn time for current fuel (for progress bar)
  } deriving stock (Show)

-- | A smelting recipe
data SmeltRecipe = SmeltRecipe
  { srInput  :: !Item
  , srOutput :: !Item
  , srTime   :: !Float  -- seconds to smelt
  } deriving stock (Show)

-- | All smelting recipes
smeltRecipes :: [SmeltRecipe]
smeltRecipes =
  [ SmeltRecipe (BlockItem IronOre)     (MaterialItem IronIngot) 10.0
  , SmeltRecipe (BlockItem GoldOre)     (MaterialItem GoldIngot) 10.0
  , SmeltRecipe (BlockItem Sand)        (BlockItem Glass)        10.0
  , SmeltRecipe (BlockItem Cobblestone) (BlockItem Stone)        10.0
  , SmeltRecipe (BlockItem CoalOre)     (MaterialItem Coal)      10.0
  , SmeltRecipe (FoodItem RawPorkchop)  (FoodItem CookedPorkchop) 10.0
  , SmeltRecipe (FoodItem RawBeef)      (FoodItem Steak)          10.0
  , SmeltRecipe (FoodItem RawChicken)   (FoodItem CookedChicken)  10.0
  ]

-- | How long a fuel item burns (in seconds). Returns 0 for non-fuel items.
fuelBurnTime :: Item -> Float
fuelBurnTime (MaterialItem Coal)  = 80.0
fuelBurnTime (BlockItem OakPlanks) = 15.0
fuelBurnTime (BlockItem OakLog)    = 15.0
fuelBurnTime _                     = 0.0

-- | Create a fresh empty furnace state
newFurnaceState :: FurnaceState
newFurnaceState = FurnaceState
  { fsInput       = Nothing
  , fsFuel        = Nothing
  , fsOutput      = Nothing
  , fsSmeltTime   = 0
  , fsFuelTime    = 0
  , fsMaxFuelTime = 0
  }

-- | Find a recipe matching the given input item
findRecipe :: Item -> Maybe SmeltRecipe
findRecipe item = go smeltRecipes
  where
    go [] = Nothing
    go (r:rs)
      | srInput r == item = Just r
      | otherwise         = go rs

-- | Check if the output slot can accept the recipe output
canAcceptOutput :: Maybe ItemStack -> Item -> Bool
canAcceptOutput Nothing _ = True
canAcceptOutput (Just (ItemStack existing count)) newItem =
  existing == newItem && count < 64

-- | Tick the furnace by dt seconds. Pure function returning updated state.
tickFurnace :: Float -> FurnaceState -> FurnaceState
tickFurnace dt fs =
  let -- Step 1: Try to consume fuel if needed and we have a valid recipe
      fs1 = tryConsumeFuel fs
      -- Step 2: Advance smelting if fuel is burning and input matches recipe
      fs2 = advanceSmelt dt fs1
  in fs2

-- | If fuel time is zero and we have fuel + a valid smeltable input, consume one fuel item
tryConsumeFuel :: FurnaceState -> FurnaceState
tryConsumeFuel fs
  | fsFuelTime fs > 0 = fs  -- still burning
  | otherwise = case (fsInput fs, fsFuel fs) of
      (Just (ItemStack inputItem _), Just (ItemStack fuelItem fuelCount)) ->
        let burnTime = fuelBurnTime fuelItem
        in if burnTime > 0 && hasRecipe inputItem && canAcceptOutput (fsOutput fs) (recipeOutput inputItem)
           then
             let newFuelStack = if fuelCount <= 1
                                then Nothing
                                else Just (ItemStack fuelItem (fuelCount - 1))
             in fs { fsFuel       = newFuelStack
                   , fsFuelTime   = burnTime
                   , fsMaxFuelTime = burnTime
                   }
           else fs
      _ -> fs
  where
    hasRecipe item = case findRecipe item of
      Just _  -> True
      Nothing -> False
    recipeOutput item = case findRecipe item of
      Just r  -> srOutput r
      Nothing -> item  -- should not happen since we check hasRecipe

-- | Advance smelting progress if fuel is burning and input has a valid recipe
advanceSmelt :: Float -> FurnaceState -> FurnaceState
advanceSmelt dt fs
  | fsFuelTime fs <= 0 = fs { fsSmeltTime = 0 }  -- no fuel, reset progress
  | otherwise =
      let remainingFuel = max 0 (fsFuelTime fs - dt)
      in case fsInput fs of
           Just (ItemStack inputItem inputCount) ->
             case findRecipe inputItem of
               Just recipe
                 | canAcceptOutput (fsOutput fs) (srOutput recipe) ->
                     let newProgress = fsSmeltTime fs + dt
                     in if newProgress >= srTime recipe
                        then
                          -- Smelting complete: produce output, consume one input
                          let newInputStack = if inputCount <= 1
                                              then Nothing
                                              else Just (ItemStack inputItem (inputCount - 1))
                              newOutputStack = case fsOutput fs of
                                Nothing -> Just (ItemStack (srOutput recipe) 1)
                                Just (ItemStack outItem outCount) ->
                                  Just (ItemStack outItem (outCount + 1))
                          in fs { fsInput     = newInputStack
                                , fsOutput    = newOutputStack
                                , fsSmeltTime = 0
                                , fsFuelTime  = remainingFuel
                                }
                        else fs { fsSmeltTime = newProgress
                                , fsFuelTime  = remainingFuel
                                }
               _ -> fs { fsSmeltTime = 0, fsFuelTime = remainingFuel }
           Nothing -> fs { fsSmeltTime = 0, fsFuelTime = remainingFuel }

-- | Accessors
getFurnaceInput :: FurnaceState -> Maybe ItemStack
getFurnaceInput = fsInput

setFurnaceInput :: FurnaceState -> Maybe ItemStack -> FurnaceState
setFurnaceInput fs stack = fs { fsInput = stack }

getFurnaceFuel :: FurnaceState -> Maybe ItemStack
getFurnaceFuel = fsFuel

setFurnaceFuel :: FurnaceState -> Maybe ItemStack -> FurnaceState
setFurnaceFuel fs stack = fs { fsFuel = stack }

getFurnaceOutput :: FurnaceState -> Maybe ItemStack
getFurnaceOutput = fsOutput

setFurnaceOutput :: FurnaceState -> Maybe ItemStack -> FurnaceState
setFurnaceOutput fs stack = fs { fsOutput = stack }
