module Game.Enchanting
  ( Enchantment(..)
  , EnchantmentType(..)
  , EnchantmentMap
  , newEnchantmentMap
  , getEnchantments
  , setEnchantments
  , clearEnchantments
  , generateEnchantments
  , applyEnchantment
  , enchantmentName
  , enchantmentMaxLevel
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map

-- | Types of enchantments available
data EnchantmentType
  = Sharpness
  | Protection
  | Efficiency
  | Unbreaking
  | Power
  | Knockback
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | An enchantment with its type and level
data Enchantment = Enchantment !EnchantmentType !Int
  deriving stock (Show, Eq)

-- | External map from inventory slot index to list of enchantments
type EnchantmentMap = IORef (Map.Map Int [Enchantment])

-- | Create a new empty enchantment map
newEnchantmentMap :: IO EnchantmentMap
newEnchantmentMap = newIORef Map.empty

-- | Get enchantments for a given slot
getEnchantments :: EnchantmentMap -> Int -> IO [Enchantment]
getEnchantments ref slot = do
  m <- readIORef ref
  pure (Map.findWithDefault [] slot m)

-- | Set enchantments for a given slot
setEnchantments :: EnchantmentMap -> Int -> [Enchantment] -> IO ()
setEnchantments ref slot enchants =
  modifyIORef' ref (Map.insert slot enchants)

-- | Clear enchantments for a given slot
clearEnchantments :: EnchantmentMap -> Int -> IO ()
clearEnchantments ref slot =
  modifyIORef' ref (Map.delete slot)

-- | Maximum level for each enchantment type
enchantmentMaxLevel :: EnchantmentType -> Int
enchantmentMaxLevel = \case
  Sharpness  -> 5
  Protection -> 4
  Efficiency -> 5
  Unbreaking -> 3
  Power      -> 5
  Knockback  -> 2

-- | Display name for an enchantment type
enchantmentName :: EnchantmentType -> String
enchantmentName = \case
  Sharpness  -> "Sharpness"
  Protection -> "Protection"
  Efficiency -> "Efficiency"
  Unbreaking -> "Unbreaking"
  Power      -> "Power"
  Knockback  -> "Knockback"

-- | Generate 3 enchantment options given player XP level and a seed.
--   Returns [(enchantmentType, level, xpCost)].
--   Higher player level allows higher enchantment levels.
generateEnchantments :: Int -> Int -> [(EnchantmentType, Int, Int)]
generateEnchantments xpLevel seed =
  let allTypes = [minBound .. maxBound] :: [EnchantmentType]
      -- Deterministic selection of 3 enchantment types using seed
      pick i = allTypes !! (abs (seed * 31 + i * 7 + 13) `mod` length allTypes)
      -- Enchantment level scales with player XP level
      lvl i =
        let base = abs (seed * 17 + i * 23 + 5) `mod` maxLvl + 1
            maxLvl = enchantmentMaxLevel (pick i)
            -- Cap based on player level: need at least level*3 XP to get max enchantment
            capFromXp = max 1 (min maxLvl ((xpLevel + 2) `div` 3))
        in min base capFromXp
      -- XP cost: scales with enchantment level
      cost i = lvl i * 2 + abs (seed * 11 + i * 37) `mod` 3
      mkOption i = (pick i, lvl i, cost i)
  in [mkOption 0, mkOption 1, mkOption 2]

-- | Apply an enchantment effect description.
--   Returns a descriptive string of what the enchantment does.
--   The actual effect is handled by the game loop (e.g. damage multipliers).
applyEnchantment :: Enchantment -> String
applyEnchantment (Enchantment etype level) = case etype of
  Sharpness  -> "+" ++ show level ++ " attack damage"
  Protection -> "+" ++ show level ++ " defense"
  Efficiency -> "+" ++ show (level * 2) ++ "0% mining speed"
  Unbreaking -> "+" ++ show (level * 50) ++ "% durability"
  Power      -> "+" ++ show level ++ " ranged damage"
  Knockback  -> "+" ++ show level ++ " knockback"
