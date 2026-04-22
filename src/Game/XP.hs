module Game.XP
  ( xpForBlock
  , xpForMobKill
  , xpLevel
  , xpForNextLevel
  , xpProgress
  ) where

import World.Block (BlockType(..))
import Entity.Mob (MobType(..), isHostile, isPassive)

-- | XP awarded for mining specific ore blocks.
--   Returns 0 for non-ore blocks.
xpForBlock :: BlockType -> Int
xpForBlock CoalOre    = 1
xpForBlock IronOre    = 1
xpForBlock GoldOre    = 3
xpForBlock DiamondOre = 7
xpForBlock LapisOre   = 2
xpForBlock RedstoneOre = 2
xpForBlock EmeraldOre = 5
xpForBlock _          = 0

-- | XP awarded for killing a mob.
--   Hostile mobs give 5 XP, passive mobs give 1-3 XP (deterministic
--   per mob type), neutral mobs give 3 XP.
xpForMobKill :: MobType -> Int
xpForMobKill mt
  | isHostile mt = 5
  | isPassive mt = passiveXP mt
  | otherwise    = 3  -- neutral (Spider, Wolf)
  where
    passiveXP :: MobType -> Int
    passiveXP Pig     = 1
    passiveXP Cow     = 2
    passiveXP Sheep   = 2
    passiveXP Chicken = 3
    passiveXP _       = 1

-- | Calculate the player level from total accumulated XP.
--   Formula: level = floor(sqrt(xp / 10))
xpLevel :: Int -> Int
xpLevel xp = floor (sqrt (fromIntegral xp / 10.0) :: Double)

-- | Total XP required to reach a given level.
--   Inverse of the level formula: xp = level^2 * 10
xpForNextLevel :: Int -> Int
xpForNextLevel lvl = (lvl + 1) ^ (2 :: Int) * 10

-- | Progress fraction (0.0 to 1.0) toward the next level.
--   Returns 0.0 at the start of a level, approaches 1.0 at the next.
xpProgress :: Int -> Float
xpProgress xp =
  let lvl       = xpLevel xp
      xpAtLevel = lvl ^ (2 :: Int) * 10
      xpAtNext  = (lvl + 1) ^ (2 :: Int) * 10
      needed    = xpAtNext - xpAtLevel
  in if needed <= 0
     then 0.0
     else fromIntegral (xp - xpAtLevel) / fromIntegral needed
