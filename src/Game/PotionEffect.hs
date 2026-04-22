module Game.PotionEffect
  ( EffectType(..)
  , ActiveEffect(..)
  , EffectTick(..)
  , tickEffects
  , applyPotion
  , effectName
  , removeEffectType
  ) where

import Game.Item (PotionType(..))

data EffectType
  = SpeedEffect
  | SlownessEffect
  | StrengthEffect
  | WeaknessEffect
  | RegenerationEffect
  | PoisonEffect
  | FireResistanceEffect
  | NightVisionEffect
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data ActiveEffect = ActiveEffect
  { aeType      :: !EffectType
  , aeAmplifier :: !Int
  , aeDuration  :: !Float
  } deriving stock (Show, Eq)

data EffectTick
  = HealTick !Float
  | DamageTick !Float
  | SpeedMod !Float
  | StrengthMod !Float
  deriving stock (Show, Eq)

tickEffects :: Float -> [ActiveEffect] -> ([ActiveEffect], [EffectTick])
tickEffects dt effs =
  let stepped = map (\e -> e { aeDuration = aeDuration e - dt }) effs
      alive   = filter (\e -> aeDuration e > 0) stepped
      ticks   = concatMap effectTick alive
  in (alive, ticks)

effectTick :: ActiveEffect -> [EffectTick]
effectTick (ActiveEffect SpeedEffect amp _)        = [SpeedMod (0.2 * fromIntegral (amp + 1))]
effectTick (ActiveEffect SlownessEffect amp _)     = [SpeedMod (negate $ 0.15 * fromIntegral (amp + 1))]
effectTick (ActiveEffect StrengthEffect amp _)     = [StrengthMod (3.0 * fromIntegral (amp + 1))]
effectTick (ActiveEffect WeaknessEffect amp _)     = [StrengthMod (negate $ 4.0 * fromIntegral (amp + 1))]
effectTick (ActiveEffect RegenerationEffect _ _)   = [HealTick 0.4]
effectTick (ActiveEffect PoisonEffect _ _)         = [DamageTick 0.8]
effectTick (ActiveEffect FireResistanceEffect _ _) = []
effectTick (ActiveEffect NightVisionEffect _ _)    = []

applyPotion :: PotionType -> [ActiveEffect] -> [ActiveEffect]
applyPotion WaterBottle  effs = effs
applyPotion AwkwardPotion effs = effs
applyPotion SpeedPotion  effs = addOrReplace (ActiveEffect SpeedEffect 0 30.0) effs
applyPotion HealingPotion effs = addOrReplace (ActiveEffect RegenerationEffect 0 10.0) effs
applyPotion PoisonPotion effs = addOrReplace (ActiveEffect PoisonEffect 0 5.0) effs

addOrReplace :: ActiveEffect -> [ActiveEffect] -> [ActiveEffect]
addOrReplace new = (new :) . filter (\e -> aeType e /= aeType new)

removeEffectType :: EffectType -> [ActiveEffect] -> [ActiveEffect]
removeEffectType t = filter (\e -> aeType e /= t)

effectName :: EffectType -> String
effectName SpeedEffect          = "Speed"
effectName SlownessEffect       = "Slowness"
effectName StrengthEffect       = "Strength"
effectName WeaknessEffect       = "Weakness"
effectName RegenerationEffect   = "Regeneration"
effectName PoisonEffect         = "Poison"
effectName FireResistanceEffect = "Fire Resistance"
effectName NightVisionEffect    = "Night Vision"
