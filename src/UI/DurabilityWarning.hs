module UI.DurabilityWarning
  ( durabilityWarning
  , warningFlashAlpha
  ) where

import Game.Item (Item(..), ToolMaterial(..), toolInfo, ToolInfo(..))

-- | Return a warning string when a tool/armor/durable item is below 10%
--   of its maximum durability.  Returns 'Nothing' for non-durable items
--   or items that still have more than 10% durability remaining.
durabilityWarning :: Item -> Maybe String
durabilityWarning item = case maxDurability item of
  Nothing  -> Nothing
  Just maxDur
    | maxDur <= 0           -> Nothing
    | durLeft <= threshold  -> Just "Low Durability!"
    | otherwise             -> Nothing
    where
      durLeft   = currentDurability item
      threshold = maxDur `div` 10   -- 10% of max

-- | Compute a pulsing alpha value for the warning flash.
--   The result oscillates between 0.5 and 1.0 using a sine wave.
--
--   @time@ is an elapsed-seconds value (e.g. from the game clock).
warningFlashAlpha :: Float -> Float
warningFlashAlpha time =
  let wave = sin (time * 2 * pi)   -- oscillates -1 .. 1
  in 0.75 + 0.25 * wave            -- maps to 0.5 .. 1.0

-- | Maximum durability for items that have a durability gauge.
--   Returns 'Nothing' for non-durable items.
maxDurability :: Item -> Maybe Int
maxDurability (ToolItem _ mat _)       = Just (tiMaxDurability (toolInfo mat))
maxDurability (ArmorItem _ _ _)        = Just (armorMaxDurability)
maxDurability (ShearsItem _)           = Just 238
maxDurability (FlintAndSteelItem _)    = Just 64
maxDurability (FishingRodItem _)       = Just 64
maxDurability _                        = Nothing

-- | Current remaining durability stored inside the item.
currentDurability :: Item -> Int
currentDurability (ToolItem _ _ d)       = d
currentDurability (ArmorItem _ _ d)      = d
currentDurability (ShearsItem d)         = d
currentDurability (FlintAndSteelItem d)  = d
currentDurability (FishingRodItem d)     = d
currentDurability _                      = 0

-- | Placeholder max durability for armor items (leather tier, simplest).
--   A more granular implementation would look up by ArmorMaterial + ArmorSlot.
armorMaxDurability :: Int
armorMaxDurability = 165
