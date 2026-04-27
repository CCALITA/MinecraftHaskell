module Game.PickupToast
  ( PickupToast(..)
  , addPickupToast
  , tickPickupToasts
  , formatToast
  , maxToasts
  , toastDuration
  , mergeToasts
  , emptyToasts
  ) where

import Game.Item (Item(..))

-- | A single pickup toast notification
data PickupToast = PickupToast
  { ptItem    :: !Item
  , ptCount   :: !Int
  , ptTimer   :: !Float   -- seconds remaining before expiry
  } deriving stock (Show, Eq)

-- | Maximum number of toasts displayed simultaneously
maxToasts :: Int
maxToasts = 5

-- | Default duration for a new toast (seconds)
toastDuration :: Float
toastDuration = 5.0

-- | Empty toast list
emptyToasts :: [PickupToast]
emptyToasts = []

-- | Add a pickup toast, merging with an existing toast for the same item
--   if one exists (resets timer and adds count). Drops oldest when at max.
addPickupToast :: Item -> Int -> [PickupToast] -> [PickupToast]
addPickupToast _item 0 toasts = toasts
addPickupToast item count toasts =
  let (merged, found) = mergeScan item count toasts
  in if found
     then merged
     else take maxToasts (PickupToast item count toastDuration : toasts)

-- | Scan the list and merge into the first matching toast
mergeScan :: Item -> Int -> [PickupToast] -> ([PickupToast], Bool)
mergeScan _ _ [] = ([], False)
mergeScan item count (t:ts)
  | ptItem t == item =
      (t { ptCount = ptCount t + count, ptTimer = toastDuration } : ts, True)
  | otherwise =
      let (rest, found) = mergeScan item count ts
      in (t : rest, found)

-- | Tick all toasts by dt seconds, removing expired ones
tickPickupToasts :: Float -> [PickupToast] -> [PickupToast]
tickPickupToasts dt = filter (\t -> ptTimer t > 0) . map (\t -> t { ptTimer = ptTimer t - dt })

-- | Format a toast for display: "Picked up 3x Dirt" or "Picked up Dirt"
formatToast :: PickupToast -> String
formatToast t
  | ptCount t <= 0 = ""
  | ptCount t == 1 = "Picked up " ++ itemName (ptItem t)
  | otherwise      = "Picked up " ++ show (ptCount t) ++ "x " ++ itemName (ptItem t)

-- | Merge duplicate items in a toast list (combine same-item entries)
mergeToasts :: [PickupToast] -> [PickupToast]
mergeToasts [] = []
mergeToasts (t:ts) =
  let (same, diff) = foldr partitionItem ([], []) ts
      merged = t { ptCount = ptCount t + sum (map ptCount same)
                 , ptTimer = maximum (ptTimer t : map ptTimer same)
                 }
  in merged : mergeToasts diff
  where
    partitionItem x (s, d)
      | ptItem x == ptItem t = (x : s, d)
      | otherwise            = (s, x : d)

-- | Simple item name for display
itemName :: Item -> String
itemName (BlockItem bt)    = show bt
itemName (ToolItem tt _ _) = show tt
itemName StickItem         = "Stick"
itemName (FoodItem ft)     = show ft
itemName (MaterialItem mt) = show mt
itemName (ArmorItem s _ _) = show s
itemName (ShearsItem _)    = "Shears"
itemName (FlintAndSteelItem _) = "Flint and Steel"
itemName CompassItem       = "Compass"
itemName ClockItem         = "Clock"
itemName (FishingRodItem _) = "Fishing Rod"
itemName GlassBottleItem   = "Glass Bottle"
itemName (PotionItem pt)   = show pt
itemName BoatItem          = "Boat"
itemName MinecartItem      = "Minecart"
itemName (BucketItem bt)   = show bt
