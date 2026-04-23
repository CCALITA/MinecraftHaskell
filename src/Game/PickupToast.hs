module Game.PickupToast
  ( PickupToast(..)
  , mergeToast
  , tickToasts
  , formatToast
  , defaultToastDuration
  ) where

-- | A notification toast for a picked-up item.
data PickupToast = PickupToast
  { ptName  :: !String   -- ^ Item display name
  , ptCount :: !Int      -- ^ Total count accumulated
  , ptTimer :: !Float    -- ^ Remaining display time in seconds
  } deriving stock (Show, Eq)

-- | Default duration for a pickup toast (seconds).
defaultToastDuration :: Float
defaultToastDuration = 3.0

-- | Merge a newly collected item into the toast list.
-- If a toast with the same name already exists, add the count and reset the timer.
-- Otherwise, append a new toast entry.
mergeToast :: String -> Int -> [PickupToast] -> [PickupToast]
mergeToast name count toasts =
  let (matched, rest) = partitionByName name toasts
  in case matched of
    []    -> rest ++ [PickupToast name count defaultToastDuration]
    (t:_) -> rest ++ [t { ptCount = ptCount t + count, ptTimer = defaultToastDuration }]

-- | Tick all toast timers by dt and remove expired ones.
tickToasts :: Float -> [PickupToast] -> [PickupToast]
tickToasts dt = filter (\t -> ptTimer t > 0) . map (\t -> t { ptTimer = ptTimer t - dt })

-- | Format a toast for display, e.g. "+5 Stone".
formatToast :: PickupToast -> String
formatToast t = "+" ++ show (ptCount t) ++ " " ++ ptName t

-- | Partition a list by matching name (avoid Data.List import).
partitionByName :: String -> [PickupToast] -> ([PickupToast], [PickupToast])
partitionByName _ [] = ([], [])
partitionByName name (x:xs) =
  let (yes, no) = partitionByName name xs
  in if ptName x == name then (x:yes, no) else (yes, x:no)
