module UI.SaveToast
  ( SaveToast(..)
  , newSaveToast
  , tickSaveToast
  , saveToastDuration
  ) where

-- | A transient on-screen notification shown after a save completes.
data SaveToast = SaveToast
  { stMessage   :: !String
  , stTimeLeft  :: !Float
  } deriving stock (Show, Eq)

-- | How long a save toast is visible (seconds).
saveToastDuration :: Float
saveToastDuration = 3.0

-- | Create a new save toast with the default duration.
newSaveToast :: String -> SaveToast
newSaveToast msg = SaveToast
  { stMessage  = msg
  , stTimeLeft = saveToastDuration
  }

-- | Tick the toast timer. Returns 'Nothing' once the timer expires.
tickSaveToast :: Float -> Maybe SaveToast -> Maybe SaveToast
tickSaveToast _ Nothing = Nothing
tickSaveToast dt (Just toast)
  | remaining <= 0 = Nothing
  | otherwise      = Just toast { stTimeLeft = remaining }
  where
    remaining = stTimeLeft toast - dt
