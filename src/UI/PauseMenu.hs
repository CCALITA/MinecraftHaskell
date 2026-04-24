module UI.PauseMenu
  ( PauseButton(..)
  , pauseResumeButton
  , pauseSaveQuitButton
  , pauseButtonHit
  , pauseOverlayVerts
  ) where

import Engine.BitmapFont (renderTextCentered)

-- | A rectangular button on the pause screen, defined in NDC coordinates.
data PauseButton = PauseButton
  { pbLeft   :: !Float
  , pbTop    :: !Float   -- ^ smaller Y = higher on screen (Vulkan NDC)
  , pbRight  :: !Float
  , pbBottom :: !Float   -- ^ larger Y = lower on screen
  } deriving stock (Show, Eq)

-- | Resume button rectangle (green, centered).
pauseResumeButton :: PauseButton
pauseResumeButton = PauseButton
  { pbLeft   = -0.30
  , pbTop    = -0.10
  , pbRight  =  0.30
  , pbBottom =  0.07
  }

-- | Save & Quit button rectangle (red, centered, below Resume).
pauseSaveQuitButton :: PauseButton
pauseSaveQuitButton = PauseButton
  { pbLeft   = -0.30
  , pbTop    =  0.14
  , pbRight  =  0.30
  , pbBottom =  0.31
  }

-- | Test whether an NDC coordinate falls inside a 'PauseButton'.
pauseButtonHit :: PauseButton -> Float -> Float -> Bool
pauseButtonHit pb x y =
  x >= pbLeft pb && x <= pbRight pb && y >= pbTop pb && y <= pbBottom pb

-- | Generate all HUD vertices for the styled pause overlay:
--   dark semi-transparent background, "PAUSED" title in white,
--   green "Resume" button with white text, and red "Save & Quit" button
--   with white text.
pauseOverlayVerts :: [Float]
pauseOverlayVerts =
  -- Full-screen dark semi-transparent overlay
  quad (-1) (-1) 1 1 (0, 0, 0, 0.7)
  -- "PAUSED" title (large, white, near top)
  ++ renderTextCentered (-0.45) 2.0 (1, 1, 1, 1) "PAUSED"
  -- Resume button: outer border (darker green) + inner fill (green) + white text
  ++ let rb = pauseResumeButton
     in quad (pbLeft rb) (pbTop rb) (pbRight rb) (pbBottom rb) (0.15, 0.45, 0.18, 1.0)
        ++ quad (pbLeft rb + 0.02) (pbTop rb + 0.02) (pbRight rb - 0.02) (pbBottom rb - 0.02) (0.25, 0.65, 0.30, 1.0)
        ++ renderTextCentered (-0.05) 1.0 (1, 1, 1, 1) "RESUME"
  -- Save & Quit button: outer border (darker red) + inner fill (red) + white text
  ++ let sq = pauseSaveQuitButton
     in quad (pbLeft sq) (pbTop sq) (pbRight sq) (pbBottom sq) (0.55, 0.15, 0.15, 1.0)
        ++ quad (pbLeft sq + 0.02) (pbTop sq + 0.02) (pbRight sq - 0.02) (pbBottom sq - 0.02) (0.75, 0.25, 0.25, 1.0)
        ++ renderTextCentered 0.19 1.0 (1, 1, 1, 1) "SAVE AND QUIT"
  where
    quad :: Float -> Float -> Float -> Float -> (Float, Float, Float, Float) -> [Float]
    quad x0 y0 x1 y1 (r, g, b, a) =
      [x0, y0, r, g, b, a,  x1, y0, r, g, b, a,  x1, y1, r, g, b, a
      ,x0, y0, r, g, b, a,  x1, y1, r, g, b, a,  x0, y1, r, g, b, a]
