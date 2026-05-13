module UI.LoadingScreen
  ( loadingScreenVerts
  , bgColor
  , barWidth
  , barHeight
  , barY
  , barBorderThickness
  , messageY
  ) where

import Engine.BitmapFont (renderTextCentered)

-- | Dark background overlay color (near-black with full alpha)
bgColor :: (Float, Float, Float, Float)
bgColor = (0.1, 0.1, 0.1, 1.0)

-- | Progress bar width in NDC
barWidth :: Float
barWidth = 0.6

-- | Progress bar height in NDC
barHeight :: Float
barHeight = 0.04

-- | Y position of the progress bar center (Vulkan NDC, positive = lower)
barY :: Float
barY = 0.1

-- | Border thickness around the progress bar in NDC
barBorderThickness :: Float
barBorderThickness = 0.006

-- | Y position for the message text (above the bar)
messageY :: Float
messageY = -0.05

-- | Build full-screen loading screen vertex data for the HUD pipeline.
--   @progress@ : load fraction in [0, 1] (clamped internally)
--   @message@  : status text to display (e.g. "LOADING WORLD")
--   Returns a flat list of floats (vec2 pos + vec4 color = 6 floats per vertex).
loadingScreenVerts :: Float -> String -> [Float]
loadingScreenVerts progress message =
  let p          = clamp01 progress
      overlay    = buildOverlay
      border     = buildBarBorder
      barBg      = buildBarBackground
      barFill    = buildBarFill p
      msgVerts   = buildMessage message
  in  overlay ++ border ++ barBg ++ barFill ++ msgVerts

-- | Clamp a value to [0, 1]
clamp01 :: Float -> Float
clamp01 x
  | x < 0    = 0
  | x > 1    = 1
  | otherwise = x

-- | Full-screen dark overlay: two triangles covering [-1,1] x [-1,1]
buildOverlay :: [Float]
buildOverlay =
  let (r, g, b, a) = bgColor
      vert x y = [x, y, r, g, b, a]
  in  vert (-1) (-1) ++ vert 1 (-1) ++ vert 1 1
   ++ vert (-1) (-1) ++ vert 1 1    ++ vert (-1) 1

-- | Border rectangle around the progress bar (slightly larger)
buildBarBorder :: [Float]
buildBarBorder =
  let halfW = barWidth / 2 + barBorderThickness
      top   = barY - barBorderThickness
      bot   = barY + barHeight + barBorderThickness
      (r, g, b, a) = (0.5, 0.5, 0.5, 0.8)
      vert x y = [x, y, r, g, b, a]
  in  vert (-halfW) top ++ vert halfW top ++ vert halfW bot
   ++ vert (-halfW) top ++ vert halfW bot ++ vert (-halfW) bot

-- | Dark background for the bar track
buildBarBackground :: [Float]
buildBarBackground =
  let halfW = barWidth / 2
      top   = barY
      bot   = barY + barHeight
      (r, g, b, a) = (0.2, 0.2, 0.2, 0.9)
      vert x y = [x, y, r, g, b, a]
  in  vert (-halfW) top ++ vert halfW top ++ vert halfW bot
   ++ vert (-halfW) top ++ vert halfW bot ++ vert (-halfW) bot

-- | Filled portion of the progress bar (green gradient)
buildBarFill :: Float -> [Float]
buildBarFill p
  | p <= 0    = []
  | otherwise =
      let halfW   = barWidth / 2
          left    = -halfW
          right   = left + barWidth * p
          top     = barY
          bot     = barY + barHeight
          (r, g, b, a) = (0.2, 0.8, 0.3, 1.0)
          vert x y = [x, y, r, g, b, a]
      in  vert left top ++ vert right top ++ vert right bot
       ++ vert left top ++ vert right bot ++ vert left  bot

-- | Message text centered above the progress bar
buildMessage :: String -> [Float]
buildMessage msg =
  let textColor = (1.0, 1.0, 1.0, 0.9)
      scale     = 1.5
  in  renderTextCentered messageY scale textColor msg
