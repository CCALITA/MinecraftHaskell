module UI.MainMenu
  ( mainMenuVerts
  , titleText
  , titleY
  , titleScale
  , titleColor
  , playButtonY
  , quitButtonY
  , buttonWidth
  , buttonHeight
  , isInsidePlayButton
  , isInsideQuitButton
  ) where

import Engine.BitmapFont (renderTextCentered, renderText, charSpacing)

-- | Title text displayed at the top of the main menu
titleText :: String
titleText = "MINECRAFT HASKELL"

-- | Y position for the title in Vulkan NDC (negative = upper half)
titleY :: Float
titleY = -0.45

-- | Scale factor for the title text
titleScale :: Float
titleScale = 3.0

-- | Title text color (golden yellow)
titleColor :: (Float, Float, Float, Float)
titleColor = (1.0, 0.85, 0.0, 1.0)

-- | Button dimensions in NDC
buttonWidth :: Float
buttonWidth = 0.5

buttonHeight :: Float
buttonHeight = 0.08

-- | Y position for the "PLAY" button
playButtonY :: Float
playButtonY = 0.0

-- | Y position for the "QUIT" button
quitButtonY :: Float
quitButtonY = 0.15

-- | Check whether a mouse position (in NDC) is inside the play button.
isInsidePlayButton :: Float -> Float -> Bool
isInsidePlayButton mx my = isInsideButton mx my playButtonY

-- | Check whether a mouse position (in NDC) is inside the quit button.
isInsideQuitButton :: Float -> Float -> Bool
isInsideQuitButton mx my = isInsideButton mx my quitButtonY

-- | Generic button hit-test: centered horizontally at the given Y.
isInsideButton :: Float -> Float -> Float -> Bool
isInsideButton mx my btnY =
  let halfW = buttonWidth / 2
      left  = -halfW
      right = halfW
      top   = btnY
      bot   = btnY + buttonHeight
  in  mx >= left && mx <= right && my >= top && my <= bot

-- | Build full main-menu vertex data for the HUD pipeline.
--   @mouseX@ : current mouse X in NDC [-1, 1]
--   @mouseY@ : current mouse Y in NDC [-1, 1]
--   Returns a flat list of floats (vec2 pos + vec4 color = 6 floats per vertex).
mainMenuVerts :: Float -> Float -> [Float]
mainMenuVerts mouseX mouseY =
  let bg      = buildBackground
      title   = buildTitle
      playBtn = buildButton "PLAY" playButtonY mouseX mouseY
      quitBtn = buildButton "QUIT" quitButtonY mouseX mouseY
  in  bg ++ title ++ playBtn ++ quitBtn

-- | Dark semi-transparent background overlay
buildBackground :: [Float]
buildBackground =
  let (r, g, b, a) = (0.1, 0.1, 0.15, 0.85)
      vert x y = [x, y, r, g, b, a]
  in  vert (-1) (-1) ++ vert 1 (-1) ++ vert 1 1
   ++ vert (-1) (-1) ++ vert 1 1    ++ vert (-1) 1

-- | Title text "MINECRAFT HASKELL" in large golden letters
buildTitle :: [Float]
buildTitle = renderTextCentered titleY titleScale titleColor titleText

-- | A single button with label, hover highlight, and text.
buildButton :: String -> Float -> Float -> Float -> [Float]
buildButton label btnY mx my =
  let hovered  = isInsideButton mx my btnY
      bgColor  = if hovered
                   then (0.4, 0.4, 0.5, 0.9)
                   else (0.25, 0.25, 0.3, 0.8)
      halfW    = buttonWidth / 2
      left     = -halfW
      right    = halfW
      top      = btnY
      bot      = btnY + buttonHeight
      (r, g, b, a) = bgColor
      vert x y = [x, y, r, g, b, a]
      bg = vert left top ++ vert right top ++ vert right bot
        ++ vert left top ++ vert right bot ++ vert left  bot
      textColor = if hovered
                    then (1.0, 1.0, 0.6, 1.0)
                    else (1.0, 1.0, 1.0, 1.0)
      textScale = 1.0
      textW     = fromIntegral (length label) * charSpacing * textScale
      textX     = -(textW / 2)
      textYPos  = btnY + 0.02
      txt       = renderText textX textYPos textScale textColor label
  in  bg ++ txt
