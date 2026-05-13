module UI.PauseScreen
  ( pauseScreenVerts
  , overlayColor
  , titleY
  , resumeButtonY
  , quitButtonY
  , buttonWidth
  , buttonHeight
  , isInsideResumeButton
  , isInsideQuitButton
  ) where

import Engine.BitmapFont (renderTextCentered, renderText, charSpacing)

-- | Dark overlay color with semi-transparent alpha
overlayColor :: (Float, Float, Float, Float)
overlayColor = (0.0, 0.0, 0.0, 0.6)

-- | Y position for "PAUSED" title text (Vulkan NDC, negative = upper)
titleY :: Float
titleY = -0.3

-- | Button dimensions in NDC
buttonWidth :: Float
buttonWidth = 0.5

buttonHeight :: Float
buttonHeight = 0.08

-- | Y position for "RESUME" button
resumeButtonY :: Float
resumeButtonY = 0.0

-- | Y position for "QUIT" button
quitButtonY :: Float
quitButtonY = 0.15

-- | Check whether a mouse position (in NDC) is inside the resume button.
isInsideResumeButton :: Float -> Float -> Bool
isInsideResumeButton = isInsideButton resumeButtonY

-- | Check whether a mouse position (in NDC) is inside the quit button.
isInsideQuitButton :: Float -> Float -> Bool
isInsideQuitButton = isInsideButton quitButtonY

-- | Generic button hit-test helper. The button is centered horizontally.
isInsideButton :: Float -> Float -> Float -> Bool
isInsideButton btnY mx my =
  let halfW = buttonWidth / 2
      left  = -halfW
      right = halfW
      top   = btnY
      bot   = btnY + buttonHeight
  in  mx >= left && mx <= right && my >= top && my <= bot

-- | Build full-screen pause screen vertex data for the HUD pipeline.
--   @mouseX@ : current mouse X in NDC [-1, 1]
--   @mouseY@ : current mouse Y in NDC [-1, 1]
--   Returns a flat list of floats (vec2 pos + vec4 color = 6 floats per vertex).
pauseScreenVerts :: Float -> Float -> [Float]
pauseScreenVerts mouseX mouseY =
  let overlay = buildOverlay
      title   = buildTitle
      resume  = buildButtonVerts "RESUME" resumeButtonY mouseX mouseY
      quit    = buildButtonVerts "QUIT" quitButtonY mouseX mouseY
  in  overlay ++ title ++ resume ++ quit

-- | Full-screen dark overlay: two triangles covering [-1,1] x [-1,1]
buildOverlay :: [Float]
buildOverlay =
  let (r, g, b, a) = overlayColor
      vert x y = [x, y, r, g, b, a]
  in  vert (-1) (-1) ++ vert 1 (-1) ++ vert 1 1
   ++ vert (-1) (-1) ++ vert 1 1    ++ vert (-1) 1

-- | "PAUSED" title rendered in large scale with white color
buildTitle :: [Float]
buildTitle =
  let titleColor = (1.0, 1.0, 1.0, 1.0)
      scale      = 3.0
  in  renderTextCentered titleY scale titleColor "PAUSED"

-- | A button with hover highlight and centered text label.
buildButtonVerts :: String -> Float -> Float -> Float -> [Float]
buildButtonVerts label btnY mx my =
  let hovered  = isInsideButton btnY mx my
      bgColor  = if hovered
                   then (0.4, 0.4, 0.4, 0.9)
                   else (0.3, 0.3, 0.3, 0.8)
      halfW    = buttonWidth / 2
      left     = -halfW
      right    = halfW
      top      = btnY
      bot      = btnY + buttonHeight
      (r, g, b, a) = bgColor
      vert x y = [x, y, r, g, b, a]
      bg = vert left top ++ vert right top ++ vert right bot
        ++ vert left top ++ vert right bot ++ vert left  bot
      -- Text inside the button
      textColor = if hovered
                    then (1.0, 1.0, 0.6, 1.0)
                    else (1.0, 1.0, 1.0, 1.0)
      textScale = 1.0
      textW     = fromIntegral (length label) * charSpacing * textScale
      textX     = -(textW / 2)
      textBtnY  = btnY + 0.02
      txt       = renderText textX textBtnY textScale textColor label
  in  bg ++ txt
