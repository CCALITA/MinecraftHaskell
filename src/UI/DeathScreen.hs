module UI.DeathScreen
  ( deathScreenVerts
  , overlayColor
  , titleY
  , scoreY
  , buttonWidth
  , buttonHeight
  , buttonY
  , isInsideButton
  ) where

import Engine.BitmapFont (renderTextCentered, renderText, charSpacing)

-- | Red overlay color with semi-transparent alpha
overlayColor :: (Float, Float, Float, Float)
overlayColor = (0.6, 0.0, 0.0, 0.5)

-- | Y position for "YOU DIED" title text (Vulkan NDC, negative = upper)
titleY :: Float
titleY = -0.25

-- | Y position for "SCORE: X" line
scoreY :: Float
scoreY = 0.0

-- | Respawn button dimensions and position in NDC
buttonWidth :: Float
buttonWidth = 0.4

buttonHeight :: Float
buttonHeight = 0.08

buttonY :: Float
buttonY = 0.2

-- | Check whether a mouse position (in NDC) is inside the respawn button.
--   The button is centered horizontally.
isInsideButton :: Float -> Float -> Bool
isInsideButton mx my =
  let halfW = buttonWidth / 2
      left  = -halfW
      right = halfW
      top   = buttonY
      bot   = buttonY + buttonHeight
  in  mx >= left && mx <= right && my >= top && my <= bot

-- | Build full-screen death screen vertex data for the HUD pipeline.
--   @xp@     : player XP at death (used for score display)
--   @days@   : number of in-game days survived
--   @mouseX@ : current mouse X in NDC [-1, 1]
--   @mouseY@ : current mouse Y in NDC [-1, 1]
--   Returns a flat list of floats (vec2 pos + vec4 color = 6 floats per vertex).
deathScreenVerts :: Int -> Int -> Float -> Float -> [Float]
deathScreenVerts xp days mouseX mouseY =
  let overlay   = buildOverlay
      title     = buildTitle
      scoreLine = buildScore xp days
      button    = buildButton mouseX mouseY
  in  overlay ++ title ++ scoreLine ++ button

-- | Full-screen red overlay: two triangles covering [-1,1] x [-1,1]
buildOverlay :: [Float]
buildOverlay =
  let (r, g, b, a) = overlayColor
      vert x y = [x, y, r, g, b, a]
  in  vert (-1) (-1) ++ vert 1 (-1) ++ vert 1 1
   ++ vert (-1) (-1) ++ vert 1 1    ++ vert (-1) 1

-- | "YOU DIED" title rendered in large scale with dark-red color
buildTitle :: [Float]
buildTitle =
  let titleColor = (0.9, 0.1, 0.1, 1.0)
      scale      = 3.0
  in  renderTextCentered titleY scale titleColor "YOU DIED"

-- | Score line: "SCORE: <xp + days*10>"
buildScore :: Int -> Int -> [Float]
buildScore xp days =
  let totalScore  = xp + days * 10
      scoreStr    = "SCORE: " ++ show totalScore
      scoreColor  = (1.0, 1.0, 1.0, 0.9)
      scale       = 1.5
  in  renderTextCentered scoreY scale scoreColor scoreStr

-- | Respawn button with hover highlight.
--   Button is a rectangle centered horizontally at buttonY.
--   When mouse hovers, the button brightens.
buildButton :: Float -> Float -> [Float]
buildButton mx my =
  let hovered  = isInsideButton mx my
      bgColor  = if hovered
                   then (0.4, 0.4, 0.4, 0.9)
                   else (0.3, 0.3, 0.3, 0.8)
      halfW    = buttonWidth / 2
      left     = -halfW
      right    = halfW
      top      = buttonY
      bot      = buttonY + buttonHeight
      (r, g, b, a) = bgColor
      vert x y = [x, y, r, g, b, a]
      bg = vert left top ++ vert right top ++ vert right bot
        ++ vert left top ++ vert right bot ++ vert left  bot
      -- "RESPAWN" text inside the button
      textColor = if hovered
                    then (1.0, 1.0, 0.6, 1.0)
                    else (1.0, 1.0, 1.0, 1.0)
      textScale = 1.0
      textStr   = "RESPAWN"
      textW     = fromIntegral (length textStr) * charSpacing * textScale
      textX     = -(textW / 2)
      textY     = buttonY + 0.02
      txt       = renderText textX textY textScale textColor textStr
  in  bg ++ txt
