module UI.CompassBar
  ( compassBarVerts
  , yawToDirection
  , directionMarkers
  , markerNdcOffset
  , compassBarHeight
  , compassBarY
  ) where

import Engine.BitmapFont (renderText)

-- | Cardinal and intercardinal direction markers with their yaw angles (degrees).
--   Yaw 0 = North (+Z), 90 = East (+X), 180 = South (-Z), 270 = West (-X).
directionMarkers :: [(String, Float)]
directionMarkers =
  [ ("N",  0)
  , ("NE", 45)
  , ("E",  90)
  , ("SE", 135)
  , ("S",  180)
  , ("SW", 225)
  , ("W",  270)
  , ("NW", 315)
  ]

-- | Height of the compass bar background in NDC units.
compassBarHeight :: Float
compassBarHeight = 0.06

-- | Top-edge Y position of the compass bar in Vulkan NDC (Y=-1 = top of screen).
compassBarY :: Float
compassBarY = -0.96

-- | Visible half-width of the compass bar in NDC (how far left/right from center).
compassBarHalfWidth :: Float
compassBarHalfWidth = 0.45

-- | Convert a yaw angle in degrees to the nearest cardinal/intercardinal label.
yawToDirection :: Float -> String
yawToDirection yaw =
  let normalized = yaw `mod'` 360
      idx = round (normalized / 45) `mod` 8
      labels = ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]
  in labels !! idx

-- | Floating-point modulo that always returns a non-negative result.
mod' :: Float -> Float -> Float
mod' x m =
  let r = x - m * fromIntegral (floor (x / m) :: Int)
  in if r < 0 then r + m else r

-- | Calculate the NDC X offset for a direction marker given the player yaw.
--   Returns the horizontal offset from screen center, where each degree of
--   yaw maps to a fixed NDC width. Markers outside the visible bar are clipped.
markerNdcOffset :: Float -> Float -> Float
markerNdcOffset playerYaw markerYaw =
  let diff = markerYaw - playerYaw
      -- Wrap to [-180, 180] range
      wrapped
        | diff > 180  = diff - 360
        | diff < -180 = diff + 360
        | otherwise   = diff
      -- Map degrees to NDC: 180 degrees spans the full bar width
      degreesPerNdc = 180 / compassBarHalfWidth
  in wrapped / degreesPerNdc

-- | Generate HUD vertex data for the compass direction bar.
--   Takes player yaw (degrees) and screen width (unused, bar uses NDC directly).
--   Returns a flat list of floats for the HUD vertex buffer (vec2 pos + vec4 color per vertex).
compassBarVerts :: Float -> Float -> [Float]
compassBarVerts playerYaw _screenWidth =
  let bgVerts    = backgroundVerts
      markerVs   = concatMap (renderMarker playerYaw) directionMarkers
      centerTick = centerTickVerts
  in bgVerts ++ markerVs ++ centerTick

-- | Dark semi-transparent background bar across the top of the screen.
backgroundVerts :: [Float]
backgroundVerts =
  let x0 = -compassBarHalfWidth
      x1 = compassBarHalfWidth
      y0 = compassBarY
      y1 = compassBarY + compassBarHeight
      (r, g, b, a) = (0.0, 0.0, 0.0, 0.5 :: Float)
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]

-- | Small white tick mark at the center of the compass bar.
centerTickVerts :: [Float]
centerTickVerts =
  let tw = 0.003   -- tick half-width
      th = 0.015   -- tick height
      y0 = compassBarY
      y1 = compassBarY + th
      (r, g, b, a) = (1.0, 1.0, 1.0, 0.9 :: Float)
  in [ -tw, y0, r, g, b, a
     ,  tw, y0, r, g, b, a
     ,  tw, y1, r, g, b, a
     , -tw, y0, r, g, b, a
     ,  tw, y1, r, g, b, a
     , -tw, y1, r, g, b, a
     ]

-- | Render a single direction marker (letter + small tick) at its correct
--   horizontal position based on the current player yaw.
renderMarker :: Float -> (String, Float) -> [Float]
renderMarker playerYaw (label, markerYaw) =
  let offset = markerNdcOffset playerYaw markerYaw
  in if abs offset > compassBarHalfWidth
       then []  -- off-screen, clip
       else
         let -- Cardinal directions (N/S/E/W) are brighter and larger
             isCardinal = length label == 1
             textScale  = if isCardinal then 0.9 else 0.65
             textColor  = if isCardinal
                            then (1.0, 1.0, 1.0, 1.0 :: Float)
                            else (0.7, 0.7, 0.7, 0.8 :: Float)
             -- Center the text on the offset position
             textW = fromIntegral (length label) * 0.032 * textScale
             textX = offset - textW / 2
             textY = compassBarY + 0.012
             letterVerts = renderText textX textY textScale textColor label
             -- Small tick below the letter
             tickW = 0.002
             tickH = 0.008
             tickY = compassBarY + 0.002
             tickColor = if isCardinal
                           then (1.0, 1.0, 1.0, 0.8 :: Float)
                           else (0.5, 0.5, 0.5, 0.6 :: Float)
             tickVerts =
               [ offset - tickW, tickY, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               , offset + tickW, tickY, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               , offset + tickW, tickY + tickH, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               , offset - tickW, tickY, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               , offset + tickW, tickY + tickH, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               , offset - tickW, tickY + tickH, fst4 tickColor, snd4 tickColor, thd4 tickColor, fth4 tickColor
               ]
         in letterVerts ++ tickVerts
  where
    fst4 (a, _, _, _) = a
    snd4 (_, b, _, _) = b
    thd4 (_, _, c, _) = c
    fth4 (_, _, _, d) = d
