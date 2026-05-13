module UI.CoordinateDisplay
  ( coordText
  , coordDisplayVerts
  ) where

import Linear (V3(..))
import Engine.BitmapFont (renderText)

-- | Format a world position as a human-readable coordinate string.
--   Truncates each component to an integer (floor toward negative infinity).
--   Example: V3 123.7 64.2 (-45.9) -> "X: 123 Y: 64 Z: -45"
coordText :: V3 Float -> String
coordText (V3 x y z) =
  "X: " ++ showCoord x ++ " Y: " ++ showCoord y ++ " Z: " ++ showCoord z
  where
    showCoord :: Float -> String
    showCoord v = show (floor v :: Int)

-- | Generate HUD vertex data for the coordinate display overlay.
--   Takes the player world position, a text scale factor, and a vertical
--   offset in NDC. Returns a flat list of floats for the HUD vertex buffer
--   (vec2 pos + vec4 color = 6 floats per vertex, 6 vertices per quad).
coordDisplayVerts :: V3 Float -> Float -> Float -> [Float]
coordDisplayVerts pos scale yOffset =
  let str = coordText pos
      textX = -0.98
      textY = yOffset
      textColor = (1.0, 1.0, 1.0, 0.9 :: Float)
  in renderText textX textY scale textColor str
