module UI.WaterOverlay
  ( waterOverlayVerts
  ) where

-- | Generate fullscreen blue-tinted overlay vertices when the player is underwater.
--   Returns a list of 36 floats (6 vertices x 6 floats each: x y r g b a)
--   forming two triangles that cover the entire screen in Vulkan NDC [-1,1].
--   The overlay uses a semi-transparent blue tint (RGBA 0.0 0.1 0.5 0.3).
--   When not underwater, returns an empty list.
waterOverlayVerts :: Bool -> [Float]
waterOverlayVerts False = []
waterOverlayVerts True  =
  let r = 0.0 :: Float
      g = 0.1 :: Float
      b = 0.5 :: Float
      a = 0.3 :: Float
      v vx vy = [vx, vy, r, g, b, a]
  in -- Triangle 1: top-left, bottom-left, bottom-right
     v (-1) (-1) ++ v (-1) 1 ++ v 1 1
     -- Triangle 2: top-left, bottom-right, top-right
  ++ v (-1) (-1) ++ v 1 1 ++ v 1 (-1)
