module UI.CelestialBody
  ( sunScreenPos
  , moonScreenPos
  , celestialDiscVerts
  ) where

import Linear (V3(..), V4(..), M44, normalize, (!*))

-- | Project a world-space direction onto screen NDC via the view-projection matrix.
--   Returns Nothing when the body is behind the camera (w <= 0).
projectDir :: V3 Float -> M44 Float -> Maybe (Float, Float)
projectDir (V3 dx dy dz) vp =
  let V4 cx cy _cz cw = vp !* V4 dx dy dz 0
  in if cw <= 0
       then Nothing
       else Just (cx / cw, cy / cw)

-- | Sun screen position in NDC given the sun direction and view-projection matrix.
--   Returns Nothing when the sun is behind the camera.
sunScreenPos :: V3 Float -> M44 Float -> Maybe (Float, Float)
sunScreenPos sunDir vp = projectDir (normalize sunDir) vp

-- | Moon screen position in NDC (opposite the sun direction).
moonScreenPos :: V3 Float -> M44 Float -> Maybe (Float, Float)
moonScreenPos (V3 sx sy sz) vp =
  projectDir (normalize (V3 (-sx) (-sy) (-sz))) vp

-- | Generate a screen-space quad (two triangles, 6 vertices) for a celestial disc.
--   Each vertex is 6 floats: x y r g b a.
--   Parameters: x y (NDC center), size (half-width), (r,g,b,a) color.
celestialDiscVerts :: Float -> Float -> Float -> (Float, Float, Float, Float) -> [Float]
celestialDiscVerts cx cy sz (r, g, b, a) =
  let x0 = cx - sz
      x1 = cx + sz
      y0 = cy - sz
      y1 = cy + sz
  in [ x0, y0, r, g, b, a
     , x1, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y0, r, g, b, a
     , x1, y1, r, g, b, a
     , x0, y1, r, g, b, a
     ]
