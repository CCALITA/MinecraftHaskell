module UI.CrosshairSpread
  ( crosshairSpread
  , crosshairWithSpread
  ) where

-- | Compute crosshair spread offset based on player movement state.
--   Sprinting adds 0.01, being midair adds 0.01, effects stack additively.
--   Returns a value in [0, 0.02].
crosshairSpread :: Bool -> Bool -> Float
crosshairSpread sprinting midair =
  let sprintOffset = if sprinting then 0.01 else 0.0
      airOffset    = if midair    then 0.01 else 0.0
  in sprintOffset + airOffset

-- | Given a spread offset and base crosshair extents (left, top, right, bottom),
--   return 48 floats (4 quads * 6 verts * 2 coords) for the four crosshair arms,
--   each pushed outward by @spread@.
crosshairWithSpread :: Float -> (Float, Float, Float, Float) -> [Float]
crosshairWithSpread spread (left, top, right, bottom) =
  let halfW  = (right - left) / 2.0
      halfH  = (top - bottom) / 2.0
      cx     = (left + right) / 2.0
      cy     = (top + bottom) / 2.0
      armLen = halfW
      thick  = 0.002
      -- left arm
      lx0 = cx - armLen - spread
      lx1 = cx - spread
      -- right arm
      rx0 = cx + spread
      rx1 = cx + armLen + spread
      -- top arm (Vulkan NDC: top = negative Y)
      ty0 = cy - halfH - spread
      ty1 = cy - spread
      -- bottom arm
      by0 = cy + spread
      by1 = cy + halfH + spread
  in  -- left arm quad (2 triangles = 6 verts)
      [ lx0, cy - thick, lx1, cy - thick, lx1, cy + thick
      , lx0, cy - thick, lx1, cy + thick, lx0, cy + thick
      -- right arm quad
      , rx0, cy - thick, rx1, cy - thick, rx1, cy + thick
      , rx0, cy - thick, rx1, cy + thick, rx0, cy + thick
      -- top arm quad
      , cx - thick, ty0, cx + thick, ty0, cx + thick, ty1
      , cx - thick, ty0, cx + thick, ty1, cx - thick, ty1
      -- bottom arm quad
      , cx - thick, by0, cx + thick, by0, cx + thick, by1
      , cx - thick, by0, cx + thick, by1, cx - thick, by1
      ]
