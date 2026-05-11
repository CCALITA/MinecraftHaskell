module UI.DamageDirection
  ( damageAngle
  , damageArcVerts
  ) where

import Linear (V3(..), normalize, (^-^))

-- | Compute the angle (in radians) from the player to the damage source,
--   relative to the player's facing direction (yaw).
--   Returns a value in (-pi, pi] where 0 = directly ahead,
--   positive = clockwise (right), negative = counter-clockwise (left).
damageAngle :: V3 Float -> Float -> V3 Float -> Float
damageAngle (V3 px _ pz) yaw (V3 sx _ sz) =
  let dx = sx - px
      dz = sz - pz
      -- World-space angle from player to source (atan2 gives angle from +Z axis)
      worldAngle = atan2 dx dz
      -- Relative angle: subtract player yaw
      raw = worldAngle - yaw
      -- Normalise to (-pi, pi]
  in  normaliseAngle raw

-- | Normalise an angle to the range (-pi, pi].
normaliseAngle :: Float -> Float
normaliseAngle a
  | a' > pi   = a' - 2 * pi
  | a' <= -pi = a' + 2 * pi
  | otherwise  = a'
  where a' = a - 2 * pi * fromIntegral (round (a / (2 * pi)) :: Int)

-- | Generate flat vertex data for a red damage-direction arc in Vulkan NDC.
--   The arc is a quad strip approximating a circular segment centred at the
--   given @angle@ (radians, 0 = top of screen).  @alpha@ controls opacity.
--   Returns a list of floats: [x, y, r, g, b, a, ...] with 6 verts per quad
--   (two triangles) forming an arc of ~60 degrees (5 segments).
damageArcVerts :: Float -> Float -> [Float]
damageArcVerts angle alpha =
  let segments = 5
      arcSpan = pi / 3  -- 60 degrees total
      innerR  = 0.15
      outerR  = 0.25
      step    = arcSpan / fromIntegral segments
      startA  = angle - arcSpan / 2
      segVerts i =
        let a0 = startA + fromIntegral i * step
            a1 = a0 + step
            -- Inner and outer points (screen-space: angle 0 = up = -Y in Vulkan NDC)
            ix0 =  innerR * sin a0
            iy0 = -innerR * cos a0
            ox0 =  outerR * sin a0
            oy0 = -outerR * cos a0
            ix1 =  innerR * sin a1
            iy1 = -innerR * cos a1
            ox1 =  outerR * sin a1
            oy1 = -outerR * cos a1
            r = 1.0; g = 0.0; b = 0.0
            vert x y = [x, y, r, g, b, alpha]
        in  -- Two triangles: (inner0, outer0, outer1) and (inner0, outer1, inner1)
            vert ix0 iy0 ++ vert ox0 oy0 ++ vert ox1 oy1
         ++ vert ix0 iy0 ++ vert ox1 oy1 ++ vert ix1 iy1
  in  concatMap segVerts [0 .. segments - 1]
