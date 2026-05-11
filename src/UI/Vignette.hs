module UI.Vignette
  ( vignetteAlpha
  , vignetteGrid
  ) where

-- | Compute vignette alpha for a screen-space position.
--   @flash@ is the damage flash intensity (0 = none, 1 = full).
--   @x@ and @y@ are normalised screen coordinates in [-1, 1].
--   Result is clamped to [0, 1].
vignetteAlpha :: Float -> Float -> Float -> Float
vignetteAlpha flash x y =
  let raw = flash * (x * x + y * y)
  in  min 1.0 (max 0.0 raw)

-- | Generate a 4×4 grid of vignette alpha values for red damage-flash quads.
--   Each cell centre is placed at evenly spaced positions across [-1, 1].
--   The returned list has 16 elements (row-major order).
vignetteGrid :: Float -> [Float]
vignetteGrid flash =
  [ vignetteAlpha flash cx cy
  | iy <- [0 .. 3 :: Int]
  , ix <- [0 .. 3 :: Int]
  , let cx = -0.75 + fromIntegral ix * 0.5
        cy = -0.75 + fromIntegral iy * 0.5
  ]
