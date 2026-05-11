module Game.EatingAnimation
  ( eatingProgress
  , eatingParticleCount
  , eatingBarVerts
  ) where

-- | Compute eating progress as a fraction in [0,1].
--   @eatingProgress timer duration@ returns how far through the eating
--   animation we are. Timer counts up from 0 toward duration.
eatingProgress :: Float -> Float -> Float
eatingProgress timer duration
  | duration <= 0 = 1.0
  | otherwise     = min 1.0 (max 0.0 (timer / duration))

-- | Number of food particles to emit based on eating progress.
--   Returns 0 at start, scaling up to 6 at full progress.
eatingParticleCount :: Float -> Int
eatingParticleCount progress
  | progress <= 0 = 0
  | otherwise     = min 6 (floor (progress * 6.0 + 0.5))

-- | Generate green progress bar vertices for the eating HUD element.
--   Returns a list of floats representing two triangles (6 verts, each with
--   x, y, r, g, b, a = 6 floats per vertex = 36 floats total).
--   The bar width is scaled by progress.
eatingBarVerts :: Float -> Float -> Float -> [Float]
eatingBarVerts progress x y
  | progress <= 0 = []
  | otherwise     =
      let barWidth  = 0.2 * clampedProgress
          barHeight = 0.02
          x0 = x
          x1 = x + barWidth
          y0 = y
          y1 = y + barHeight
          -- Green color with alpha
          r = 0.2
          g = 0.8
          b = 0.2
          a = 0.9
          -- Triangle 1: bottom-left, bottom-right, top-right
          -- Triangle 2: bottom-left, top-right, top-left
      in [ x0, y0, r, g, b, a
         , x1, y0, r, g, b, a
         , x1, y1, r, g, b, a
         , x0, y0, r, g, b, a
         , x1, y1, r, g, b, a
         , x0, y1, r, g, b, a
         ]
  where
    clampedProgress = min 1.0 (max 0.0 progress)
