module Game.TickLimiter
  ( shouldTick
  , ticksThisFrame
  , maxTicksPerFrame
  ) where

-- | Whether enough time has accumulated to run at least one tick.
--   Returns 'True' when @accum >= rate@.
shouldTick :: Float -> Float -> Bool
shouldTick accum rate = accum >= rate

-- | How many fixed-timestep ticks to execute this frame.
--   Returns @min (floor (accum / rate)) 'maxTicksPerFrame'@,
--   clamped to 0 when @rate <= 0@ or @accum < rate@.
ticksThisFrame :: Float -> Float -> Int
ticksThisFrame accum rate
  | rate <= 0  = 0
  | accum < rate = 0
  | otherwise  = min (floor (accum / rate)) maxTicksPerFrame

-- | Hard cap on simulation ticks per frame to prevent spiral-of-death.
maxTicksPerFrame :: Int
maxTicksPerFrame = 10
