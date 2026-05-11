module UI.StarField
  ( starPositions
  , starBrightness
  ) where

-- | Deterministic star positions in NDC space (-1..1, -1..1) from a seed.
--   Uses a simple linear congruential generator for reproducibility.
starPositions :: Int -> [(Float, Float)]
starPositions seed =
  let go _ 0 = []
      go s n =
        let s1 = lcg s
            s2 = lcg s1
            x  = toNdc s1
            y  = toNdc s2
        in (x, y) : go s2 (n - 1 :: Int)
  in go seed 200

-- | Star alpha based on ambient light level and normalised time-of-day.
--   Stars are visible only when ambient < 0.3.
--   Returns alpha in [0..1].
starBrightness :: Float -> Float -> Float
starBrightness ambient time
  | ambient >= 0.3 = 0.0
  | otherwise      =
      let baseFade = (0.3 - ambient) / 0.3          -- 0..1
          twinkle  = 0.5 + 0.5 * sin (time * 6.2832) -- gentle flicker
      in clampF 0.0 1.0 (baseFade * twinkle)

-- internal helpers

lcg :: Int -> Int
lcg s = (s * 1103515245 + 12345) `mod` 0x7FFFFFFF

toNdc :: Int -> Float
toNdc s = fromIntegral s / fromIntegral (0x7FFFFFFF :: Int) * 2.0 - 1.0

clampF :: Float -> Float -> Float -> Float
clampF lo hi v
  | v < lo    = lo
  | v > hi    = hi
  | otherwise = v
