module Engine.ColorGrade
  ( contrastAdjust
  , saturationAdjust
  , brightnessAdjust
  ) where

-- | Adjust contrast of a value around the midpoint 0.5.
--   @contrastAdjust factor value@ scales the distance from 0.5
--   by @factor@ and clamps the result to [0, 1].
contrastAdjust :: Float -> Float -> Float
contrastAdjust factor value =
  let adjusted = 0.5 + (value - 0.5) * factor
  in clamp01 adjusted

-- | Adjust saturation of an RGB triple.
--   @saturationAdjust (r, g, b) factor@ lerps each channel toward
--   the luminance (Rec. 709) by @(1 - factor)@.
--   factor = 0 gives greyscale, factor = 1 leaves color unchanged.
saturationAdjust :: (Float, Float, Float) -> Float -> (Float, Float, Float)
saturationAdjust (r, g, b) factor =
  let lum = luminance r g b
      r'  = lum + (r - lum) * factor
      g'  = lum + (g - lum) * factor
      b'  = lum + (b - lum) * factor
  in (clamp01 r', clamp01 g', clamp01 b')

-- | Adjust brightness by adding an offset and clamping to [0, 1].
brightnessAdjust :: Float -> Float -> Float
brightnessAdjust offset value =
  clamp01 (value + offset)

-- | Rec. 709 luminance from linear RGB.
luminance :: Float -> Float -> Float -> Float
luminance r g b = 0.2126 * r + 0.7152 * g + 0.0722 * b

-- | Clamp a value to [0, 1].
clamp01 :: Float -> Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise  = x
