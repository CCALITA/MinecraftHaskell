module World.Noise
  ( Seed
  , noise2D
  , noise3D
  , fractalNoise2D
  , fractalNoise3D
  , ridgedNoise2D
  ) where

import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word64)

-- | Seed for noise generation
type Seed = Int

-- | Permutation table (256 entries, doubled for overflow)
permTable :: Seed -> Int -> Int
permTable seed i =
  let h = hash (seed + i)
  in h .&. 255
{-# INLINE permTable #-}

-- | Simple integer hash
hash :: Int -> Int
hash x0 =
  let x1 = ((x0 `xor` (x0 `shiftR` 16)) * 0x45d9f3b) .&. 0x7fffffff
      x2 = ((x1 `xor` (x1 `shiftR` 16)) * 0x45d9f3b) .&. 0x7fffffff
  in x2 `xor` (x2 `shiftR` 16)
{-# INLINE hash #-}

-- | Gradient vectors for 2D (8 directions)
grad2 :: Int -> Double -> Double -> Double
grad2 h x y =
  case h .&. 7 of
    0 ->  x + y
    1 -> -x + y
    2 ->  x - y
    3 -> -x - y
    4 ->  x + y * 0.5
    5 -> -x + y * 0.5
    6 ->  x - y * 0.5
    _ -> -x - y * 0.5
{-# INLINE grad2 #-}

-- | Gradient vectors for 3D (12 directions)
grad3 :: Int -> Double -> Double -> Double -> Double
grad3 h x y z =
  case h .&. 11 of
    0  ->  x + y
    1  -> -x + y
    2  ->  x - y
    3  -> -x - y
    4  ->  x + z
    5  -> -x + z
    6  ->  x - z
    7  -> -x - z
    8  ->  y + z
    9  -> -y + z
    10 ->  y - z
    _  -> -y - z
{-# INLINE grad3 #-}

-- | Smooth interpolation (improved Perlin fade curve: 6t^5 - 15t^4 + 10t^3)
fade :: Double -> Double
fade t = t * t * t * (t * (t * 6 - 15) + 10)
{-# INLINE fade #-}

-- | Linear interpolation
lerp :: Double -> Double -> Double -> Double
lerp t a b = a + t * (b - a)
{-# INLINE lerp #-}

-- | 2D Perlin noise. Returns value in [-1, 1].
noise2D :: Seed -> Double -> Double -> Double
noise2D seed x y =
  let xi = floor x :: Int
      yi = floor y :: Int
      xf = x - fromIntegral xi
      yf = y - fromIntegral yi
      u  = fade xf
      v  = fade yf
      p i = permTable seed i
      aa = p (p xi + yi)
      ab = p (p xi + yi + 1)
      ba = p (p (xi + 1) + yi)
      bb = p (p (xi + 1) + yi + 1)
  in lerp v
       (lerp u (grad2 aa xf       yf      ) (grad2 ba (xf - 1) yf      ))
       (lerp u (grad2 ab xf       (yf - 1)) (grad2 bb (xf - 1) (yf - 1)))
{-# INLINE noise2D #-}

-- | 3D Perlin noise. Returns value in [-1, 1].
noise3D :: Seed -> Double -> Double -> Double -> Double
noise3D seed x y z =
  let xi = floor x :: Int
      yi = floor y :: Int
      zi = floor z :: Int
      xf = x - fromIntegral xi
      yf = y - fromIntegral yi
      zf = z - fromIntegral zi
      u  = fade xf
      v  = fade yf
      w  = fade zf
      p i = permTable seed i
      aaa = p (p (p xi + yi) + zi)
      aba = p (p (p xi + yi + 1) + zi)
      aab = p (p (p xi + yi) + zi + 1)
      abb = p (p (p xi + yi + 1) + zi + 1)
      baa = p (p (p (xi+1) + yi) + zi)
      bba = p (p (p (xi+1) + yi + 1) + zi)
      bab = p (p (p (xi+1) + yi) + zi + 1)
      bbb = p (p (p (xi+1) + yi + 1) + zi + 1)
  in lerp w
       (lerp v
         (lerp u (grad3 aaa xf       yf       zf      ) (grad3 baa (xf-1) yf       zf      ))
         (lerp u (grad3 aba xf       (yf-1)   zf      ) (grad3 bba (xf-1) (yf-1)   zf      )))
       (lerp v
         (lerp u (grad3 aab xf       yf       (zf-1)  ) (grad3 bab (xf-1) yf       (zf-1)  ))
         (lerp u (grad3 abb xf       (yf-1)   (zf-1)  ) (grad3 bbb (xf-1) (yf-1)   (zf-1)  )))

-- | Fractal Brownian Motion (fBM) 2D noise.
--   Sums multiple octaves of noise at increasing frequency and decreasing amplitude.
fractalNoise2D
  :: Seed
  -> Int       -- ^ octaves
  -> Double    -- ^ lacunarity (frequency multiplier per octave, typically 2.0)
  -> Double    -- ^ persistence (amplitude multiplier per octave, typically 0.5)
  -> Double    -- ^ x
  -> Double    -- ^ y
  -> Double
fractalNoise2D seed octaves lacunarity persistence x y = go 0 1.0 1.0 0 0
  where
    go i freq amp total maxAmp
      | i >= octaves = total / maxAmp  -- normalize to [-1, 1]
      | otherwise =
          let n = noise2D (seed + i * 31) (x * freq) (y * freq)
          in go (i + 1) (freq * lacunarity) (amp * persistence)
                (total + n * amp) (maxAmp + amp)

-- | Fractal Brownian Motion 3D noise.
fractalNoise3D
  :: Seed -> Int -> Double -> Double
  -> Double -> Double -> Double
  -> Double
fractalNoise3D seed octaves lacunarity persistence x y z = go 0 1.0 1.0 0 0
  where
    go i freq amp total maxAmp
      | i >= octaves = total / maxAmp
      | otherwise =
          let n = noise3D (seed + i * 31) (x * freq) (y * freq) (z * freq)
          in go (i + 1) (freq * lacunarity) (amp * persistence)
                (total + n * amp) (maxAmp + amp)

-- | Ridged multifractal noise 2D (good for mountain ridges).
--   Takes absolute value of noise and inverts it.
ridgedNoise2D :: Seed -> Int -> Double -> Double -> Double -> Double -> Double
ridgedNoise2D seed octaves lacunarity persistence x y = go 0 1.0 1.0 0 0
  where
    go i freq amp total maxAmp
      | i >= octaves = total / maxAmp
      | otherwise =
          let n = 1.0 - abs (noise2D (seed + i * 37) (x * freq) (y * freq))
          in go (i + 1) (freq * lacunarity) (amp * persistence)
                (total + n * amp) (maxAmp + amp)
