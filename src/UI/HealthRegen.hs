module UI.HealthRegen
  ( regenHeartColor
  , isRegenerating
  ) where

-- | Determine whether the player is regenerating health.
-- In Minecraft, natural health regeneration occurs when hunger >= 18
-- and current health is below the maximum.
isRegenerating :: Int -> Int -> Bool
isRegenerating hunger health = hunger >= 18 && health < 20

-- | Compute a pulsing pink color for a regenerating heart.
-- Takes the current time (seconds) and heart index, and returns
-- an RGBA tuple that oscillates between base pink and bright pink.
-- Each heart pulses at a slightly offset phase based on its index,
-- creating a wave-like animation across the health bar.
regenHeartColor :: Float -> Int -> (Float, Float, Float, Float)
regenHeartColor time idx = (r, g, b, a)
  where
    phase  = time * 4.0 + fromIntegral idx * 0.5
    pulse  = (sin phase + 1.0) * 0.5  -- normalised to [0, 1]
    r      = 1.0
    g      = 0.3 + pulse * 0.4        -- [0.3, 0.7]
    b      = 0.4 + pulse * 0.4        -- [0.4, 0.8]
    a      = 0.8 + pulse * 0.2        -- [0.8, 1.0]
