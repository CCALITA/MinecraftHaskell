module Engine.SkyGradient
  ( skyGradientColor
  , horizonHaze
  ) where

-- | Horizon haze factor based on elevation angle.
--
-- Returns 1.0 at the horizon (elevation 0) decaying smoothly to 0.0 at zenith
-- (elevation 1). Uses an exponential falloff: exp(-5 * elevation).
horizonHaze :: Float -> Float -> Float
horizonHaze elev ambient =
  let clampedElev = max 0.0 (min 1.0 elev)
      baseFalloff = exp (-5.0 * clampedElev)
      -- ambient dims the haze at night (less atmospheric scattering)
      ambientFactor = 0.3 + 0.7 * (max 0.0 (min 1.0 ambient))
  in baseFalloff * ambientFactor

-- | Compute sky gradient RGBA for a given point on the sky dome.
--
-- @elevation@: 0 = horizon, 1 = zenith (clamped to [0,1])
-- @azimuth@:   0 to 2*pi, angle around the horizon (currently unused but
--              reserved for sun-glow directional tinting)
-- @ambient@:   0 = full night, 1 = full day (clamped to [0,1])
--
-- The color blends between a night sky and a day sky based on @ambient@,
-- then mixes in a warm horizon haze band near the horizon.
skyGradientColor :: Float -> Float -> Float -> (Float, Float, Float, Float)
skyGradientColor elevation azimuth ambient =
  let elev = max 0.0 (min 1.0 elevation)
      amb  = max 0.0 (min 1.0 ambient)
      _az  = azimuth  -- reserved for future sun-direction tinting

      -- Night sky base (dark blue)
      nightR = 0.01
      nightG = 0.01
      nightB = 0.05

      -- Day sky zenith (bright blue)
      dayZenithR = 0.40
      dayZenithG = 0.65
      dayZenithB = 0.95

      -- Day sky horizon (lighter, slightly desaturated)
      dayHorizonR = 0.65
      dayHorizonG = 0.78
      dayHorizonB = 0.92

      -- Blend day zenith → day horizon by elevation (lower = more horizon color)
      elevFactor = 1.0 - elev  -- 1 at horizon, 0 at zenith
      dayR = dayZenithR + elevFactor * (dayHorizonR - dayZenithR)
      dayG = dayZenithG + elevFactor * (dayHorizonG - dayZenithG)
      dayB = dayZenithB + elevFactor * (dayHorizonB - dayZenithB)

      -- Blend night → day by ambient
      baseR = nightR + amb * (dayR - nightR)
      baseG = nightG + amb * (dayG - nightG)
      baseB = nightB + amb * (dayB - nightB)

      -- Horizon haze: warm orange-white band near the horizon
      haze = horizonHaze elev amb
      hazeR = 0.85
      hazeG = 0.70
      hazeB = 0.55

      finalR = baseR + haze * (hazeR - baseR) * 0.5
      finalG = baseG + haze * (hazeG - baseG) * 0.5
      finalB = baseB + haze * (hazeB - baseB) * 0.5

  in (finalR, finalG, finalB, 1.0)
