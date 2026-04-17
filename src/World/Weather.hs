module World.Weather
  ( WeatherState(..)
  , WeatherType(..)
  , newWeatherState
  , updateWeather
  , isRaining
  , weatherSkyMultiplier
  , weatherAmbientMultiplier
  ) where

import System.Random (randomRIO)

-- | Weather types
data WeatherType = Clear | Rain deriving stock (Show, Eq)

-- | Weather state tracking current type, timer until transition, and smooth intensity
data WeatherState = WeatherState
  { wsType      :: !WeatherType
  , wsTimer     :: !Float   -- seconds until next transition
  , wsIntensity :: !Float   -- 0.0 to 1.0, for smooth transitions
  } deriving stock (Show, Eq)

-- | Initial weather state: clear skies, 5 minutes until possible rain
newWeatherState :: WeatherState
newWeatherState = WeatherState Clear 300.0 0.0

-- | Update weather by elapsed seconds.
--   Decrements timer; on expiry, transitions between Clear and Rain.
--   Smoothly ramps intensity toward target (1.0 for Rain, 0.0 for Clear).
updateWeather :: Float -> WeatherState -> IO WeatherState
updateWeather dt ws = do
  let newTimer = wsTimer ws - dt
      -- Smooth intensity transition at 0.5 per second
      targetIntensity = case wsType ws of
        Rain  -> 1.0
        Clear -> 0.0
      step = dt * 0.5
      newIntensity
        | wsIntensity ws < targetIntensity = min targetIntensity (wsIntensity ws + step)
        | wsIntensity ws > targetIntensity = max targetIntensity (wsIntensity ws - step)
        | otherwise                        = wsIntensity ws
  if newTimer <= 0
    then case wsType ws of
      Clear -> do
        timer <- randomRIO (180.0, 300.0 :: Float)
        pure $ WeatherState Rain timer newIntensity
      Rain -> do
        timer <- randomRIO (300.0, 600.0 :: Float)
        pure $ WeatherState Clear timer newIntensity
    else pure ws { wsTimer = newTimer, wsIntensity = newIntensity }

-- | Check if it is currently raining
isRaining :: WeatherState -> Bool
isRaining ws = wsType ws == Rain

-- | Sky color multiplier: darkens sky by up to 30% during rain
weatherSkyMultiplier :: WeatherState -> Float
weatherSkyMultiplier ws = 1.0 - 0.3 * wsIntensity ws

-- | Ambient light multiplier: reduces light by up to 20% during rain
weatherAmbientMultiplier :: WeatherState -> Float
weatherAmbientMultiplier ws = 1.0 - 0.2 * wsIntensity ws
