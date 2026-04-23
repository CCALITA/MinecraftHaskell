module Game.DayNight
  ( TimeOfDay(..)
  , DayNightCycle(..)
  , newDayNightCycle
  , updateDayNight
  , getSunAngle
  , getSkyColor
  , getAmbientLight
  , getSunDirection
  , getTimeOfDay
  , isNight
  , isDawn
  , isDusk
  -- Constants
  , dayLengthSeconds
  , ticksPerDay
  ) where

import Linear (V3(..), V4(..), normalize)

-- | Full day length in seconds (20 minutes like Minecraft)
dayLengthSeconds :: Float
dayLengthSeconds = 1200.0

-- | Ticks per day (20 tps * 1200s = 24000)
ticksPerDay :: Int
ticksPerDay = 24000

-- | Phases of day for gameplay
data TimeOfDay
  = Dawn       -- 0.20 - 0.30
  | Day        -- 0.30 - 0.70
  | Dusk       -- 0.70 - 0.80
  | Night      -- 0.80 - 1.00, 0.00 - 0.20
  deriving stock (Show, Eq)

-- | Day/night cycle state
data DayNightCycle = DayNightCycle
  { dncTime      :: !Float    -- 0.0 to 1.0 (0 = midnight, 0.25 = dawn, 0.5 = noon, 0.75 = dusk)
  , dncDayCount  :: !Int      -- number of full days elapsed
  , dncSpeed     :: !Float    -- time multiplier (1.0 = normal)
  } deriving stock (Show, Eq)

-- | Create a new day/night cycle starting at dawn
newDayNightCycle :: DayNightCycle
newDayNightCycle = DayNightCycle
  { dncTime     = 0.25   -- start at dawn
  , dncDayCount = 0
  , dncSpeed    = 1.0
  }

-- | Update cycle by elapsed real seconds
updateDayNight :: Float -> DayNightCycle -> DayNightCycle
updateDayNight dt cycle =
  let advance = (dt * dncSpeed cycle) / dayLengthSeconds
      newTime = dncTime cycle + advance
      (days, fracTime) = properFraction newTime
  in cycle
    { dncTime     = fracTime
    , dncDayCount = dncDayCount cycle + days
    }

-- | Get current phase of day
getTimeOfDay :: DayNightCycle -> TimeOfDay
getTimeOfDay cycle
  | t >= 0.20 && t < 0.30 = Dawn
  | t >= 0.30 && t < 0.70 = Day
  | t >= 0.70 && t < 0.80 = Dusk
  | otherwise              = Night
  where t = dncTime cycle

isNight :: DayNightCycle -> Bool
isNight c = getTimeOfDay c == Night

isDawn :: DayNightCycle -> Bool
isDawn c = getTimeOfDay c == Dawn

isDusk :: DayNightCycle -> Bool
isDusk c = getTimeOfDay c == Dusk

-- | Sun angle in radians (0 = horizon east, pi/2 = zenith, pi = horizon west)
getSunAngle :: DayNightCycle -> Float
getSunAngle cycle = (dncTime cycle - 0.25) * 2.0 * pi

-- | Sun direction vector (normalized, in world space: +Y is up)
getSunDirection :: DayNightCycle -> V3 Float
getSunDirection cycle =
  let angle = getSunAngle cycle
  in normalize $ V3 (cos angle) (sin angle) 0.2

-- | Sky color based on time of day (RGBA)
--
-- Dawn and dusk use multi-step interpolation through warm horizon colors
-- for dramatic sunrise/sunset effects.
getSkyColor :: DayNightCycle -> V4 Float
getSkyColor cycle = case getTimeOfDay cycle of
  Day   -> lerp4 dayProgress daySkyStart daySkyPeak
  Night -> nightSky
  Dawn
    | t < 0.24  -> lerp4 ((t - 0.20) / 0.04) nightSky     dawnHorizon
    | t < 0.27  -> lerp4 ((t - 0.24) / 0.03) dawnHorizon   dawnGold
    | otherwise -> lerp4 ((t - 0.27) / 0.03) dawnGold      daySkyStart
  Dusk
    | t < 0.73  -> lerp4 ((t - 0.70) / 0.03) daySkyStart   duskGolden
    | t < 0.77  -> lerp4 ((t - 0.73) / 0.04) duskGolden    duskDeepOrange
    | otherwise -> lerp4 ((t - 0.77) / 0.03) duskDeepOrange nightSky
  where
    t = dncTime cycle

    -- Progress within day phase (0.0 to 1.0)
    dayProgress  = (t - 0.30) / 0.40

    -- Sky colors
    nightSky       = V4 0.01 0.01 0.05 1.0
    daySkyStart    = V4 0.40 0.60 0.85 1.0
    daySkyPeak     = V4 0.53 0.81 0.92 1.0

    -- Dawn transition colors
    dawnHorizon    = V4 0.70 0.40 0.30 1.0   -- warm orange-pink
    dawnGold       = V4 0.80 0.70 0.50 1.0   -- pale gold

    -- Dusk transition colors
    duskGolden     = V4 0.80 0.60 0.30 1.0   -- golden
    duskDeepOrange = V4 0.60 0.25 0.15 1.0   -- deep orange-red

-- | Ambient light level (0.0 to 1.0)
getAmbientLight :: DayNightCycle -> Float
getAmbientLight cycle = case getTimeOfDay cycle of
  Day   -> 1.0
  Night -> 0.15
  Dawn  -> lerp1 ((dncTime cycle - 0.20) / 0.10) 0.15 1.0
  Dusk  -> lerp1 ((dncTime cycle - 0.70) / 0.10) 1.0 0.15

-- | Linear interpolation helpers
lerp1 :: Float -> Float -> Float -> Float
lerp1 t a b = a + t * (b - a)

lerp4 :: Float -> V4 Float -> V4 Float -> V4 Float
lerp4 t (V4 r1 g1 b1 a1) (V4 r2 g2 b2 a2) =
  V4 (lerp1 t r1 r2) (lerp1 t g1 g2) (lerp1 t b1 b2) (lerp1 t a1 a2)
