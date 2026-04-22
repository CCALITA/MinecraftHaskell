module Game.Particle
  ( Particle(..)
  , ParticleSystem
  , PrecipitationType(..)
  , newParticleSystem
  , spawnBlockBreakParticles
  , tickParticles
  , renderParticles
  , precipitationForBiome
  , spawnRainParticle
  , spawnSnowParticle
  , spawnPrecipitationBatch
  , tickPrecipitation
  , renderPrecipitation
  , precipitationColor
  , precipitationSpeed
  , rainSpeed
  , snowSpeed
  , precipCylinderRadius
  , precipCylinderHeight
  , maxPrecipParticles
  ) where

import Linear
import Data.IORef
import Control.Monad (replicateM)
import System.Random (randomRIO)

data Particle = Particle
  { partPos      :: !(V3 Float)
  , partVelocity :: !(V3 Float)
  , partColor    :: !(V4 Float)
  , partLife     :: !Float
  , partMaxLife  :: !Float
  } deriving stock (Show, Eq)

type ParticleSystem = IORef [Particle]

newParticleSystem :: IO ParticleSystem
newParticleSystem = newIORef []

spawnBlockBreakParticles :: ParticleSystem -> V3 Float -> (Float, Float, Float, Float) -> IO ()
spawnBlockBreakParticles psRef (V3 cx cy cz) (cr, cg, cb, ca) = do
  count <- randomRIO (8 :: Int, 12)
  newParticles <- replicateM count makeParticle
  modifyIORef' psRef (newParticles ++)
  where
    makeParticle = do
      vx <- randomRIO (-3.0, 3.0)
      vy <- randomRIO (2.0, 6.0)
      vz <- randomRIO (-3.0, 3.0)
      ox <- randomRIO (-0.3, 0.3)
      oy <- randomRIO (-0.3, 0.3)
      oz <- randomRIO (-0.3, 0.3)
      life <- randomRIO (0.4, 0.8)
      pure Particle
        { partPos      = V3 (cx + 0.5 + ox) (cy + 0.5 + oy) (cz + 0.5 + oz)
        , partVelocity = V3 vx vy vz
        , partColor    = V4 cr cg cb ca
        , partLife     = life
        , partMaxLife  = life
        }

tickParticles :: Float -> ParticleSystem -> IO ()
tickParticles dt psRef = modifyIORef' psRef (map step . filter alive)
  where
    gravity = V3 0 (-15) 0
    alive p = partLife p > 0
    step p = p
      { partPos      = partPos p + partVelocity p ^* dt
      , partVelocity = partVelocity p + gravity ^* dt
      , partLife     = partLife p - dt
      }

renderParticles :: [Particle] -> M44 Float -> [Float]
renderParticles particles vpMat = concatMap renderOne particles
  where
    renderOne p =
      let V3 wx wy wz = partPos p
          V4 cx cy _cz cw = vpMat !* V4 wx wy wz 1
      in if cw > 0.01
         then
           let ndcX = cx / cw
               ndcY = cy / cw
           in if abs ndcX < 1.5 && abs ndcY < 1.5
              then
                let V4 r g b _a = partColor p
                    lifeFrac = partLife p / partMaxLife p
                    alpha = lifeFrac * 0.9
                    sz = 0.008 * lifeFrac + 0.004
                    x0 = ndcX - sz
                    y0 = ndcY - sz
                    x1 = ndcX + sz
                    y1 = ndcY + sz
                in [ x0, y0, r, g, b, alpha
                   , x1, y0, r, g, b, alpha
                   , x1, y1, r, g, b, alpha
                   , x0, y0, r, g, b, alpha
                   , x1, y1, r, g, b, alpha
                   , x0, y1, r, g, b, alpha
                   ]
              else []
         else []

data PrecipitationType = PrecipRain | PrecipSnow
  deriving stock (Show, Eq, Ord)

precipCylinderRadius :: Float
precipCylinderRadius = 16.0

precipCylinderHeight :: Float
precipCylinderHeight = 20.0

maxPrecipParticles :: Int
maxPrecipParticles = 200

rainSpeed :: Float
rainSpeed = 15.0

snowSpeed :: Float
snowSpeed = 3.0

precipitationForBiome :: String -> Maybe PrecipitationType
precipitationForBiome biomeName
  | "Tundra" == biomeName = Just PrecipSnow
  | "Taiga"  == biomeName = Just PrecipSnow
  | otherwise             = Just PrecipRain

precipitationColor :: PrecipitationType -> V4 Float
precipitationColor PrecipRain = V4 0.4 0.5 0.9 0.6
precipitationColor PrecipSnow = V4 0.95 0.95 1.0 0.7

precipitationSpeed :: PrecipitationType -> Float
precipitationSpeed PrecipRain = rainSpeed
precipitationSpeed PrecipSnow = snowSpeed

spawnRainParticle :: V3 Float -> IO Particle
spawnRainParticle (V3 px py pz) = do
  angle <- randomRIO (0.0, 2.0 * pi :: Float)
  dist  <- randomRIO (0.0, precipCylinderRadius)
  let ox = dist * cos angle
      oz = dist * sin angle
  oy <- randomRIO (5.0, precipCylinderHeight)
  let startPos = V3 (px + ox) (py + oy) (pz + oz)
      vel      = V3 0 (negate rainSpeed) 0
      color    = precipitationColor PrecipRain
      life     = (oy + 5.0) / rainSpeed
  pure Particle
    { partPos      = startPos
    , partVelocity = vel
    , partColor    = color
    , partLife     = life
    , partMaxLife  = life
    }

spawnSnowParticle :: V3 Float -> IO Particle
spawnSnowParticle (V3 px py pz) = do
  angle <- randomRIO (0.0, 2.0 * pi :: Float)
  dist  <- randomRIO (0.0, precipCylinderRadius)
  let ox = dist * cos angle
      oz = dist * sin angle
  oy <- randomRIO (5.0, precipCylinderHeight)
  driftX <- randomRIO (-0.5, 0.5 :: Float)
  driftZ <- randomRIO (-0.5, 0.5 :: Float)
  let startPos = V3 (px + ox) (py + oy) (pz + oz)
      vel      = V3 driftX (negate snowSpeed) driftZ
      color    = precipitationColor PrecipSnow
      life     = (oy + 5.0) / snowSpeed
  pure Particle
    { partPos      = startPos
    , partVelocity = vel
    , partColor    = color
    , partLife     = life
    , partMaxLife  = life
    }

spawnPrecipitationBatch :: Int -> PrecipitationType -> V3 Float -> IO [Particle]
spawnPrecipitationBatch currentCount ptype playerPos = do
  let deficit = max 0 (maxPrecipParticles - currentCount)
      toSpawn = min 20 deficit
  case ptype of
    PrecipRain -> replicateM toSpawn (spawnRainParticle playerPos)
    PrecipSnow -> replicateM toSpawn (spawnSnowParticle playerPos)

tickPrecipitation :: Float -> [Particle] -> [Particle]
tickPrecipitation dt = map step . filter alive
  where
    alive p = partLife p > 0
    step p = p
      { partPos  = partPos p + partVelocity p ^* dt
      , partLife = partLife p - dt
      }

renderPrecipitation :: [Particle] -> PrecipitationType -> M44 Float -> [Float]
renderPrecipitation particles ptype vpMat = concatMap renderOne particles
  where
    renderOne p =
      let V3 wx wy wz = partPos p
          V4 cx cy _cz cw = vpMat !* V4 wx wy wz 1
      in if cw > 0.01
         then
           let ndcX = cx / cw
               ndcY = cy / cw
           in if abs ndcX < 1.2 && abs ndcY < 1.2
              then
                let V4 r g b a = partColor p
                    lifeFrac = partLife p / partMaxLife p
                    alpha = a * min 1.0 (lifeFrac * 2.0)
                    (hw, hh) = case ptype of
                      PrecipRain -> (0.001, 0.015)
                      PrecipSnow -> (0.004, 0.004)
                    x0 = ndcX - hw
                    y0 = ndcY - hh
                    x1 = ndcX + hw
                    y1 = ndcY + hh
                in [ x0, y0, r, g, b, alpha
                   , x1, y0, r, g, b, alpha
                   , x1, y1, r, g, b, alpha
                   , x0, y0, r, g, b, alpha
                   , x1, y1, r, g, b, alpha
                   , x0, y1, r, g, b, alpha
                   ]
              else []
         else []
