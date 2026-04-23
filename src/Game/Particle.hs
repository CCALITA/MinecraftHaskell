module Game.Particle
  ( Particle(..)
  , ParticleSystem
  , newParticleSystem
  , spawnBlockBreakParticles
  , spawnBlockBreakParticlesIO
  , tickParticles
  , renderParticles
  , WeatherParticle(..)
  , WeatherParticleType(..)
  , spawnWeatherParticles
  , tickWeatherParticles
  , renderWeatherParticles
  , weatherParticleRadius
  , weatherParticleHeight
  , weatherParticleCount
  , rainFallSpeed
  , snowFallSpeed
  , isSnowBiome
  , clampParticleXZ
  , clampParticleY
  ) where

import Linear
import Data.IORef
import Control.Monad (replicateM)
import System.Random (randomRIO)
import World.Biome (BiomeType(..))
import World.Block (BlockType(..))
import Game.Item (Item(..))
import Game.ItemDisplay (itemColor)

data Particle = Particle
  { partPos      :: !(V3 Float)
  , partVelocity :: !(V3 Float)
  , partColor    :: !(V4 Float)
  , partLife     :: !Float
  , partMaxLife  :: !Float
  }
type ParticleSystem = IORef [Particle]
newParticleSystem :: IO ParticleSystem
newParticleSystem = newIORef []
-- | Pure function producing 12 block-break particles with the block's actual color.
--   Each tuple: (position, velocity, size, r, g, b)
--   Velocities spread in a hemisphere above the break point.
--   Particle sizes vary between 0.03 and 0.08.
spawnBlockBreakParticles :: BlockType -> V3 Float -> [(V3 Float, V3 Float, Float, Float, Float, Float)]
spawnBlockBreakParticles bt center =
  let (cr, cg, cb, _ca) = itemColor (BlockItem bt)
      -- 12 directions spread over the upper hemisphere using spherical coordinates
      -- 4 rings of 3 particles at varying elevation angles
      angles = [ (phi, theta)
               | phi   <- [0.3, 0.7, 1.1, 1.4]   -- elevation from vertical (radians, <pi/2 = upward)
               , theta <- [0, 2*pi/3, 4*pi/3]     -- azimuth angles
               ]
      mkParticle (i, (phi, theta)) =
        let speed  = 3.0 + 0.5 * sin (fromIntegral i * 1.7)
            vx     = speed * sin phi * cos theta
            vy     = speed * cos phi              -- always positive (hemisphere above)
            vz     = speed * sin phi * sin theta
            -- Offsets from block center
            ox     = 0.15 * cos (theta + fromIntegral i)
            oy     = 0.15 * sin (fromIntegral i * 0.8)
            oz     = 0.15 * sin (theta + fromIntegral i)
            pos    = center + V3 (0.5 + ox) (0.5 + oy) (0.5 + oz)
            vel    = V3 vx vy vz
            -- Size varies 0.03 to 0.08
            size   = 0.03 + 0.05 * abs (sin (fromIntegral i * 2.3))
        in (pos, vel, size, cr, cg, cb)
  in map mkParticle (zip [0 :: Int ..] angles)

-- | IO version: spawns block-break particles into the particle system using the
--   pure spawnBlockBreakParticles result plus random jitter for lifetime.
spawnBlockBreakParticlesIO :: ParticleSystem -> BlockType -> V3 Float -> IO ()
spawnBlockBreakParticlesIO psRef bt center = do
  let pureParticles = spawnBlockBreakParticles bt center
  newParticles <- mapM toParticle pureParticles
  modifyIORef' psRef (newParticles ++)
  where
    toParticle (pos, vel, _sz, cr, cg, cb) = do
      life <- randomRIO (0.4, 0.8)
      pure Particle
        { partPos      = pos
        , partVelocity = vel
        , partColor    = V4 cr cg cb 1.0
        , partLife     = life
        , partMaxLife  = life
        }
tickParticles :: Float -> ParticleSystem -> IO ()
tickParticles dt psRef = modifyIORef' psRef (map step . filter alive)
  where
    gravity = V3 0 (-15) 0; alive p = partLife p > 0
    step p = p { partPos = partPos p + partVelocity p ^* dt, partVelocity = partVelocity p + gravity ^* dt, partLife = partLife p - dt }
renderParticles :: [Particle] -> M44 Float -> [Float]
renderParticles particles vpMat = concatMap renderOne particles
  where
    renderOne p = let V3 wx wy wz = partPos p; V4 cx cy _cz cw = vpMat !* V4 wx wy wz 1
      in if cw > 0.01
         then let ndcX = cx / cw; ndcY = cy / cw
           in if abs ndcX < 1.5 && abs ndcY < 1.5
              then let V4 r g b _a = partColor p; lifeFrac = partLife p / partMaxLife p; alpha = lifeFrac * 0.9; sz = 0.008 * lifeFrac + 0.004; x0 = ndcX - sz; y0 = ndcY - sz; x1 = ndcX + sz; y1 = ndcY + sz
                in [x0,y0,r,g,b,alpha, x1,y0,r,g,b,alpha, x1,y1,r,g,b,alpha, x0,y0,r,g,b,alpha, x1,y1,r,g,b,alpha, x0,y1,r,g,b,alpha]
              else []
         else []
weatherParticleRadius :: Float
weatherParticleRadius = 16.0
weatherParticleHeight :: Float
weatherParticleHeight = 20.0
weatherParticleCount :: Int
weatherParticleCount = 200
rainFallSpeed :: Float
rainFallSpeed = 15.0
snowFallSpeed :: Float
snowFallSpeed = 3.0
isSnowBiome :: BiomeType -> Bool
isSnowBiome Tundra = True
isSnowBiome Taiga  = True
isSnowBiome _      = False
data WeatherParticleType = RainDrop | SnowFlake deriving stock (Show, Eq)
data WeatherParticle = WeatherParticle { wpPos :: !(V3 Float), wpType :: !WeatherParticleType } deriving stock (Show, Eq)
clampParticleXZ :: Float -> Float -> Float
clampParticleXZ radius v | v > radius = v - 2*radius | v < -radius = v + 2*radius | otherwise = v
clampParticleY :: Float -> Float -> Maybe Float
clampParticleY height dy | dy < 0 = Nothing | dy > height = Just (dy - height) | otherwise = Just dy
spawnWeatherParticles :: V3 Float -> BiomeType -> Int -> IO [WeatherParticle]
spawnWeatherParticles (V3 px py pz) biome count = replicateM count $ do
  ang <- randomRIO (0, 2*pi :: Float); rad <- randomRIO (0, weatherParticleRadius)
  let ox = rad * cos ang; oz = rad * sin ang
  oy <- randomRIO (0, weatherParticleHeight)
  pure WeatherParticle { wpPos = V3 (px+ox) (py+oy) (pz+oz), wpType = if isSnowBiome biome then SnowFlake else RainDrop }
tickWeatherParticles :: Float -> V3 Float -> BiomeType -> [WeatherParticle] -> IO [WeatherParticle]
tickWeatherParticles dt (V3 px py pz) biome = mapM $ \wp -> do
  let speed = case wpType wp of { RainDrop -> rainFallSpeed; SnowFlake -> snowFallSpeed }
      V3 wx wy wz = wpPos wp; wy' = wy - speed*dt
      dx = clampParticleXZ weatherParticleRadius (wx-px); dz = clampParticleXZ weatherParticleRadius (wz-pz)
      wp' = wp { wpPos = V3 (px+dx) wy' (pz+dz) }; dy = wy' - py
  case clampParticleY weatherParticleHeight dy of
    Just _ -> pure wp'
    Nothing -> do
      ang <- randomRIO (0, 2*pi :: Float); rad <- randomRIO (0, weatherParticleRadius)
      let ox = rad * cos ang; oz = rad * sin ang
      pure WeatherParticle { wpPos = V3 (px+ox) (py+weatherParticleHeight) (pz+oz), wpType = if isSnowBiome biome then SnowFlake else RainDrop }
renderWeatherParticles :: [WeatherParticle] -> M44 Float -> [Float]
renderWeatherParticles particles vpMat = concatMap renderOne particles
  where
    renderOne wp = let V3 wx wy wz = wpPos wp; V4 cx cy _cz cw = vpMat !* V4 wx wy wz 1
      in if cw > 0.01
         then let ndcX = cx/cw; ndcY = cy/cw
           in if abs ndcX < 1.5 && abs ndcY < 1.5
              then case wpType wp of
                RainDrop -> let r=0.5;g=0.6;b=1.0;a=0.4;hw=0.001;hh=0.012;x0=ndcX-hw;x1=ndcX+hw;y0=ndcY-hh;y1=ndcY+hh in [x0,y0,r,g,b,a,x1,y0,r,g,b,a,x1,y1,r,g,b,a,x0,y0,r,g,b,a,x1,y1,r,g,b,a,x0,y1,r,g,b,a]
                SnowFlake -> let r=1.0;g=1.0;b=1.0;a=0.7;sz=0.004;x0=ndcX-sz;x1=ndcX+sz;y0=ndcY-sz;y1=ndcY+sz in [x0,y0,r,g,b,a,x1,y0,r,g,b,a,x1,y1,r,g,b,a,x0,y0,r,g,b,a,x1,y1,r,g,b,a,x0,y1,r,g,b,a]
              else []
         else []
