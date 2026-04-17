module Game.Particle
  ( Particle(..)
  , ParticleSystem
  , newParticleSystem
  , spawnBlockBreakParticles
  , tickParticles
  , renderParticles
  ) where

import Linear
import Data.IORef
import Control.Monad (replicateM)
import System.Random (randomRIO)

-- | A single visual particle with position, velocity, color, and lifetime
data Particle = Particle
  { partPos      :: !(V3 Float)
  , partVelocity :: !(V3 Float)
  , partColor    :: !(V4 Float)  -- RGBA
  , partLife     :: !Float       -- seconds remaining
  , partMaxLife  :: !Float       -- for fade calculation
  }

-- | Mutable container for active particles
type ParticleSystem = IORef [Particle]

-- | Create an empty particle system
newParticleSystem :: IO ParticleSystem
newParticleSystem = newIORef []

-- | Spawn 8-12 particles at the center of a broken block
--   The color tuple (r,g,b,a) matches the itemColor format used elsewhere.
spawnBlockBreakParticles :: ParticleSystem -> V3 Float -> (Float, Float, Float, Float) -> IO ()
spawnBlockBreakParticles psRef (V3 cx cy cz) (cr, cg, cb, ca) = do
  count <- randomRIO (8 :: Int, 12)
  newParticles <- replicateM count makeParticle
  modifyIORef' psRef (newParticles ++)
  where
    makeParticle = do
      vx <- randomRIO (-3.0, 3.0)
      vy <- randomRIO (2.0, 6.0)   -- biased upward
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

-- | Advance all particles by dt seconds.
--   Applies gravity, updates positions, removes dead particles.
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

-- | Project live particles to 2D NDC and produce HUD vertex data.
--   Each particle is rendered as a small quad. Alpha fades with remaining life.
--   Returns a flat [Float] with 6 floats per vertex (x, y, r, g, b, a), 6 vertices per quad.
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
               -- Only draw if on-screen
           in if abs ndcX < 1.5 && abs ndcY < 1.5
              then
                let V4 r g b _a = partColor p
                    lifeFrac = partLife p / partMaxLife p
                    alpha = lifeFrac * 0.9  -- fade out as life decreases
                    sz = 0.008 * lifeFrac + 0.004  -- shrink over time
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
