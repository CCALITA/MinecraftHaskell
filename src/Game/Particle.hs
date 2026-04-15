module Game.Particle
  ( Particle(..)
  , ParticleSystem
  , newParticleSystem
  , spawnBlockBreakParticles
  , updateParticles
  , renderParticles
  ) where

import Linear
import Data.IORef
import qualified System.Random as R

-- | A single particle in world space
data Particle = Particle
  { pPos      :: !(V3 Float)   -- ^ World position
  , pVel      :: !(V3 Float)   -- ^ Velocity
  , pColor    :: !(V4 Float)   -- ^ RGBA color
  , pLife     :: !Float         -- ^ Remaining lifetime (seconds)
  , pMaxLife  :: !Float         -- ^ Initial lifetime (for alpha fade)
  , pSize     :: !Float         -- ^ Particle size in NDC
  }

-- | Mutable particle system
type ParticleSystem = IORef [Particle]

newParticleSystem :: IO ParticleSystem
newParticleSystem = newIORef []

-- | Spawn particles at a block position with given color
spawnBlockBreakParticles :: ParticleSystem -> V3 Float -> (Float, Float, Float, Float) -> IO ()
spawnBlockBreakParticles psRef (V3 bx by bz) (cr, cg, cb, ca) = do
  gen <- R.newStdGen
  let center = V3 (bx + 0.5) (by + 0.5) (bz + 0.5)
      -- Generate 8 particles with random velocities
      particles = take 8 $ mkParticles gen center
  modifyIORef' psRef (particles ++)
  where
    mkParticles g pos =
      let (vx, g1) = R.uniformR (-1.5, 1.5) g
          (vy, g2) = R.uniformR (1.0, 4.0) g1
          (vz, g3) = R.uniformR (-1.5, 1.5) g2
          (ox, g4) = R.uniformR (-0.3, 0.3) g3
          (oy, g5) = R.uniformR (-0.3, 0.3) g4
          (oz, g6) = R.uniformR (-0.3, 0.3) g5
          (life, g7) = R.uniformR (0.3, 0.7) g6
          -- Slight color variation
          (dr, g8) = R.uniformR (-0.1, 0.1) g7
          (dg, g9) = R.uniformR (-0.1, 0.1) g8
          (db, g10) = R.uniformR (-0.1, 0.1) g9
          p = Particle
            { pPos     = pos ^+^ V3 ox oy oz
            , pVel     = V3 vx vy vz
            , pColor   = V4 (clamp01 (cr + dr)) (clamp01 (cg + dg)) (clamp01 (cb + db)) ca
            , pLife    = life
            , pMaxLife = life
            , pSize    = 0.015
            }
      in p : mkParticles g10 pos

    clamp01 x = max 0 (min 1 x)

-- | Update particles: apply gravity, decay lifetime, remove dead
updateParticles :: ParticleSystem -> Float -> IO ()
updateParticles psRef dt = modifyIORef' psRef (filter alive . map step)
  where
    gravity = V3 0 (-9.8) 0
    step p = p
      { pPos  = pPos p ^+^ (pVel p ^* dt)
      , pVel  = pVel p ^+^ (gravity ^* dt)
      , pLife = pLife p - dt
      }
    alive p = pLife p > 0

-- | Render particles to HUD vertex data.
--   Takes the combined view-projection matrix to project 3D -> NDC.
--   Returns list of floats (x, y, r, g, b, a per vertex, 6 verts per quad).
renderParticles :: ParticleSystem -> M44 Float -> IO [Float]
renderParticles psRef vp = do
  particles <- readIORef psRef
  pure $ concatMap (renderOne vp) particles

renderOne :: M44 Float -> Particle -> [Float]
renderOne vp p =
  let V3 wx wy wz = pPos p
      -- Project world position to clip space
      clipPos = vp !* V4 wx wy wz 1.0
      V4 cx cy cz cw = clipPos
  in if cw <= 0.01
     then []  -- Behind camera
     else
       let -- Perspective divide -> NDC
           ndcX = cx / cw
           ndcY = cy / cw
           ndcZ = cz / cw
           -- Cull if outside NDC cube
       in if abs ndcX > 1.2 || abs ndcY > 1.2 || ndcZ < 0 || ndcZ > 1
          then []
          else
            let -- Alpha fade based on remaining life
                alpha = pLife p / pMaxLife p
                V4 r g b a = pColor p
                a' = a * alpha
                s = pSize p
                -- Quad corners
                x0 = ndcX - s
                y0 = ndcY - s
                x1 = ndcX + s
                y1 = ndcY + s
            in quad x0 y0 x1 y1 r g b a'

-- | Generate a colored quad (2 triangles, 6 vertices, 6 floats each)
quad :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> [Float]
quad x0 y0 x1 y1 r g b a =
  [ x0, y0, r, g, b, a
  , x1, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y0, r, g, b, a
  , x1, y1, r, g, b, a
  , x0, y1, r, g, b, a
  ]
