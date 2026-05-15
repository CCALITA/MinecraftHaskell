module Engine.FrameProfile
  ( FrameProfile(..)
  , emptyProfile
  , addSample
  , averageProfile
  , formatProfile
  ) where

import Text.Printf (printf)

-- | Per-frame timing breakdown for the main subsystems.
data FrameProfile = FrameProfile
  { fpPhysicsTime :: Float
  , fpMeshTime    :: Float
  , fpRenderTime  :: Float
  , fpHudTime     :: Float
  } deriving stock (Show, Eq)

-- | A profile with all timings at zero.
emptyProfile :: FrameProfile
emptyProfile = FrameProfile 0 0 0 0

-- | Accumulate two profiles by summing corresponding fields.
addSample :: FrameProfile -> FrameProfile -> FrameProfile
addSample a b = FrameProfile
  { fpPhysicsTime = fpPhysicsTime a + fpPhysicsTime b
  , fpMeshTime    = fpMeshTime a + fpMeshTime b
  , fpRenderTime  = fpRenderTime a + fpRenderTime b
  , fpHudTime     = fpHudTime a + fpHudTime b
  }

-- | Average a list of profiles.  Returns 'emptyProfile' for an empty list.
averageProfile :: [FrameProfile] -> FrameProfile
averageProfile [] = emptyProfile
averageProfile ps =
  let n   = fromIntegral (length ps) :: Float
      acc = foldr addSample emptyProfile ps
  in FrameProfile
       { fpPhysicsTime = fpPhysicsTime acc / n
       , fpMeshTime    = fpMeshTime acc / n
       , fpRenderTime  = fpRenderTime acc / n
       , fpHudTime     = fpHudTime acc / n
       }

-- | Human-readable summary, e.g.
--
-- @phys 1.20ms | mesh 0.80ms | render 4.50ms | hud 0.30ms@
formatProfile :: FrameProfile -> String
formatProfile fp = printf "phys %.2fms | mesh %.2fms | render %.2fms | hud %.2fms"
  (fpPhysicsTime fp)
  (fpMeshTime fp)
  (fpRenderTime fp)
  (fpHudTime fp)
