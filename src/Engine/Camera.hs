module Engine.Camera
  ( Camera(..)
  , defaultCamera
  , cameraViewMatrix
  , cameraProjectionMatrix
  , updateCamera
  ) where

import Linear

-- | FPS camera state
data Camera = Camera
  { camPosition :: !(V3 Float)
  , camFront    :: !(V3 Float)
  , camUp       :: !(V3 Float)
  , camYaw      :: !Float    -- degrees
  , camPitch    :: !Float    -- degrees
  , camFov      :: !Float    -- degrees
  , camSpeed    :: !Float
  , camSensitivity :: !Float
  } deriving stock (Show, Eq)

defaultCamera :: Camera
defaultCamera = Camera
  { camPosition    = V3 0 0 3
  , camFront       = V3 0 0 (-1)
  , camUp          = V3 0 1 0
  , camYaw         = -90
  , camPitch       = 0
  , camFov         = 45
  , camSpeed       = 5.0
  , camSensitivity = 0.1
  }

-- | Compute view matrix from camera state
cameraViewMatrix :: Camera -> M44 Float
cameraViewMatrix cam = lookAt (camPosition cam) (camPosition cam ^+^ camFront cam) (camUp cam)

-- | Compute perspective projection matrix
cameraProjectionMatrix :: Float -> Float -> Float -> Camera -> M44 Float
cameraProjectionMatrix aspect near far cam =
  perspective (camFov cam * pi / 180) aspect near far

-- | Update camera front vector from yaw/pitch
updateCamera :: Float -> Float -> Camera -> Camera
updateCamera dx dy cam =
  let yaw'   = camYaw cam + dx * camSensitivity cam
      pitch' = max (-89) . min 89 $ camPitch cam + dy * camSensitivity cam
      radYaw   = yaw' * pi / 180
      radPitch = pitch' * pi / 180
      front = normalize $ V3
        (cos radYaw * cos radPitch)
        (sin radPitch)
        (sin radYaw * cos radPitch)
  in cam { camYaw = yaw', camPitch = pitch', camFront = front }
