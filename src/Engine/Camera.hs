module Engine.Camera
  ( Camera(..)
  , defaultCamera
  , cameraViewMatrix
  , cameraProjectionMatrix
  , updateCamera
  , Frustum
  , extractFrustum
  , isAABBInFrustum
  , thirdPersonOffset
  , thirdPersonViewMatrix
  , cameraFromPlayer
  , dirFromPlayer
  ) where

import Linear

import Game.Player (Player(..), eyeHeight, sneakEyeOffset)

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

-- | Compute perspective projection matrix adjusted for Vulkan clip space.
--   linear's 'perspective' targets OpenGL (Y-up, depth [-1,1]).
--   Vulkan needs Y-down, depth [0,1], so we apply a clip correction.
cameraProjectionMatrix :: Float -> Float -> Float -> Camera -> M44 Float
cameraProjectionMatrix aspect near far cam =
  let openglProj = perspective (camFov cam * pi / 180) aspect near far
      -- Vulkan clip correction: flip Y, remap depth from [-1,1] to [0,1]
      vulkanClip = V4 (V4 1 0    0   0)
                      (V4 0 (-1) 0   0)
                      (V4 0 0    0.5 0.5)
                      (V4 0 0    0   1)
  in vulkanClip !*! openglProj

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

-- | A frustum defined by 6 planes (normal.xyz + distance).
--   Each V4 (a, b, c, d) represents plane ax + by + cz + d >= 0.
type Frustum = [V4 Float]

-- | Extract 6 frustum planes from a combined view-projection matrix.
--   Input should be projection * view (NOT transposed — the raw linear M44).
extractFrustum :: M44 Float -> Frustum
extractFrustum m =
  let V4 r0 r1 r2 r3 = m
      -- Gribb-Hartmann method: extract planes from rows of the VP matrix
      left   = r3 ^+^ r0
      right  = r3 ^-^ r0
      bottom = r3 ^+^ r1
      top'   = r3 ^-^ r1
      near'  = r3 ^+^ r2
      far'   = r3 ^-^ r2
  in map normalizePlane [left, right, bottom, top', near', far']
  where
    normalizePlane (V4 a b c d) =
      let len = sqrt (a*a + b*b + c*c)
      in if len > 0.0001 then V4 (a/len) (b/len) (c/len) (d/len) else V4 0 0 0 0

-- | Test if an axis-aligned bounding box is at least partially inside the frustum.
--   AABB defined by (min corner, max corner).
isAABBInFrustum :: Frustum -> V3 Float -> V3 Float -> Bool
isAABBInFrustum planes (V3 minX minY minZ) (V3 maxX maxY maxZ) =
  all testPlane planes
  where
    -- For each plane, find the corner most in the direction of the normal.
    -- If that corner is behind the plane, the AABB is fully outside.
    testPlane (V4 a b c d) =
      let px = if a >= 0 then maxX else minX
          py = if b >= 0 then maxY else minY
          pz = if c >= 0 then maxZ else minZ
      in a * px + b * py + c * pz + d >= 0

-- | Compute the camera position offset for third-person views.
--   @isBack@ True = behind the player (looking in same direction),
--   False = in front of the player (looking back).
--   @dist@ is the distance in blocks from the player's eye position.
--   Returns @(offsetPosition, lookTarget)@ where the view matrix should
--   use @lookAt offsetPosition lookTarget up@.
thirdPersonOffset :: Bool -> Float -> Camera -> (V3 Float, V3 Float)
thirdPersonOffset isBack dist cam =
  let eyePos = camPosition cam
      front  = normalize (camFront cam)
  in if isBack
       then (eyePos ^-^ (front ^* dist), eyePos)
       else (eyePos ^+^ (front ^* dist), eyePos)

-- | Build a view matrix for third-person camera.
--   @isBack@ True = ThirdPersonBack, False = ThirdPersonFront.
thirdPersonViewMatrix :: Bool -> Float -> Camera -> M44 Float
thirdPersonViewMatrix isBack dist cam =
  let (pos, target) = thirdPersonOffset isBack dist cam
  in lookAt pos target (camUp cam)

-- | Convert player state to Camera
cameraFromPlayer :: Player -> Camera
cameraFromPlayer player =
  let yawR   = plYaw player * pi / 180
      pitchR = plPitch player * pi / 180
      front  = V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)
      fov    = if plSprinting player then 55 else 45
      eye    = if plSneaking player then eyeHeight - sneakEyeOffset else eyeHeight
  in defaultCamera
    { camPosition = plPos player + V3 0 eye 0
    , camFront    = front
    , camYaw      = plYaw player
    , camPitch    = plPitch player
    , camFov      = fov
    }

-- | Get look direction from player
dirFromPlayer :: Player -> V3 Float
dirFromPlayer player =
  let yawR   = plYaw player * pi / 180
      pitchR = plPitch player * pi / 180
  in normalize $ V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)
