module Game.Player
  ( Player(..)
  , PlayerInput(..)
  , defaultPlayer
  , noInput
  , updatePlayer
  , raycastBlock
  , RayHit(..)
  ) where

import Game.Physics
import World.Block (BlockType(..), isSolid)

import Linear (V3(..), normalize, cross, (^*), norm)
import Data.Maybe (listToMaybe)

-- | Player state
data Player = Player
  { plPos       :: !(V3 Float)   -- feet position
  , plVelocity  :: !(V3 Float)
  , plYaw       :: !Float        -- degrees, 0 = +Z
  , plPitch     :: !Float        -- degrees, clamped [-89, 89]
  , plOnGround  :: !Bool
  , plFlying    :: !Bool         -- creative-mode flying
  , plSprinting :: !Bool
  } deriving stock (Show, Eq)

-- | Input state for a single tick
data PlayerInput = PlayerInput
  { piForward  :: !Bool
  , piBackward :: !Bool
  , piLeft     :: !Bool
  , piRight    :: !Bool
  , piJump     :: !Bool
  , piSneak    :: !Bool
  , piSprint   :: !Bool
  , piMouseDX  :: !Float
  , piMouseDY  :: !Float
  , piToggleFly :: !Bool
  } deriving stock (Show, Eq)

-- | Default player at spawn
defaultPlayer :: V3 Float -> Player
defaultPlayer spawnPos = Player
  { plPos       = spawnPos
  , plVelocity  = V3 0 0 0
  , plYaw       = 0
  , plPitch     = 0
  , plOnGround  = False
  , plFlying    = True  -- start in creative fly mode
  , plSprinting = False
  }

noInput :: PlayerInput
noInput = PlayerInput False False False False False False False 0 0 False

-- | Movement speeds
walkSpeed, sprintSpeed, flySpeed :: Float
walkSpeed   = 4.317
sprintSpeed = 5.612
flySpeed    = 11.0

-- | Jump velocity (blocks/s upward)
jumpVelocity :: Float
jumpVelocity = 9.0

-- | Mouse sensitivity
mouseSensitivity :: Float
mouseSensitivity = 0.15

-- | Update player for one physics tick
updatePlayer :: Float -> PlayerInput -> BlockQuery -> Player -> IO Player
updatePlayer dt input isSolidBlock player = do
  -- 1. Mouse look
  let yaw'   = plYaw player + piMouseDX input * mouseSensitivity
      pitch' = max (-89) . min 89 $ plPitch player + piMouseDY input * mouseSensitivity
      front  = directionFromAngles yaw' pitch'
      right  = normalize $ front `cross` V3 0 1 0
      flatFront = normalize $ V3 (v3x front) 0 (v3z front)

  -- 2. Toggle flying
  let flying = if piToggleFly input then not (plFlying player) else plFlying player

  -- 3. Movement input → wish direction
  let moveDir = V3 0 0 0
        + (if piForward  input then flatFront   else V3 0 0 0)
        + (if piBackward input then -flatFront  else V3 0 0 0)
        + (if piLeft     input then -right      else V3 0 0 0)
        + (if piRight    input then right       else V3 0 0 0)
      normalizedDir = if norm moveDir > 0.001 then normalize moveDir else V3 0 0 0

  -- 4. Compute velocity
  let speed
        | flying              = flySpeed
        | piSprint input      = sprintSpeed
        | otherwise           = walkSpeed

  let V3 vx vy vz = plVelocity player

  if flying
    then do
      -- Flying mode: direct movement, no gravity
      let flyDir = normalizedDir
            + (if piJump  input then V3 0 1 0 else V3 0 0 0)
            + (if piSneak input then V3 0 (-1) 0 else V3 0 0 0)
          flyVel = if norm flyDir > 0.001 then normalize flyDir ^* flySpeed else V3 0 0 0
          displacement = flyVel ^* dt

      -- Simple collision for flying
      (newPos, _) <- resolveCollision isSolidBlock (plPos player) displacement
      pure player
        { plPos      = newPos
        , plVelocity = flyVel
        , plYaw      = yaw'
        , plPitch    = pitch'
        , plOnGround = False
        , plFlying   = flying
        }
    else do
      -- Survival mode: gravity + collision
      -- Horizontal velocity from input
      let hVel = normalizedDir ^* speed
          V3 hvx _ hvz = hVel

      -- Vertical: gravity + jump
      onGround <- isOnGround isSolidBlock (plPos player)
      let vy' = if onGround && piJump input
                then jumpVelocity
                else applyGravity dt vy

      let desiredVel = V3 hvx vy' hvz
          displacement = desiredVel ^* dt

      (newPos, resolvedVel) <- resolveCollision isSolidBlock (plPos player) displacement

      onGround' <- isOnGround isSolidBlock newPos
      let finalVy = if onGround' && v3y resolvedVel <= 0 then 0 else v3y resolvedVel

      pure player
        { plPos       = newPos
        , plVelocity  = V3 (v3x resolvedVel) finalVy (v3z resolvedVel)
        , plYaw       = yaw'
        , plPitch     = pitch'
        , plOnGround  = onGround'
        , plFlying    = flying
        , plSprinting = piSprint input
        }

-- | Direction vector from yaw and pitch (degrees)
directionFromAngles :: Float -> Float -> V3 Float
directionFromAngles yaw pitch =
  let yawR   = yaw * pi / 180
      pitchR = pitch * pi / 180
  in V3 (sin yawR * cos pitchR) (sin pitchR) (cos yawR * cos pitchR)

-- | Extract V3 components
v3x, v3y, v3z :: V3 Float -> Float
v3x (V3 x _ _) = x
v3y (V3 _ y _) = y
v3z (V3 _ _ z) = z

-- | Result of a block raycast
data RayHit = RayHit
  { rhBlockPos  :: !(V3 Int)     -- block that was hit
  , rhFaceNormal :: !(V3 Int)    -- normal of the face hit (for placement)
  , rhDistance   :: !Float
  } deriving stock (Show, Eq)

-- | DDA voxel raycast (Amanatides & Woo algorithm).
--   Casts a ray from origin in direction, returns first solid block hit within maxDist.
raycastBlock :: (Int -> Int -> Int -> IO Bool) -> V3 Float -> V3 Float -> Float -> IO (Maybe RayHit)
raycastBlock isSolidBlock origin direction maxDist = do
  let V3 ox oy oz = origin
      V3 dx dy dz = direction

      -- Current voxel
      startX = floor ox :: Int
      startY = floor oy :: Int
      startZ = floor oz :: Int

      -- Step direction
      stepX = if dx > 0 then 1 else if dx < 0 then -1 else 0 :: Int
      stepY = if dy > 0 then 1 else if dy < 0 then -1 else 0 :: Int
      stepZ = if dz > 0 then 1 else if dz < 0 then -1 else 0 :: Int

      -- Distance to next voxel boundary on each axis
      tMaxX0 = if dx /= 0
               then (fromIntegral (if dx > 0 then startX + 1 else startX) - ox) / dx
               else 1e30
      tMaxY0 = if dy /= 0
               then (fromIntegral (if dy > 0 then startY + 1 else startY) - oy) / dy
               else 1e30
      tMaxZ0 = if dz /= 0
               then (fromIntegral (if dz > 0 then startZ + 1 else startZ) - oz) / dz
               else 1e30

      -- How far along ray to move one full voxel on each axis
      tDeltaX = if dx /= 0 then abs (1.0 / dx) else 1e30
      tDeltaY = if dy /= 0 then abs (1.0 / dy) else 1e30
      tDeltaZ = if dz /= 0 then abs (1.0 / dz) else 1e30

  let step !bx !by !bz !tmx !tmy !tmz !lastNormal = do
        -- Check distance
        let dist = minimum [tmx, tmy, tmz]
        if dist > maxDist
          then pure Nothing
          else do
            -- Check current block
            solid <- isSolidBlock bx by bz
            if solid
              then pure $ Just $ RayHit (V3 bx by bz) lastNormal dist
              else do
                -- Step to next voxel
                if tmx < tmy && tmx < tmz
                  then step (bx + stepX) by bz (tmx + tDeltaX) tmy tmz (V3 (-stepX) 0 0)
                  else if tmy < tmz
                    then step bx (by + stepY) bz tmx (tmy + tDeltaY) tmz (V3 0 (-stepY) 0)
                    else step bx by (bz + stepZ) tmx tmy (tmz + tDeltaZ) (V3 0 0 (-stepZ))

  -- Skip the block the player is standing in
  step startX startY startZ tMaxX0 tMaxY0 tMaxZ0 (V3 0 0 0)
