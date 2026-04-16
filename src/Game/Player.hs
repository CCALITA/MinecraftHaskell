module Game.Player
  ( Player(..)
  , PlayerInput(..)
  , defaultPlayer
  , noInput
  , endFrameInput
  , updatePlayer
  , raycastBlock
  , RayHit(..)
  , maxHealth
  , maxHunger
  , applyFallDamage
  , damagePlayer
  , respawnPlayer
  , isPlayerDead
  ) where

import Game.Physics
import World.Block (BlockType(..), isSolid)

import Linear (V3(..), normalize, cross, (^*), norm)
import Data.Maybe (listToMaybe)

-- | Player state
data Player = Player
  { plPos          :: !(V3 Float)   -- feet position
  , plVelocity     :: !(V3 Float)
  , plYaw          :: !Float        -- degrees, 0 = +Z
  , plPitch        :: !Float        -- degrees, clamped [-89, 89]
  , plOnGround     :: !Bool
  , plFlying       :: !Bool         -- creative-mode flying
  , plSprinting    :: !Bool
  , plHealth       :: !Int          -- 0-20 (10 hearts)
  , plHunger       :: !Int          -- 0-20 (10 drumsticks)
  , plFallDist     :: !Float        -- accumulated fall distance
  , plEatingTimer  :: !Float        -- 0.0 = not eating, counts down from 1.6
  } deriving stock (Show, Eq)

-- | Max health (10 hearts = 20 half-hearts)
maxHealth :: Int
maxHealth = 20

-- | Max hunger (10 drumsticks = 20 half-drumsticks)
maxHunger :: Int
maxHunger = 20

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
  { plPos          = spawnPos
  , plVelocity     = V3 0 0 0
  , plYaw          = 0
  , plPitch        = 0
  , plOnGround     = False
  , plFlying       = True  -- start in creative fly mode
  , plSprinting    = False
  , plHealth       = maxHealth
  , plHunger       = maxHunger
  , plFallDist     = 0
  , plEatingTimer  = 0.0
  }

noInput :: PlayerInput
noInput = PlayerInput False False False False False False False 0 0 False

-- | Clear frame-local input, preserving queued one-shot actions until physics consumes them.
endFrameInput :: Bool -> PlayerInput -> PlayerInput
endFrameInput physicsTickRan input =
  noInput { piToggleFly = not physicsTickRan && piToggleFly input }

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
updatePlayer :: Float -> PlayerInput -> BlockQuery -> BlockQuery -> Player -> IO Player
updatePlayer dt input isSolidBlock isWaterBlock player = do
  -- 1. Mouse look
  let yaw'   = plYaw player - piMouseDX input * mouseSensitivity
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
        , plHealth   = plHealth player
        , plFallDist = 0
        }
    else do
      -- Survival mode: gravity + collision
      -- Check if player is in water
      let V3 px py pz = plPos player
      inWater <- isWaterBlock (floor px) (floor py) (floor pz)

      -- Horizontal velocity (slower in water)
      let waterMul = if inWater then 0.4 else 1.0
          hVel = normalizedDir ^* (speed * waterMul)
          V3 hvx _ hvz = hVel

      -- Vertical: gravity + jump (buoyancy in water)
      onGround <- isOnGround isSolidBlock (plPos player)
      let vy'
            | inWater && piJump input = 4.0   -- swim upward
            | inWater                 = max (-2.0) (vy - 5.0 * dt)  -- slow sinking
            | onGround && piJump input = jumpVelocity
            | otherwise               = applyGravity dt vy

      let desiredVel = V3 hvx vy' hvz
          displacement = desiredVel ^* dt

      (newPos0, resolvedVel) <- resolveCollision isSolidBlock (plPos player) displacement

      -- Sneak edge-stop: if sneaking and on ground, prevent walking off edges
      newPos <- if piSneak input && onGround
        then do
          hasGround <- isOnGround isSolidBlock newPos0
          if hasGround
            then pure newPos0
            else do
              -- Keep Y movement but cancel horizontal to stay on edge
              let V3 _ newY _ = newPos0
                  V3 oldX _ oldZ = plPos player
              pure (V3 oldX newY oldZ)
        else pure newPos0

      onGround' <- isOnGround isSolidBlock newPos
      -- Store actual velocity, zeroing axes where collision blocked movement.
      -- resolvedVel components are 0 when blocked by collision.
      let finalVy = if onGround' && vy' <= 0 then 0
                    else if v3y resolvedVel == 0 then 0  -- blocked by floor/ceiling
                    else vy'
          finalVx = if v3x resolvedVel == 0 then 0 else hvx
          finalVz = if v3z resolvedVel == 0 then 0 else hvz
          -- Track fall distance
          dy = v3y (plPos player) - v3y newPos  -- positive when falling down
          newFallDist = if dy > 0 then plFallDist player + dy else plFallDist player
          -- Apply fall damage on landing
          landed = onGround' && not (plOnGround player)
          baseDmg = if landed && newFallDist > 3.0 then floor newFallDist - 3 else 0
          finalHealth = max 0 (plHealth player - baseDmg)
          finalFallDist = if onGround' then 0 else newFallDist
          -- Hunger: drain on sprint/jump, regen health when full, starve when empty
          hungerDrain = (if piSprint input then 1 else 0)
                      + (if landed && piJump input then 1 else 0)
          curHunger = plHunger player
          -- Drain hunger very slowly (1 point per ~100 ticks of sprinting)
          newHunger = max 0 (curHunger - hungerDrain)
          -- Health regen when hunger >= 18
          healthRegen = if newHunger >= 18 && finalHealth < maxHealth then 1 else 0
          -- Starvation when hunger = 0
          starveDmg = if newHunger <= 0 && finalHealth > 1 then 1 else 0
          finalHealth' = min maxHealth (max 0 (finalHealth + healthRegen - starveDmg))

      pure player
        { plPos       = newPos
        , plVelocity  = V3 finalVx finalVy finalVz
        , plYaw       = yaw'
        , plPitch     = pitch'
        , plOnGround  = onGround'
        , plFlying    = flying
        , plSprinting = piSprint input
        , plHealth    = finalHealth'
        , plHunger    = newHunger
        , plFallDist  = finalFallDist
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

-- | Apply fall damage when player lands. Returns updated player.
--   Fall damage = floor(fallDist) - 3 (Minecraft formula: 1 HP per block above 3)
applyFallDamage :: Player -> Player
applyFallDamage player
  | plFlying player = player { plFallDist = 0 }
  | plOnGround player && plFallDist player > 3.0 =
      let damage = floor (plFallDist player) - 3
      in player { plHealth = max 0 (plHealth player - damage), plFallDist = 0 }
  | plOnGround player = player { plFallDist = 0 }
  | v3y (plVelocity player) < 0 =
      -- Accumulate fall distance while falling
      player  -- distance tracking is done in updatePlayer
  | otherwise = player { plFallDist = 0 }  -- going up, reset

-- | Apply damage to player
damagePlayer :: Int -> Player -> Player
damagePlayer dmg player = player { plHealth = max 0 (plHealth player - dmg) }

-- | Respawn player at given position with full health
respawnPlayer :: V3 Float -> Player -> Player
respawnPlayer spawnPos player = player
  { plPos = spawnPos, plVelocity = V3 0 0 0, plHealth = maxHealth, plHunger = maxHunger, plFallDist = 0 }

-- | Is the player dead?
isPlayerDead :: Player -> Bool
isPlayerDead = (<= 0) . plHealth

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
