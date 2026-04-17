module Game.Physics
  ( AABB(..)
  , playerAABB
  , resolveCollision
  , applyGravity
  , isOnGround
  , sweepAABBVoxels
  , BlockQuery
  , BlockHeightQuery
  ) where

import Linear (V3(..))

-- | Axis-aligned bounding box defined by min and max corners
data AABB = AABB
  { aabbMin :: !(V3 Float)
  , aabbMax :: !(V3 Float)
  } deriving stock (Show, Eq)

-- | Block query: given world (x,y,z), returns True if block is solid
type BlockQuery = Int -> Int -> Int -> IO Bool

-- | Block height query: given world (x,y,z), returns collision height (0.0 for non-solid, 0.5 for slabs, 1.0 for full blocks)
type BlockHeightQuery = Int -> Int -> Int -> IO Float

-- | Player AABB: 0.6 wide, 1.8 tall, centered on position (feet at pos.y)
playerAABB :: V3 Float -> AABB
playerAABB (V3 x y z) = AABB
  { aabbMin = V3 (x - 0.3) y       (z - 0.3)
  , aabbMax = V3 (x + 0.3) (y + 1.8) (z + 0.3)
  }

-- | Gravity constant (blocks per second^2, Minecraft uses ~32)
gravity :: Float
gravity = 32.0

-- | Terminal velocity
terminalVelocity :: Float
terminalVelocity = -78.4

-- | Apply gravity to vertical velocity
applyGravity :: Float -> Float -> Float
applyGravity dt vy = max terminalVelocity (vy - gravity * dt)

-- | Check if the player is standing on a solid block
isOnGround :: BlockHeightQuery -> V3 Float -> IO Bool
isOnGround blockHeight (V3 x y z) = do
  -- Check with larger epsilon to handle collision resolution gap
  -- Player can float up to ~0.1 blocks above the surface
  let checkY = floor (y - 0.15) :: Int
      -- Check blocks under the player's feet (4 corners of AABB base)
      corners =
        [ (floor (x - 0.29) :: Int, checkY, floor (z - 0.29) :: Int)
        , (floor (x + 0.29) :: Int, checkY, floor (z - 0.29) :: Int)
        , (floor (x - 0.29) :: Int, checkY, floor (z + 0.29) :: Int)
        , (floor (x + 0.29) :: Int, checkY, floor (z + 0.29) :: Int)
        ]
  -- A block supports the player if its top reaches up to the player's feet
  any id <$> mapM (\(bx, by, bz) -> do
    h <- blockHeight bx by bz
    let blockTop = fromIntegral by + h
    pure (h > 0 && abs (y - blockTop) < 0.15)
    ) corners

-- | Resolve movement against solid blocks.
--   Takes current position, desired velocity * dt, and block height query.
--   Returns new position after collision resolution.
--   Uses separate axis resolution (X, then Y, then Z) to avoid tunneling.
resolveCollision :: BlockHeightQuery -> V3 Float -> V3 Float -> IO (V3 Float, V3 Float)
resolveCollision blockHeight pos (V3 dvx dvy dvz) = do
  -- Resolve each axis independently to prevent corner sticking
  -- Y first (gravity is most important), then X, then Z
  let pos0 = pos
      mkVelResult rdx rdy rdz = V3 (if rdx == 0 then 0 else dvx)
                                    (if rdy == 0 then 0 else dvy)
                                    (if rdz == 0 then 0 else dvz)

  -- Move Y
  (pos1, newDvy) <- resolveAxis blockHeight pos0 (V3 0 dvy 0) 1
  -- Move X
  (pos2, newDvx) <- resolveAxis blockHeight pos1 (V3 dvx 0 0) 0
  -- Move Z
  (pos3, newDvz) <- resolveAxis blockHeight pos2 (V3 0 0 dvz) 2

  -- Auto-step: if blocked horizontally, try stepping up by 0.5 (slab height)
  let wasBlockedX = newDvx == 0 && abs dvx > 0.001
      wasBlockedZ = newDvz == 0 && abs dvz > 0.001
  if wasBlockedX || wasBlockedZ
    then do
      let stepUpPos = pos1 + V3 0 0.5 0
          stepUpAABB = playerAABB stepUpPos
      stepUpBlocked <- aabbCollidesVoxelsHeight blockHeight stepUpAABB
      if stepUpBlocked
        then pure (pos3, mkVelResult newDvx newDvy newDvz)
        else do
          -- Try moving horizontally from the stepped-up position
          (stepPos2, stepDvx) <- resolveAxis blockHeight stepUpPos (V3 dvx 0 0) 0
          (stepPos3, stepDvz) <- resolveAxis blockHeight stepPos2 (V3 0 0 dvz) 2
          -- Settle back down
          (stepPos4, _) <- resolveAxis blockHeight stepPos3 (V3 0 (-0.5) 0) 1
          -- Only use step-up if it allowed more horizontal movement
          let madeProgressX = abs (v3x stepPos4 - v3x pos1) > abs (v3x pos3 - v3x pos1)
              madeProgressZ = abs (v3z stepPos4 - v3z pos1) > abs (v3z pos3 - v3z pos1)
          if madeProgressX || madeProgressZ
            then pure (stepPos4, mkVelResult stepDvx newDvy stepDvz)
            else pure (pos3, mkVelResult newDvx newDvy newDvz)
    else pure (pos3, mkVelResult newDvx newDvy newDvz)

-- | Resolve movement along a single axis.
--   Returns new position and remaining displacement on that axis (0 if blocked).
--   When falling (Y axis, downward), snaps to block top to prevent floating.
resolveAxis :: BlockHeightQuery -> V3 Float -> V3 Float -> Int -> IO (V3 Float, Float)
resolveAxis blockHeight pos delta axisIdx = do
  let newPos = pos + delta
      aabb = playerAABB newPos
  blocked <- aabbCollidesVoxelsHeight blockHeight aabb
  if blocked
    then do
      -- For Y axis falling (delta.y < 0), try snapping to block top
      if axisIdx == 1 && getAxis 1 delta < 0
        then do
          let V3 px py pz = pos
              -- Find the highest block top below the player
              -- Check the block at floor(py - epsilon) and snap to its top
              checkY = floor py :: Int
          -- Get the height of the block below
          maxTop <- getMaxBlockTop blockHeight (playerAABB (V3 px (fromIntegral checkY) pz)) checkY
          let snappedY = if maxTop > 0 then maxTop else fromIntegral (floor py :: Int)
              snappedPos = V3 px snappedY pz
              snappedAABB = playerAABB snappedPos
          snappedBlocked <- aabbCollidesVoxelsHeight blockHeight snappedAABB
          if snappedBlocked
            then pure (pos, 0)  -- still blocked even at snap, don't move
            else pure (snappedPos, 0)
        else pure (pos, 0)  -- blocked on other axes: don't move
    else pure (newPos, getAxis axisIdx delta)

-- | Get the maximum block top Y coordinate within an AABB's XZ footprint at a given Y level
getMaxBlockTop :: BlockHeightQuery -> AABB -> Int -> IO Float
getMaxBlockTop blockHeight (AABB (V3 minX _ minZ) (V3 maxX _ maxZ)) by = do
  let bMinX = floor minX :: Int
      bMinZ = floor minZ :: Int
      bMaxX = floor (maxX - 0.001) :: Int
      bMaxZ = floor (maxZ - 0.001) :: Int
  go bMinX bMinZ bMaxX bMaxZ (fromIntegral by)
  where
    go !bx !bz !mx !mz !best = checkLoop bx bz best
      where
        checkLoop !x !z !acc
          | x > mx = pure acc
          | z > mz = checkLoop (x + 1) bz acc
          | otherwise = do
              h <- blockHeight x by z
              let top = if h > 0 then fromIntegral by + h else acc
                  acc' = max acc top
              checkLoop x (z + 1) acc'

-- | Get axis component
getAxis :: Int -> V3 Float -> Float
getAxis 0 (V3 x _ _) = x
getAxis 1 (V3 _ y _) = y
getAxis _ (V3 _ _ z) = z

-- | Get V3 X component
v3x :: V3 Float -> Float
v3x (V3 x _ _) = x

-- | Get V3 Z component
v3z :: V3 Float -> Float
v3z (V3 _ _ z) = z

-- | Check if an AABB overlaps any solid block (with height-aware collision)
aabbCollidesVoxelsHeight :: BlockHeightQuery -> AABB -> IO Bool
aabbCollidesVoxelsHeight blockHeight (AABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) = do
  let bMinX = floor minX :: Int
      bMinY = floor minY :: Int
      bMinZ = floor minZ :: Int
      bMaxX = floor (maxX - 0.001) :: Int  -- tiny epsilon to avoid edge-touching
      bMaxY = floor (maxY - 0.001) :: Int
      bMaxZ = floor (maxZ - 0.001) :: Int
  go bMinX bMinY bMinZ bMaxX bMaxY bMaxZ
  where
    go !bx !by !bz !mx !my !mz = checkLoop bx by bz
      where
        checkLoop !x !y !z
          | y > my = pure False
          | x > mx = checkLoop bx (y + 1) bz  -- next Y, reset X
          | z > mz = checkLoop (x + 1) y bz   -- next X, reset Z
          | otherwise = do
              h <- blockHeight x y z
              -- Block collision box: (x, y, z) to (x+1, y+h, z+1)
              let blockTop = fromIntegral y + h
                  overlaps = h > 0
                          && maxX > fromIntegral x
                          && minX < fromIntegral (x + 1)
                          && maxY > fromIntegral y
                          && minY < blockTop
                          && maxZ > fromIntegral z
                          && minZ < fromIntegral (z + 1)
              if overlaps then pure True
              else checkLoop x y (z + 1)

-- | Sweep an AABB through voxel space and find all solid blocks it would intersect.
--   Used for more precise collision in the future.
sweepAABBVoxels :: BlockQuery -> AABB -> V3 Float -> IO [(V3 Int, Float)]
sweepAABBVoxels isSolid aabb velocity = do
  -- For now, simple discrete collision check
  let AABB (V3 minX minY minZ) (V3 maxX maxY maxZ) = expandAABB aabb velocity
      blocks = [ V3 bx by bz
               | bx <- [floor minX .. floor maxX]
               , by <- [floor minY .. floor maxY]
               , bz <- [floor minZ .. floor maxZ]
               ]
  solidBlocks <- filterM' (\(V3 x y z) -> isSolid x y z) blocks
  pure $ map (\b -> (b, 0)) solidBlocks

-- | Expand AABB by velocity for broad-phase
expandAABB :: AABB -> V3 Float -> AABB
expandAABB (AABB mn mx) (V3 vx vy vz) = AABB
  (V3 (min (ev3x mn) (ev3x mn + vx)) (min (ev3y mn) (ev3y mn + vy)) (min (ev3z mn) (ev3z mn + vz)))
  (V3 (max (ev3x mx) (ev3x mx + vx)) (max (ev3y mx) (ev3y mx + vy)) (max (ev3z mx) (ev3z mx + vz)))
  where
    ev3x (V3 x _ _) = x
    ev3y (V3 _ y _) = y
    ev3z (V3 _ _ z) = z

-- | filterM for lists in IO
filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' _ [] = pure []
filterM' p (x:xs) = do
  b <- p x
  rest <- filterM' p xs
  pure $ if b then x : rest else rest
