module Game.Physics
  ( AABB(..)
  , playerAABB
  , resolveCollision
  , applyGravity
  , isOnGround
  , sweepAABBVoxels
  , BlockQuery
  ) where

import Linear (V3(..))
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Axis-aligned bounding box defined by min and max corners
data AABB = AABB
  { aabbMin :: !(V3 Float)
  , aabbMax :: !(V3 Float)
  } deriving stock (Show, Eq)

-- | Block query: given world (x,y,z), returns True if block is solid
type BlockQuery = Int -> Int -> Int -> IO Bool

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
isOnGround :: BlockQuery -> V3 Float -> IO Bool
isOnGround isSolid (V3 x y z) = do
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
  any id <$> mapM (\(bx, by, bz) -> isSolid bx by bz) corners

-- | Resolve movement against solid blocks.
--   Takes current position, desired velocity * dt, and block query.
--   Returns new position after collision resolution.
--   Uses separate axis resolution (X, then Y, then Z) to avoid tunneling.
resolveCollision :: BlockQuery -> V3 Float -> V3 Float -> IO (V3 Float, V3 Float)
resolveCollision isSolid pos (V3 dvx dvy dvz) = do
  -- Resolve each axis independently to prevent corner sticking
  -- Y first (gravity is most important), then X, then Z
  let pos0 = pos

  -- Move Y
  (pos1, newDvy) <- resolveAxis isSolid pos0 (V3 0 dvy 0) 1
  -- Move X
  (pos2, newDvx) <- resolveAxis isSolid pos1 (V3 dvx 0 0) 0
  -- Move Z
  (pos3, newDvz) <- resolveAxis isSolid pos2 (V3 0 0 dvz) 2

  let velResult = V3 (if newDvx == 0 then 0 else dvx)
                     (if newDvy == 0 then 0 else dvy)
                     (if newDvz == 0 then 0 else dvz)

  pure (pos3, velResult)

-- | Resolve movement along a single axis.
--   Returns new position and remaining displacement on that axis (0 if blocked).
resolveAxis :: BlockQuery -> V3 Float -> V3 Float -> Int -> IO (V3 Float, Float)
resolveAxis isSolid pos delta axisIdx = do
  let newPos = pos + delta
      aabb = playerAABB newPos
  blocked <- aabbCollidesVoxels isSolid aabb
  if blocked
    then pure (pos, 0)  -- blocked: don't move, zero out velocity on this axis
    else pure (newPos, getAxis axisIdx delta)

-- | Get axis component
getAxis :: Int -> V3 Float -> Float
getAxis 0 (V3 x _ _) = x
getAxis 1 (V3 _ y _) = y
getAxis _ (V3 _ _ z) = z

-- | Check if an AABB overlaps any solid block
aabbCollidesVoxels :: BlockQuery -> AABB -> IO Bool
aabbCollidesVoxels isSolid (AABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) = do
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
              solid <- isSolid x y z
              if solid then pure True
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
  (V3 (min (v3x mn) (v3x mn + vx)) (min (v3y mn) (v3y mn + vy)) (min (v3z mn) (v3z mn + vz)))
  (V3 (max (v3x mx) (v3x mx + vx)) (max (v3y mx) (v3y mx + vy)) (max (v3z mx) (v3z mx + vz)))
  where
    v3x (V3 x _ _) = x
    v3y (V3 _ y _) = y
    v3z (V3 _ _ z) = z

-- | filterM for lists in IO
filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' _ [] = pure []
filterM' p (x:xs) = do
  b <- p x
  rest <- filterM' p xs
  pure $ if b then x : rest else rest
