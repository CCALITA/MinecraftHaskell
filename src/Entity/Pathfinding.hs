module Entity.Pathfinding
  ( PathNode(..)
  , findPath
  , pathDistance
  ) where

import Game.Physics (BlockQuery)
import Linear (V3(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (foldl')

-- | A node in the path grid (block coordinates)
data PathNode = PathNode
  { pnPos  :: !(V3 Int)
  , pnCost :: !Float       -- g-cost: actual cost from start
  } deriving stock (Show, Eq)

instance Ord PathNode where
  compare a b = compare (pnCost a) (pnCost b)

-- | Maximum path length to prevent infinite searches
maxPathLength :: Int
maxPathLength = 64

-- | A* pathfinding on the voxel grid.
--   Returns a list of positions from start to goal (excluding start), or Nothing if no path.
findPath :: BlockQuery -> V3 Int -> V3 Int -> IO (Maybe [V3 Int])
findPath isSolid start goal = do
  let startNode = PathNode start 0
  go (Map.singleton start (0 + heuristic start goal))  -- openSet: pos → fScore
     (Map.singleton start 0)                            -- gScore: pos → g
     Map.empty                                          -- cameFrom: pos → pos
     (Set.singleton start)                              -- openPositions
     0
  where
    go openScores gScores cameFrom openPositions iterations
      | Map.null openScores = pure Nothing
      | iterations > maxPathLength * 4 = pure Nothing  -- bail out
      | otherwise = do
          -- Pick node with lowest fScore
          let (currentPos, _) = Map.foldlWithKey'
                (\acc@(_, bestF) pos f -> if f < bestF then (pos, f) else acc)
                (fst $ Map.findMin openScores, snd $ Map.findMin openScores)
                openScores

          if currentPos == goal
            then pure $ Just (reconstructPath cameFrom currentPos)
            else do
              let openScores' = Map.delete currentPos openScores
                  openPositions' = Set.delete currentPos openPositions
                  currentG = Map.findWithDefault 1e30 currentPos gScores

              -- Get walkable neighbors
              nbrs <- walkableNeighbors isSolid currentPos
              let processNeighbor (os, gs, cf, op) nbrPos = do
                    let tentativeG = currentG + neighborCost currentPos nbrPos
                        oldG = Map.findWithDefault 1e30 nbrPos gs
                    if tentativeG < oldG
                      then do
                        let fScore = tentativeG + heuristic nbrPos goal
                        pure ( Map.insert nbrPos fScore os
                             , Map.insert nbrPos tentativeG gs
                             , Map.insert nbrPos currentPos cf
                             , Set.insert nbrPos op
                             )
                      else pure (os, gs, cf, op)

              (os'', gs'', cf'', op'') <- foldlM' processNeighbor
                (openScores', gScores, cameFrom, openPositions') nbrs
              go os'' gs'' cf'' op'' (iterations + 1)

-- | Heuristic: Manhattan distance (admissible for grid)
heuristic :: V3 Int -> V3 Int -> Float
heuristic (V3 x1 y1 z1) (V3 x2 y2 z2) =
  fromIntegral (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2))

-- | Cost to move between adjacent nodes
neighborCost :: V3 Int -> V3 Int -> Float
neighborCost (V3 x1 y1 z1) (V3 x2 y2 z2)
  | y1 /= y2 = 1.5   -- vertical movement costs more
  | otherwise = 1.0

-- | Get walkable neighbor positions.
--   A neighbor is walkable if:
--   1. The block at neighbor position is not solid (can stand)
--   2. The block above neighbor is not solid (head clearance)
--   3. The block below neighbor IS solid (has ground to stand on)
walkableNeighbors :: BlockQuery -> V3 Int -> IO [V3 Int]
walkableNeighbors isSolid (V3 x y z) = do
  let candidates =
        -- Horizontal movement
        [ V3 (x+1) y z, V3 (x-1) y z
        , V3 x y (z+1), V3 x y (z-1)
        -- Step up (jump onto 1-high ledge)
        , V3 (x+1) (y+1) z, V3 (x-1) (y+1) z
        , V3 x (y+1) (z+1), V3 x (y+1) (z-1)
        -- Step down
        , V3 (x+1) (y-1) z, V3 (x-1) (y-1) z
        , V3 x (y-1) (z+1), V3 x (y-1) (z-1)
        ]
  filterM' (isWalkable isSolid) candidates

-- | Check if a position is walkable (feet pos, not solid; head pos, not solid; below, solid)
isWalkable :: BlockQuery -> V3 Int -> IO Bool
isWalkable isSolid (V3 x y z) = do
  feetSolid <- isSolid x y z
  headSolid <- isSolid x (y + 1) z
  groundSolid <- isSolid x (y - 1) z
  pure $ not feetSolid && not headSolid && groundSolid

-- | Reconstruct path from cameFrom map
reconstructPath :: Map.Map (V3 Int) (V3 Int) -> V3 Int -> [V3 Int]
reconstructPath cameFrom current = go current []
  where
    go pos acc = case Map.lookup pos cameFrom of
      Nothing  -> acc  -- reached start, don't include it
      Just prev -> go prev (pos : acc)

-- | Total path distance
pathDistance :: [V3 Int] -> Float
pathDistance [] = 0
pathDistance [_] = 0
pathDistance (a:b:rest) = heuristic a b + pathDistance (b:rest)

-- | Monadic foldl' for IO
foldlM' :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ acc [] = pure acc
foldlM' f acc (x:xs) = do
  acc' <- f acc x
  foldlM' f acc' xs

-- | filterM for IO
filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' _ [] = pure []
filterM' p (x:xs) = do
  b <- p x
  rest <- filterM' p xs
  pure $ if b then x : rest else rest
