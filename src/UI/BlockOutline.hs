module UI.BlockOutline
  ( blockOutlineVerts
  ) where

import Linear (V3(..), V4(..), M44, (!*))

-- | The 8 corners of a unit cube at the given block position.
blockCorners :: V3 Int -> [V4 Float]
blockCorners (V3 bx by bz) =
  let x = fromIntegral bx
      y = fromIntegral by
      z = fromIntegral bz
  in [ V4 x       y       z       1
     , V4 (x+1)   y       z       1
     , V4 (x+1)   (y+1)   z       1
     , V4 x       (y+1)   z       1
     , V4 x       y       (z+1)   1
     , V4 (x+1)   y       (z+1)   1
     , V4 (x+1)   (y+1)   (z+1)   1
     , V4 x       (y+1)   (z+1)   1
     ]

-- | The 12 edges of a cube as pairs of corner indices (0-based).
cubeEdges :: [(Int, Int)]
cubeEdges =
  [ (0,1), (1,2), (2,3), (3,0)   -- front face
  , (4,5), (5,6), (6,7), (7,4)   -- back face
  , (0,4), (1,5), (2,6), (3,7)   -- connecting edges
  ]

-- | Project a homogeneous point via the VP matrix to NDC (x, y).
--   Returns Nothing if behind the camera (w <= 0).
projectPoint :: M44 Float -> V4 Float -> Maybe (Float, Float)
projectPoint vp pt =
  let V4 cx cy _cz cw = vp !* pt
  in if cw <= 0.001
       then Nothing
       else Just (cx / cw, cy / cw)

-- | Build a thin quad (2 triangles, 6 vertices) between two NDC points.
--   Each vertex is 6 floats: x y r g b a.
edgeQuad :: Float -> (Float, Float) -> (Float, Float) -> [Float]
edgeQuad halfW (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      len = sqrt (dx * dx + dy * dy)
      nx = if len < 1e-8 then 0 else (-dy) / len * halfW
      ny = if len < 1e-8 then 0 else   dx  / len * halfW
      -- 4 corners of the thin quad
      ax = x0 + nx; ay = y0 + ny
      bx = x0 - nx; by' = y0 - ny
      cx' = x1 - nx; cy' = y1 - ny
      dx' = x1 + nx; dy' = y1 + ny
      r = 0; g = 0; b = 0; a = 0.8 :: Float
      v vx vy = [vx, vy, r, g, b, a]
  in v ax ay ++ v bx by' ++ v dx' dy'
  ++ v bx by' ++ v cx' cy' ++ v dx' dy'

-- | Generate NDC vertex data for a block selection outline.
--   Projects the 8 corners of the block at the given position through the
--   view-projection matrix and draws 12 edges as thin quads (width 0.003).
--   Each vertex is 6 floats: x y r g b a (black with 0.8 alpha).
--   Edges behind the camera are omitted.
blockOutlineVerts :: V3 Int -> M44 Float -> [Float]
blockOutlineVerts pos vp =
  let corners = blockCorners pos
      projected = map (projectPoint vp) corners
      halfW = 0.003 / 2
      buildEdge (i, j) =
        case (projected !! i, projected !! j) of
          (Just a, Just b) -> edgeQuad halfW a b
          _                -> []
  in concatMap buildEdge cubeEdges
