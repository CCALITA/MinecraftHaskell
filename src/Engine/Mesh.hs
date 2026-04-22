module Engine.Mesh
  ( MeshData(..)
  , NeighborData(..)
  , emptyMesh
  , emptyNeighborData
  , meshChunk
  , meshChunkWithLight
  , BlockVertex(..)
  , calculateAO
  ) where

import World.Block
import World.Chunk
import World.Light

import Linear (V2(..), V3(..))
import Data.Word (Word8, Word32)
import Data.IORef
import Control.Monad (when)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)

-- | Vertex for block rendering: position + normal + texcoord + AO
data BlockVertex = BlockVertex
  { bvPosition :: !(V3 Float)
  , bvNormal   :: !(V3 Float)
  , bvTexCoord :: !(V2 Float)
  , bvAO       :: !Float         -- ambient occlusion (0-1)
  } deriving stock (Show, Eq)

instance Storable BlockVertex where
  sizeOf _ = 3*4 + 3*4 + 2*4 + 4  -- 36 bytes
  alignment _ = 4
  peek ptr = do
    let p = castPtr ptr
    pos <- peekByteOff p 0
    nor <- peekByteOff p 12
    tex <- peekByteOff p 24
    ao  <- peekByteOff p 32
    pure $ BlockVertex pos nor tex ao
  poke ptr (BlockVertex pos nor tex ao) = do
    let p = castPtr ptr
    pokeByteOff p 0  pos
    pokeByteOff p 12 nor
    pokeByteOff p 24 tex
    pokeByteOff p 32 ao

-- | CPU-side mesh data
data MeshData = MeshData
  { mdVertices :: !(VS.Vector BlockVertex)
  , mdIndices  :: !(VS.Vector Word32)
  } deriving stock (Show)

emptyMesh :: MeshData
emptyMesh = MeshData VS.empty VS.empty

-- | Optional frozen block vectors for the 4 cardinal neighbor chunks.
--   Used to look up blocks across chunk boundaries during meshing,
--   preventing invisible seam faces between adjacent chunks.
data NeighborData = NeighborData
  { ndNorth :: !(Maybe (UV.Vector Word8))   -- +Z neighbor
  , ndSouth :: !(Maybe (UV.Vector Word8))   -- -Z neighbor
  , ndEast  :: !(Maybe (UV.Vector Word8))   -- +X neighbor
  , ndWest  :: !(Maybe (UV.Vector Word8))   -- -X neighbor
  }

emptyNeighborData :: NeighborData
emptyNeighborData = NeighborData Nothing Nothing Nothing Nothing

-- | Atlas tile size: 16x16 tiles in a 256x256 atlas
atlasSize :: Float
atlasSize = 16.0

tileUV :: V2 Int -> (V2 Float, V2 Float)
tileUV (V2 tx ty) =
  let u0 = fromIntegral tx / atlasSize
      v0 = fromIntegral ty / atlasSize
      u1 = (fromIntegral tx + 1) / atlasSize
      v1 = (fromIntegral ty + 1) / atlasSize
  in (V2 u0 v0, V2 u1 v1)

-- | Generate mesh for a chunk using naive face culling.
--   For each block, emit faces only where the adjacent block is transparent.
--   TODO: Upgrade to full greedy meshing (merge coplanar quads) in optimization pass.
meshChunk :: Chunk -> IO MeshData
meshChunk chunk = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])   -- list of face groups (O(1) cons)
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  let addFace :: V3 Float -> BlockFace -> BlockType -> IO ()
      addFace (V3 bx by bz) face bt = do
        let (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0
            V2 u1 v1 = uv1
            ao = 1.0  -- placeholder, no AO yet
            -- Generate 4 vertices + 6 indices (2 triangles) per face
            (verts, _normal) = faceVertices bx by bz face u0 v0 u1 v1 ao
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)       -- O(1) cons
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

  -- Read blocks once (freeze for consistent snapshot during meshing)
  blocksVec <- freezeBlocks chunk
  let go !x !y !z
        | y >= chunkHeight = pure ()
        | z >= chunkDepth  = go 0 (y + 1) 0
        | x >= chunkWidth  = go 0 y (z + 1)
        | otherwise = do
            let bt = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z
            if bt == Air
              then go (x + 1) y z
              else do
                let bx = fromIntegral x
                    by = fromIntegral y
                    bz = fromIntegral z
                    pos = V3 bx by bz
                -- Check each face: emit if neighbor is transparent (using cached vector)
                checkFaceVec blocksVec pos FaceTop    bt (x, y+1, z) addFace
                checkFaceVec blocksVec pos FaceBottom bt (x, y-1, z) addFace
                checkFaceVec blocksVec pos FaceNorth  bt (x, y, z+1) addFace
                checkFaceVec blocksVec pos FaceSouth  bt (x, y, z-1) addFace
                checkFaceVec blocksVec pos FaceEast   bt (x+1, y, z) addFace
                checkFaceVec blocksVec pos FaceWest   bt (x-1, y, z) addFace
                go (x + 1) y z
  go 0 0 0

  verts   <- VS.fromList . concat . reverse <$> readIORef vertsRef
  indices <- VS.fromList . concat . reverse <$> readIORef indicesRef
  pure $ MeshData verts indices

-- | Generate mesh for a chunk with light map data and per-vertex AO.
--   Light levels and ambient occlusion are combined in the bvAO vertex attribute.
--   For each face vertex, AO is calculated from 3 neighbor blocks (side1, side2, corner).
--   The final value is lightVal * aoFactor, giving smooth shadow in corners.
--   Neighbor data enables cross-chunk face culling: at chunk edges, looks up the
--   adjacent block in the neighbor chunk instead of treating it as Air.
meshChunkWithLight :: Chunk -> LightMap -> NeighborData -> IO MeshData
meshChunkWithLight chunk lm neighbors = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  let addFace :: V3 Float -> BlockFace -> BlockType -> Float -> Float -> Float -> Float -> IO ()
      addFace (V3 bx by bz) face bt ao0 ao1 ao2 ao3 = do
        let (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0
            V2 u1 v1 = uv1
            (verts, _normal) = faceVerticesAO bx by bz face u0 v0 u1 v1 ao0 ao1 ao2 ao3
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

  blocksVec <- freezeBlocks chunk
  let -- | Look up a block type at the given local coords, consulting neighbor
      -- data when the coordinate falls outside this chunk's bounds.
      lookupBlock :: Int -> Int -> Int -> BlockType
      lookupBlock nx ny nz
        | ny < 0 || ny >= chunkHeight = Air
        -- If both x and z are out of bounds, treat as Air (diagonal across chunk corners)
        | (nx < 0 || nx >= chunkWidth) && (nz < 0 || nz >= chunkDepth) = Air
        -- +X boundary: look in east neighbor
        | nx >= chunkWidth = case ndEast neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex 0 ny nz
            Nothing -> Air
        -- -X boundary: look in west neighbor
        | nx < 0 = case ndWest neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex (chunkWidth - 1) ny nz
            Nothing -> Air
        -- +Z boundary: look in north neighbor
        | nz >= chunkDepth = case ndNorth neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny 0
            Nothing -> Air
        -- -Z boundary: look in south neighbor
        | nz < 0 = case ndSouth neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny (chunkDepth - 1)
            Nothing -> Air
        -- Within bounds: look in current chunk
        | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
      {-# INLINE lookupBlock #-}

      -- | Check if a block at given coords is solid (for AO calculation).
      isSolidAt :: Int -> Int -> Int -> Bool
      isSolidAt nx ny nz = isSolid (lookupBlock nx ny nz)
      {-# INLINE isSolidAt #-}

      -- | Compute per-vertex AO for a face. Returns (ao0, ao1, ao2, ao3)
      --   corresponding to the 4 corner vertices in winding order.
      --   Each corner checks its 2 adjacent side neighbors and 1 diagonal corner.
      faceAO :: Int -> Int -> Int -> BlockFace -> (Float, Float, Float, Float)
      faceAO bx by bz face = case face of
        FaceTop ->
          -- +Y face at (bx, by+1, bz). Neighbors are in the y+1 plane.
          let ny = by + 1
              -- vertex 0: (bx, by+1, bz) — sides: -X, -Z; corner: -X,-Z
              a0 = calculateAO (isSolidAt (bx-1) ny bz) (isSolidAt bx ny (bz-1)) (isSolidAt (bx-1) ny (bz-1))
              -- vertex 1: (bx+1, by+1, bz) — sides: +X, -Z; corner: +X,-Z
              a1 = calculateAO (isSolidAt (bx+1) ny bz) (isSolidAt bx ny (bz-1)) (isSolidAt (bx+1) ny (bz-1))
              -- vertex 2: (bx+1, by+1, bz+1) — sides: +X, +Z; corner: +X,+Z
              a2 = calculateAO (isSolidAt (bx+1) ny bz) (isSolidAt bx ny (bz+1)) (isSolidAt (bx+1) ny (bz+1))
              -- vertex 3: (bx, by+1, bz+1) — sides: -X, +Z; corner: -X,+Z
              a3 = calculateAO (isSolidAt (bx-1) ny bz) (isSolidAt bx ny (bz+1)) (isSolidAt (bx-1) ny (bz+1))
          in (a0, a1, a2, a3)

        FaceBottom ->
          -- -Y face at (bx, by-1, bz). Neighbors are in the y-1 plane.
          let ny = by - 1
              -- vertex 0: (bx, by, bz+1) — sides: -X, +Z; corner: -X,+Z
              a0 = calculateAO (isSolidAt (bx-1) ny bz) (isSolidAt bx ny (bz+1)) (isSolidAt (bx-1) ny (bz+1))
              -- vertex 1: (bx+1, by, bz+1) — sides: +X, +Z; corner: +X,+Z
              a1 = calculateAO (isSolidAt (bx+1) ny bz) (isSolidAt bx ny (bz+1)) (isSolidAt (bx+1) ny (bz+1))
              -- vertex 2: (bx+1, by, bz) — sides: +X, -Z; corner: +X,-Z
              a2 = calculateAO (isSolidAt (bx+1) ny bz) (isSolidAt bx ny (bz-1)) (isSolidAt (bx+1) ny (bz-1))
              -- vertex 3: (bx, by, bz) — sides: -X, -Z; corner: -X,-Z
              a3 = calculateAO (isSolidAt (bx-1) ny bz) (isSolidAt bx ny (bz-1)) (isSolidAt (bx-1) ny (bz-1))
          in (a0, a1, a2, a3)

        FaceNorth ->
          -- +Z face at bz+1. Neighbors are in the z+1 plane.
          let nz = bz + 1
              -- vertex 0: (bx+1, by, bz+1) — sides: +X, -Y; corner: +X,-Y
              a0 = calculateAO (isSolidAt (bx+1) by nz) (isSolidAt bx (by-1) nz) (isSolidAt (bx+1) (by-1) nz)
              -- vertex 1: (bx, by, bz+1) — sides: -X, -Y; corner: -X,-Y
              a1 = calculateAO (isSolidAt (bx-1) by nz) (isSolidAt bx (by-1) nz) (isSolidAt (bx-1) (by-1) nz)
              -- vertex 2: (bx, by+1, bz+1) — sides: -X, +Y; corner: -X,+Y
              a2 = calculateAO (isSolidAt (bx-1) by nz) (isSolidAt bx (by+1) nz) (isSolidAt (bx-1) (by+1) nz)
              -- vertex 3: (bx+1, by+1, bz+1) — sides: +X, +Y; corner: +X,+Y
              a3 = calculateAO (isSolidAt (bx+1) by nz) (isSolidAt bx (by+1) nz) (isSolidAt (bx+1) (by+1) nz)
          in (a0, a1, a2, a3)

        FaceSouth ->
          -- -Z face at bz-1. Neighbors are in the z-1 plane.
          let nz = bz - 1
              -- vertex 0: (bx, by, bz) — sides: -X, -Y; corner: -X,-Y
              a0 = calculateAO (isSolidAt (bx-1) by nz) (isSolidAt bx (by-1) nz) (isSolidAt (bx-1) (by-1) nz)
              -- vertex 1: (bx+1, by, bz) — sides: +X, -Y; corner: +X,-Y
              a1 = calculateAO (isSolidAt (bx+1) by nz) (isSolidAt bx (by-1) nz) (isSolidAt (bx+1) (by-1) nz)
              -- vertex 2: (bx+1, by+1, bz) — sides: +X, +Y; corner: +X,+Y
              a2 = calculateAO (isSolidAt (bx+1) by nz) (isSolidAt bx (by+1) nz) (isSolidAt (bx+1) (by+1) nz)
              -- vertex 3: (bx, by+1, bz) — sides: -X, +Y; corner: -X,+Y
              a3 = calculateAO (isSolidAt (bx-1) by nz) (isSolidAt bx (by+1) nz) (isSolidAt (bx-1) (by+1) nz)
          in (a0, a1, a2, a3)

        FaceEast ->
          -- +X face at bx+1. Neighbors are in the x+1 plane.
          let nx = bx + 1
              -- vertex 0: (bx+1, by, bz) — sides: -Z, -Y; corner: -Z,-Y
              a0 = calculateAO (isSolidAt nx by (bz-1)) (isSolidAt nx (by-1) bz) (isSolidAt nx (by-1) (bz-1))
              -- vertex 1: (bx+1, by, bz+1) — sides: +Z, -Y; corner: +Z,-Y
              a1 = calculateAO (isSolidAt nx by (bz+1)) (isSolidAt nx (by-1) bz) (isSolidAt nx (by-1) (bz+1))
              -- vertex 2: (bx+1, by+1, bz+1) — sides: +Z, +Y; corner: +Z,+Y
              a2 = calculateAO (isSolidAt nx by (bz+1)) (isSolidAt nx (by+1) bz) (isSolidAt nx (by+1) (bz+1))
              -- vertex 3: (bx+1, by+1, bz) — sides: -Z, +Y; corner: -Z,+Y
              a3 = calculateAO (isSolidAt nx by (bz-1)) (isSolidAt nx (by+1) bz) (isSolidAt nx (by+1) (bz-1))
          in (a0, a1, a2, a3)

        FaceWest ->
          -- -X face at bx-1. Neighbors are in the x-1 plane.
          let nx = bx - 1
              -- vertex 0: (bx, by, bz+1) — sides: +Z, -Y; corner: +Z,-Y
              a0 = calculateAO (isSolidAt nx by (bz+1)) (isSolidAt nx (by-1) bz) (isSolidAt nx (by-1) (bz+1))
              -- vertex 1: (bx, by, bz) — sides: -Z, -Y; corner: -Z,-Y
              a1 = calculateAO (isSolidAt nx by (bz-1)) (isSolidAt nx (by-1) bz) (isSolidAt nx (by-1) (bz-1))
              -- vertex 2: (bx, by+1, bz) — sides: -Z, +Y; corner: -Z,+Y
              a2 = calculateAO (isSolidAt nx by (bz-1)) (isSolidAt nx (by+1) bz) (isSolidAt nx (by+1) (bz-1))
              -- vertex 3: (bx, by+1, bz+1) — sides: +Z, +Y; corner: +Z,+Y
              a3 = calculateAO (isSolidAt nx by (bz+1)) (isSolidAt nx (by+1) bz) (isSolidAt nx (by+1) (bz+1))
          in (a0, a1, a2, a3)
      {-# INLINE faceAO #-}

      go !x !y !z
        | y >= chunkHeight = pure ()
        | z >= chunkDepth  = go 0 (y + 1) 0
        | x >= chunkWidth  = go 0 y (z + 1)
        | otherwise = do
            let bt = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z
            if bt == Air
              then go (x + 1) y z
              else do
                let bx = fromIntegral x
                    by = fromIntegral y
                    bz = fromIntegral z
                    pos = V3 bx by bz
                    checkFaceLight nx ny nz face = do
                      let neighbor = lookupBlock nx ny nz
                      when (isTransparent neighbor) $ do
                        lightLevel <- getTotalLight lm nx ny nz
                        let lightVal = max 0.1 (fromIntegral lightLevel / 15.0)
                            (a0, a1, a2, a3) = faceAO x y z face
                        addFace pos face bt (lightVal * a0) (lightVal * a1) (lightVal * a2) (lightVal * a3)
                checkFaceLight x (y+1) z FaceTop
                checkFaceLight x (y-1) z FaceBottom
                checkFaceLight x y (z+1) FaceNorth
                checkFaceLight x y (z-1) FaceSouth
                checkFaceLight (x+1) y z FaceEast
                checkFaceLight (x-1) y z FaceWest
                go (x + 1) y z
  go 0 0 0

  verts   <- VS.fromList . concat . reverse <$> readIORef vertsRef
  indices <- VS.fromList . concat . reverse <$> readIORef indicesRef
  pure $ MeshData verts indices
checkFaceVec :: UV.Vector Word8 -> V3 Float -> BlockFace -> BlockType
             -> (Int, Int, Int) -> (V3 Float -> BlockFace -> BlockType -> IO ()) -> IO ()
checkFaceVec blocksVec pos face bt (nx, ny, nz) addFace = do
  let neighbor
        | not (isInBounds nx ny nz) = Air  -- chunk boundary: always emit face
        | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
  if isTransparent neighbor
    then addFace pos face bt
    else pure ()
{-# INLINE checkFaceVec #-}

-- | Calculate ambient occlusion for a single vertex corner.
--   Takes three neighbor solidity flags: side1, side2, and the diagonal corner.
--   If both sides are solid, the corner is forced solid (fully occluded).
--   Returns a value in [0.0, 1.0] where 1.0 = no occlusion, 0.0 = fully occluded.
calculateAO :: Bool -> Bool -> Bool -> Float
calculateAO side1 side2 corner
  | side1 && side2 = 0.0   -- both sides solid → force corner solid
  | otherwise      = fromIntegral (3 - solidCount) / 3.0
  where
    solidCount :: Int
    solidCount = fromEnum side1 + fromEnum side2 + fromEnum corner
{-# INLINE calculateAO #-}

-- | Generate 4 vertices for a block face with per-vertex AO.
--   ao0..ao3 correspond to the four corner vertices in winding order.
faceVerticesAO :: Float -> Float -> Float -> BlockFace
               -> Float -> Float -> Float -> Float  -- u0 v0 u1 v1
               -> Float -> Float -> Float -> Float   -- ao0 ao1 ao2 ao3
               -> ([BlockVertex], V3 Float)
faceVerticesAO x y z face u0 v0 u1 v1 ao0 ao1 ao2 ao3 = case face of
  FaceTop -> -- +Y face (y+1 plane)
    ( [ BlockVertex (V3 x     (y+1) z    ) normal (V2 u0 v0) ao0
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u1 v0) ao1
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v1) ao2
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u0 v1) ao3
      ], normal)
    where normal = V3 0 1 0

  FaceBottom -> -- -Y face (y plane)
    ( [ BlockVertex (V3 x     y (z+1)) normal (V2 u0 v0) ao0
      , BlockVertex (V3 (x+1) y (z+1)) normal (V2 u1 v0) ao1
      , BlockVertex (V3 (x+1) y z    ) normal (V2 u1 v1) ao2
      , BlockVertex (V3 x     y z    ) normal (V2 u0 v1) ao3
      ], normal)
    where normal = V3 0 (-1) 0

  FaceNorth -> -- +Z face
    ( [ BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u0 v1) ao0
      , BlockVertex (V3 x     y     (z+1)) normal (V2 u1 v1) ao1
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u1 v0) ao2
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 0 0 1

  FaceSouth -> -- -Z face
    ( [ BlockVertex (V3 x     y     z) normal (V2 u0 v1) ao0
      , BlockVertex (V3 (x+1) y     z) normal (V2 u1 v1) ao1
      , BlockVertex (V3 (x+1) (y+1) z) normal (V2 u1 v0) ao2
      , BlockVertex (V3 x     (y+1) z) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 0 0 (-1)

  FaceEast -> -- +X face
    ( [ BlockVertex (V3 (x+1) y     z    ) normal (V2 u0 v1) ao0
      , BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u1 v1) ao1
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v0) ao2
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 1 0 0

  FaceWest -> -- -X face
    ( [ BlockVertex (V3 x y     (z+1)) normal (V2 u0 v1) ao0
      , BlockVertex (V3 x y     z    ) normal (V2 u1 v1) ao1
      , BlockVertex (V3 x (y+1) z    ) normal (V2 u1 v0) ao2
      , BlockVertex (V3 x (y+1) (z+1)) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 (-1) 0 0

-- | Generate 4 vertices for a block face (uniform AO for all vertices).
--   Kept for backward compatibility with meshChunk.
faceVertices :: Float -> Float -> Float -> BlockFace
             -> Float -> Float -> Float -> Float  -- u0 v0 u1 v1
             -> Float                              -- ao
             -> ([BlockVertex], V3 Float)
faceVertices x y z face u0 v0 u1 v1 ao =
  faceVerticesAO x y z face u0 v0 u1 v1 ao ao ao ao
