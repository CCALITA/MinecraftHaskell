module Engine.Mesh
  ( MeshData(..)
  , NeighborData(..)
  , emptyMesh
  , emptyNeighborData
  , meshChunk
  , meshChunkWithLight
  , BlockVertex(..)
  ) where

import World.Block
import World.Chunk
import World.Light

import Linear (V2(..), V3(..))
import Data.Word (Word8, Word32)
import Data.IORef
import Control.Monad (when, forM_)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
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

-- | Key for greedy meshing: block type and quantized light must match to merge.
data FaceKey = FaceKey
  { fkBlockType :: !BlockType
  , fkLightQ    :: !Word8
  } deriving stock (Eq)

fkLightVal :: FaceKey -> Float
fkLightVal fk = max 0.1 (fromIntegral (fkLightQ fk) / 15.0)

-- | Neighbor coordinate for a given face direction.
neighborCoord :: Int -> Int -> Int -> BlockFace -> (Int, Int, Int)
neighborCoord x y z FaceTop    = (x, y+1, z)
neighborCoord x y z FaceBottom = (x, y-1, z)
neighborCoord x y z FaceNorth  = (x, y, z+1)
neighborCoord x y z FaceSouth  = (x, y, z-1)
neighborCoord x y z FaceEast   = (x+1, y, z)
neighborCoord x y z FaceWest   = (x-1, y, z)
{-# INLINE neighborCoord #-}

-- | Generate mesh for a chunk using greedy meshing (no lighting).
--   Merges adjacent coplanar faces with the same block type into larger quads.
meshChunk :: Chunk -> IO MeshData
meshChunk chunk = do
  blocksVec <- freezeBlocks chunk
  let lightLookup _ _ _ _ = 15 :: Word8
      lookupBlockFn = lookupBlockSimple blocksVec
  greedyMeshAll blocksVec lookupBlockFn lightLookup

-- | Generate mesh for a chunk with light map data using greedy meshing.
--   Merges adjacent faces with the same block type and light level.
--   Light levels are passed through the AO vertex attribute (0.0 = dark, 1.0 = full light).
--   Neighbor data enables cross-chunk face culling at chunk edges.
meshChunkWithLight :: Chunk -> LightMap -> NeighborData -> IO MeshData
meshChunkWithLight chunk lm nd = do
  blocksVec <- freezeBlocks chunk

  -- Precompute light values for each face direction into frozen vectors.
  lightTop    <- precomputeLight lm (\x y z -> (x, y+1, z))
  lightBottom <- precomputeLight lm (\x y z -> (x, y-1, z))
  lightNorth  <- precomputeLight lm (\x y z -> (x, y, z+1))
  lightSouth  <- precomputeLight lm (\x y z -> (x, y, z-1))
  lightEast   <- precomputeLight lm (\x y z -> (x+1, y, z))
  lightWest   <- precomputeLight lm (\x y z -> (x-1, y, z))

  let lightLookup :: Int -> Int -> Int -> BlockFace -> Word8
      lightLookup x y z face =
        let idx = blockIndex x y z
        in case face of
             FaceTop    -> lightTop    `UV.unsafeIndex` idx
             FaceBottom -> lightBottom `UV.unsafeIndex` idx
             FaceNorth  -> lightNorth  `UV.unsafeIndex` idx
             FaceSouth  -> lightSouth  `UV.unsafeIndex` idx
             FaceEast   -> lightEast   `UV.unsafeIndex` idx
             FaceWest   -> lightWest   `UV.unsafeIndex` idx

      lookupBlockFn = lookupBlockWithNeighbors blocksVec nd

  greedyMeshAll blocksVec lookupBlockFn lightLookup

-- | Look up block type with neighbor chunk data for cross-boundary faces.
lookupBlockWithNeighbors :: UV.Vector Word8 -> NeighborData -> Int -> Int -> Int -> BlockType
lookupBlockWithNeighbors blocksVec nd' nx ny nz
  | ny < 0 || ny >= chunkHeight = Air
  | nx >= chunkWidth = case ndEast nd' of
      Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex 0 ny nz
      Nothing -> Air
  | nx < 0 = case ndWest nd' of
      Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex (chunkWidth - 1) ny nz
      Nothing -> Air
  | nz >= chunkDepth = case ndNorth nd' of
      Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny 0
      Nothing -> Air
  | nz < 0 = case ndSouth nd' of
      Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny (chunkDepth - 1)
      Nothing -> Air
  | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
{-# INLINE lookupBlockWithNeighbors #-}

-- | Simple block lookup: treats out-of-bounds as Air.
lookupBlockSimple :: UV.Vector Word8 -> Int -> Int -> Int -> BlockType
lookupBlockSimple blocksVec nx ny nz
  | not (isInBounds nx ny nz) = Air
  | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
{-# INLINE lookupBlockSimple #-}

-- | Precompute light values for one face direction.
precomputeLight :: LightMap -> (Int -> Int -> Int -> (Int, Int, Int)) -> IO (UV.Vector Word8)
precomputeLight lm offsetFn = do
  let size = chunkWidth * chunkDepth * chunkHeight
  mv <- MUV.new size
  let go !y
        | y >= chunkHeight = pure ()
        | otherwise = do
            let goZ !z
                  | z >= chunkDepth = go (y + 1)
                  | otherwise = do
                      let goX !x
                            | x >= chunkWidth = goZ (z + 1)
                            | otherwise = do
                                let (nx, ny, nz) = offsetFn x y z
                                ll <- getTotalLight lm nx ny nz
                                MUV.unsafeWrite mv (blockIndex x y z) ll
                                goX (x + 1)
                      goX 0
            goZ 0
  go 0
  UV.unsafeFreeze mv

-- | Dimensions for greedy meshing slices: (sliceCount, uSize, vSize).
sliceDimensions :: BlockFace -> (Int, Int, Int)
sliceDimensions FaceTop    = (chunkHeight, chunkWidth, chunkDepth)
sliceDimensions FaceBottom = (chunkHeight, chunkWidth, chunkDepth)
sliceDimensions FaceNorth  = (chunkDepth,  chunkWidth, chunkHeight)
sliceDimensions FaceSouth  = (chunkDepth,  chunkWidth, chunkHeight)
sliceDimensions FaceEast   = (chunkWidth,  chunkDepth, chunkHeight)
sliceDimensions FaceWest   = (chunkWidth,  chunkDepth, chunkHeight)

-- | Convert slice coordinates (face, depth, u, v) to world coordinates (x, y, z).
sliceToWorld :: BlockFace -> Int -> Int -> Int -> (Int, Int, Int)
sliceToWorld FaceTop    d u v = (u, d, v)
sliceToWorld FaceBottom d u v = (u, d, v)
sliceToWorld FaceNorth  d u v = (u, v, d)
sliceToWorld FaceSouth  d u v = (u, v, d)
sliceToWorld FaceEast   d u v = (d, v, u)
sliceToWorld FaceWest   d u v = (d, v, u)
{-# INLINE sliceToWorld #-}

-- | Core greedy meshing for all 6 face directions.
--   For each 2D slice perpendicular to the face normal, scan left-to-right and
--   bottom-to-top, expanding each unvisited exposed face into the largest
--   rectangle of matching block type + light level, then emit a single quad.
greedyMeshAll
  :: UV.Vector Word8
  -> (Int -> Int -> Int -> BlockType)
  -> (Int -> Int -> Int -> BlockFace -> Word8)
  -> IO MeshData
greedyMeshAll blocksVec lookupBlockFn lightLookup = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  let emitQuad :: [BlockVertex] -> IO ()
      emitQuad verts = do
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

      isAirAt :: Int -> Int -> Int -> Bool
      isAirAt x y z =
        (toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z :: BlockType) == Air
      {-# INLINE isAirAt #-}

      isExposedAt :: Int -> Int -> Int -> Bool
      isExposedAt nx ny nz = isTransparent (lookupBlockFn nx ny nz)
      {-# INLINE isExposedAt #-}

      mkKey :: Int -> Int -> Int -> BlockFace -> FaceKey
      mkKey x y z face =
        let bt = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z
            ll = lightLookup x y z face
        in FaceKey bt ll
      {-# INLINE mkKey #-}

  forM_ allBlockFaces $ \face -> do
    let (sliceCount, uSize, vSize) = sliceDimensions face
    visited <- MUV.new (uSize * vSize)
    forM_ [0..sliceCount-1] $ \d -> do
      MUV.set visited False
      forM_ [0..vSize-1] $ \v ->
        forM_ [0..uSize-1] $ \u -> do
          let visIdx = u + v * uSize
          alreadyVisited <- MUV.unsafeRead visited visIdx
          when (not alreadyVisited) $ do
            let (x, y, z) = sliceToWorld face d u v
            when (isInBounds x y z) $ do
              let (nx, ny, nz) = neighborCoord x y z face
              when (not (isAirAt x y z) && isExposedAt nx ny nz) $ do
                let key = mkKey x y z face

                -- Greedy expand along u (width)
                let findWidth !wu
                      | wu >= uSize = wu
                      | otherwise =
                          let (wx, wy, wz) = sliceToWorld face d wu v
                              (wnx, wny, wnz) = neighborCoord wx wy wz face
                          in if not (isInBounds wx wy wz)
                                || isAirAt wx wy wz
                                || not (isExposedAt wnx wny wnz)
                                || mkKey wx wy wz face /= key
                             then wu
                             else findWidth (wu + 1)
                    width = findWidth u - u

                -- Greedy expand along v (height)
                let findHeight !hv
                      | hv >= vSize = hv
                      | otherwise =
                          let rowOk = checkRow u
                              checkRow !cu
                                | cu >= u + width = True
                                | otherwise =
                                    let (cx, cy, cz) = sliceToWorld face d cu hv
                                        (cnx, cny, cnz) = neighborCoord cx cy cz face
                                    in isInBounds cx cy cz
                                       && not (isAirAt cx cy cz)
                                       && isExposedAt cnx cny cnz
                                       && mkKey cx cy cz face == key
                                       && checkRow (cu + 1)
                          in if rowOk then findHeight (hv + 1) else hv
                    height = findHeight v - v

                -- Mark merged cells as visited
                forM_ [v..v+height-1] $ \mv' ->
                  forM_ [u..u+width-1] $ \mu ->
                    MUV.unsafeWrite visited (mu + mv' * uSize) True

                -- Emit the merged quad with tiled UVs
                let bt = fkBlockType key
                    lightVal = fkLightVal key
                    (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
                    V2 u0' v0' = uv0
                    V2 u1' v1' = uv1
                    tileW = u1' - u0'
                    tileH = v1' - v0'
                    su1 = u0' + tileW * fromIntegral width
                    sv1 = v0' + tileH * fromIntegral height
                    (bx, by, bz) = sliceToWorld face d u v

                emitQuad (greedyFaceVertices
                            (fromIntegral bx) (fromIntegral by) (fromIntegral bz)
                            face (fromIntegral width) (fromIntegral height)
                            u0' v0' su1 sv1 lightVal)

  verts   <- VS.fromList . concat . reverse <$> readIORef vertsRef
  indices <- VS.fromList . concat . reverse <$> readIORef indicesRef
  pure $ MeshData verts indices

-- | Generate 4 vertices for a greedy-merged face quad.
--   w = extent along the slice u-axis, h = extent along the slice v-axis.
greedyFaceVertices :: Float -> Float -> Float -> BlockFace
                   -> Float -> Float
                   -> Float -> Float -> Float -> Float
                   -> Float
                   -> [BlockVertex]
greedyFaceVertices x y z face w h u0 v0 u1 v1 ao = case face of
  FaceTop -> -- +Y face (y+1 plane), u=X, v=Z
    [ BlockVertex (V3 x       (y+1) z      ) normal (V2 u0 v0) ao
    , BlockVertex (V3 (x+w)   (y+1) z      ) normal (V2 u1 v0) ao
    , BlockVertex (V3 (x+w)   (y+1) (z+h)  ) normal (V2 u1 v1) ao
    , BlockVertex (V3 x       (y+1) (z+h)  ) normal (V2 u0 v1) ao
    ]
    where normal = V3 0 1 0

  FaceBottom -> -- -Y face (y plane), u=X, v=Z
    [ BlockVertex (V3 x       y (z+h)  ) normal (V2 u0 v0) ao
    , BlockVertex (V3 (x+w)   y (z+h)  ) normal (V2 u1 v0) ao
    , BlockVertex (V3 (x+w)   y z      ) normal (V2 u1 v1) ao
    , BlockVertex (V3 x       y z      ) normal (V2 u0 v1) ao
    ]
    where normal = V3 0 (-1) 0

  FaceNorth -> -- +Z face (z+1 plane), u=X, v=Y
    [ BlockVertex (V3 (x+w) y       (z+1)) normal (V2 u0 v1) ao
    , BlockVertex (V3 x     y       (z+1)) normal (V2 u1 v1) ao
    , BlockVertex (V3 x     (y+h)   (z+1)) normal (V2 u1 v0) ao
    , BlockVertex (V3 (x+w) (y+h)   (z+1)) normal (V2 u0 v0) ao
    ]
    where normal = V3 0 0 1

  FaceSouth -> -- -Z face (z plane), u=X, v=Y
    [ BlockVertex (V3 x     y       z) normal (V2 u0 v1) ao
    , BlockVertex (V3 (x+w) y       z) normal (V2 u1 v1) ao
    , BlockVertex (V3 (x+w) (y+h)   z) normal (V2 u1 v0) ao
    , BlockVertex (V3 x     (y+h)   z) normal (V2 u0 v0) ao
    ]
    where normal = V3 0 0 (-1)

  FaceEast -> -- +X face (x+1 plane), u=Z, v=Y
    [ BlockVertex (V3 (x+1) y       z    ) normal (V2 u0 v1) ao
    , BlockVertex (V3 (x+1) y       (z+w)) normal (V2 u1 v1) ao
    , BlockVertex (V3 (x+1) (y+h)   (z+w)) normal (V2 u1 v0) ao
    , BlockVertex (V3 (x+1) (y+h)   z    ) normal (V2 u0 v0) ao
    ]
    where normal = V3 1 0 0

  FaceWest -> -- -X face (x plane), u=Z, v=Y
    [ BlockVertex (V3 x y       (z+w)) normal (V2 u0 v1) ao
    , BlockVertex (V3 x y       z    ) normal (V2 u1 v1) ao
    , BlockVertex (V3 x (y+h)   z    ) normal (V2 u1 v0) ao
    , BlockVertex (V3 x (y+h)   (z+w)) normal (V2 u0 v1) ao
    ]
    where normal = V3 (-1) 0 0
