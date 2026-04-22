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
import qualified Data.Vector.Storable.Mutable as VSM
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

-- | Greedy-mesh key: identifies a face cell for merging.
--   Encodes block type (Word8), texture tile (u8, u8), and quantised light (Word8).
--   Zero means "no face" (empty cell).
type FaceKey = Word32

-- | Pack a face key from block type, tile coords, and light.
packFaceKey :: BlockType -> V2 Int -> Float -> FaceKey
packFaceKey bt (V2 tu tv) lightVal =
  let b = fromIntegral (fromEnum bt) :: Word32
      u = fromIntegral tu :: Word32
      v = fromIntegral tv :: Word32
      l = round (lightVal * 255) :: Word32
  in (b `shiftL24`) .|. (u `shiftL16`) .|. (v `shiftL8`) .|. l
  where
    shiftL24 x = x * 16777216    -- 2^24
    shiftL16 x = x * 65536       -- 2^16
    shiftL8  x = x * 256         -- 2^8
    (.|.)       = (+)             -- fields don't overlap since each < 256
{-# INLINE packFaceKey #-}

-- | Unpack block type from a face key.
unpackBlockType :: FaceKey -> BlockType
unpackBlockType k = toEnum (fromIntegral (k `div` 16777216))
{-# INLINE unpackBlockType #-}

-- | Unpack light value (0.0-1.0) from a face key.
unpackLight :: FaceKey -> Float
unpackLight k = fromIntegral (k `mod` 256) / 255.0
{-# INLINE unpackLight #-}

-- | Generate mesh for a chunk using naive face culling (no light data).
meshChunk :: Chunk -> IO MeshData
meshChunk chunk = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  let addFace :: V3 Float -> BlockFace -> BlockType -> IO ()
      addFace (V3 bx by bz) face bt = do
        let (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0
            V2 u1 v1 = uv1
            ao = 1.0
            (verts, _normal) = faceVertices bx by bz face u0 v0 u1 v1 ao
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

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

-- | Generate mesh for a chunk with light map data using greedy meshing.
--   Adjacent faces with the same block type, texture, and light level are
--   merged into larger quads. UVs scale with quad size so the texture tiles.
--   Neighbor data enables cross-chunk face culling at chunk boundaries.
meshChunkWithLight :: Chunk -> LightMap -> NeighborData -> IO MeshData
meshChunkWithLight chunk lm neighbors = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  blocksVec <- freezeBlocks chunk

  let -- | Look up a block type at the given local coords, consulting neighbor
      -- data when the coordinate falls outside this chunk's bounds.
      lookupBlock :: Int -> Int -> Int -> BlockType
      lookupBlock nx ny nz
        | ny < 0 || ny >= chunkHeight = Air
        | nx >= chunkWidth = case ndEast neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex 0 ny nz
            Nothing -> Air
        | nx < 0 = case ndWest neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex (chunkWidth - 1) ny nz
            Nothing -> Air
        | nz >= chunkDepth = case ndNorth neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny 0
            Nothing -> Air
        | nz < 0 = case ndSouth neighbors of
            Just nv -> toEnum . fromIntegral $ nv `UV.unsafeIndex` blockIndex nx ny (chunkDepth - 1)
            Nothing -> Air
        | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
      {-# INLINE lookupBlock #-}

  -- Emit a merged quad: given face, origin in block coords, width/height along
  -- the two axes of the face plane, block type, and light value.
  let emitGreedyQuad :: BlockFace -> Int -> Int -> Int -> Int -> Int -> BlockType -> Float -> IO ()
      emitGreedyQuad face sliceCoord u0i v0i du dv bt lightVal = do
        let tileCoords = blockFaceTexCoords bt face
            (V2 tu0 tv0, V2 tu1 tv1) = tileUV tileCoords
            -- Scale UVs by quad size so texture tiles across merged faces
            tileW = tu1 - tu0
            tileH = tv1 - tv0
            su1 = tu0 + tileW * fromIntegral du
            sv1 = tv0 + tileH * fromIntegral dv
            -- Build the 4 corner positions depending on face direction
            (verts, _) = greedyFaceVertices face sliceCoord u0i v0i du dv tu0 tv0 su1 sv1 lightVal
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

  -- Process each face direction with greedy meshing
  -- For each face, iterate over slices perpendicular to the face normal.
  -- Within each slice, build a 2D mask and greedily merge rectangles.

  -- FaceTop (+Y): slices along Y, grid is X x Z
  greedyMeshFace blocksVec lm lookupBlock FaceTop
    chunkHeight chunkWidth chunkDepth
    (\slice u v -> (u, slice, v))    -- (x, y, z) from (slice, u, v)
    (\x y z -> (x, y+1, z))         -- neighbor to check
    emitGreedyQuad

  -- FaceBottom (-Y): slices along Y, grid is X x Z
  greedyMeshFace blocksVec lm lookupBlock FaceBottom
    chunkHeight chunkWidth chunkDepth
    (\slice u v -> (u, slice, v))
    (\x y z -> (x, y-1, z))
    emitGreedyQuad

  -- FaceEast (+X): slices along X, grid is Z x Y
  greedyMeshFace blocksVec lm lookupBlock FaceEast
    chunkWidth chunkDepth chunkHeight
    (\slice u v -> (slice, v, u))    -- (x, y, z) = (slice, v, u)
    (\x y z -> (x+1, y, z))
    emitGreedyQuad

  -- FaceWest (-X): slices along X, grid is Z x Y
  greedyMeshFace blocksVec lm lookupBlock FaceWest
    chunkWidth chunkDepth chunkHeight
    (\slice u v -> (slice, v, u))
    (\x y z -> (x-1, y, z))
    emitGreedyQuad

  -- FaceNorth (+Z): slices along Z, grid is X x Y
  greedyMeshFace blocksVec lm lookupBlock FaceNorth
    chunkDepth chunkWidth chunkHeight
    (\slice u v -> (u, v, slice))    -- (x, y, z) = (u, v, slice)
    (\x y z -> (x, y, z+1))
    emitGreedyQuad

  -- FaceSouth (-Z): slices along Z, grid is X x Y
  greedyMeshFace blocksVec lm lookupBlock FaceSouth
    chunkDepth chunkWidth chunkHeight
    (\slice u v -> (u, v, slice))
    (\x y z -> (x, y, z-1))
    emitGreedyQuad

  verts   <- VS.fromList . concat . reverse <$> readIORef vertsRef
  indices <- VS.fromList . concat . reverse <$> readIORef indicesRef
  pure $ MeshData verts indices

-- | Run greedy meshing for a single face direction.
--   Parameters:
--     blocksVec   - frozen chunk blocks
--     lm          - light map
--     lookupBlock - function to look up block type (with neighbor support)
--     face        - which face direction
--     sliceCount  - number of slices perpendicular to face normal
--     gridW       - width of the 2D grid within each slice
--     gridH       - height of the 2D grid within each slice
--     toXYZ       - convert (slice, u, v) to (x, y, z)
--     neighborOf  - given (x,y,z) return the neighbor coords to check
--     emitQuad    - callback to emit a merged quad
greedyMeshFace
  :: UV.Vector Word8
  -> LightMap
  -> (Int -> Int -> Int -> BlockType)
  -> BlockFace
  -> Int -> Int -> Int
  -> (Int -> Int -> Int -> (Int, Int, Int))
  -> (Int -> Int -> Int -> (Int, Int, Int))
  -> (BlockFace -> Int -> Int -> Int -> Int -> Int -> BlockType -> Float -> IO ())
  -> IO ()
greedyMeshFace blocksVec lm lookupBlock face sliceCount gridW gridH toXYZ neighborOf emitQuad = do
  -- Allocate a mutable mask array for one slice (gridW * gridH)
  mask <- MUV.new (gridW * gridH) :: IO (MUV.IOVector Word32)

  forM_ [0 .. sliceCount - 1] $ \slice -> do
    -- Fill the mask for this slice
    let fillMask !u !v
          | v >= gridH = pure ()
          | u >= gridW = fillMask 0 (v + 1)
          | otherwise = do
              let (x, y, z) = toXYZ slice u v
                  bt = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z
              if bt == Air
                then do
                  MUV.unsafeWrite mask (u + v * gridW) 0
                  fillMask (u + 1) v
                else do
                  let (nx, ny, nz) = neighborOf x y z
                      neighbor = lookupBlock nx ny nz
                  if isTransparent neighbor
                    then do
                      lightLevel <- getTotalLight lm nx ny nz
                      let lightVal = max 0.1 (fromIntegral lightLevel / 15.0)
                          tile = blockFaceTexCoords bt face
                          key = packFaceKey bt tile lightVal
                      MUV.unsafeWrite mask (u + v * gridW) key
                      fillMask (u + 1) v
                    else do
                      MUV.unsafeWrite mask (u + v * gridW) 0
                      fillMask (u + 1) v
    fillMask 0 0

    -- Greedy merge: scan the mask and extract maximal rectangles
    let greedyScan !u !v
          | v >= gridH = pure ()
          | u >= gridW = greedyScan 0 (v + 1)
          | otherwise = do
              key <- MUV.unsafeRead mask (u + v * gridW)
              if key == 0
                then greedyScan (u + 1) v
                else do
                  -- Expand width: find max du where all cells in row v from u..u+du-1 match key
                  du <- expandWidth mask gridW key u v gridW
                  -- Expand height: find max dv where all rows v..v+dv-1 have matching cells
                  dv <- expandHeight mask gridW gridH key u v du
                  -- Clear the merged region in the mask
                  clearMask mask gridW u v du dv
                  -- Emit the merged quad
                  let bt = unpackBlockType key
                      lightVal = unpackLight key
                  emitQuad face slice u v du dv bt lightVal
                  greedyScan (u + du) v
    greedyScan 0 0
{-# INLINE greedyMeshFace #-}

-- | Find the maximum width of matching cells starting at (u, v).
expandWidth :: MUV.IOVector Word32 -> Int -> Word32 -> Int -> Int -> Int -> IO Int
expandWidth mask gridW key u0 v maxW = go u0
  where
    go !u
      | u >= maxW = pure (u - u0)
      | otherwise = do
          k <- MUV.unsafeRead mask (u + v * gridW)
          if k == key
            then go (u + 1)
            else pure (u - u0)
{-# INLINE expandWidth #-}

-- | Find the maximum height of matching rows starting at (u, v) with width du.
expandHeight :: MUV.IOVector Word32 -> Int -> Int -> Word32 -> Int -> Int -> Int -> IO Int
expandHeight mask gridW gridH key u0 v0 du = go (v0 + 1)
  where
    go !v
      | v >= gridH = pure (v - v0)
      | otherwise = do
          ok <- checkRow mask gridW key u0 v du
          if ok
            then go (v + 1)
            else pure (v - v0)
{-# INLINE expandHeight #-}

-- | Check if all cells in a row match the key.
checkRow :: MUV.IOVector Word32 -> Int -> Word32 -> Int -> Int -> Int -> IO Bool
checkRow mask gridW key u0 v du = go u0
  where
    go !u
      | u >= u0 + du = pure True
      | otherwise = do
          k <- MUV.unsafeRead mask (u + v * gridW)
          if k == key
            then go (u + 1)
            else pure False
{-# INLINE checkRow #-}

-- | Clear a rectangular region in the mask.
clearMask :: MUV.IOVector Word32 -> Int -> Int -> Int -> Int -> Int -> IO ()
clearMask mask gridW u0 v0 du dv = go v0
  where
    go !v
      | v >= v0 + dv = pure ()
      | otherwise = do
          clearRow v u0
          go (v + 1)
    clearRow !v !u
      | u >= u0 + du = pure ()
      | otherwise = do
          MUV.unsafeWrite mask (u + v * gridW) 0
          clearRow v (u + 1)
{-# INLINE clearMask #-}

checkFaceVec :: UV.Vector Word8 -> V3 Float -> BlockFace -> BlockType
             -> (Int, Int, Int) -> (V3 Float -> BlockFace -> BlockType -> IO ()) -> IO ()
checkFaceVec blocksVec pos face bt (nx, ny, nz) addFace = do
  let neighbor
        | not (isInBounds nx ny nz) = Air
        | otherwise = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex nx ny nz
  if isTransparent neighbor
    then addFace pos face bt
    else pure ()
{-# INLINE checkFaceVec #-}

-- | Generate 4 vertices for a single-block face (used by meshChunk).
faceVertices :: Float -> Float -> Float -> BlockFace
             -> Float -> Float -> Float -> Float  -- u0 v0 u1 v1
             -> Float                              -- ao
             -> ([BlockVertex], V3 Float)
faceVertices x y z face u0 v0 u1 v1 ao = case face of
  FaceTop ->
    ( [ BlockVertex (V3 x     (y+1) z    ) normal (V2 u0 v0) ao
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u0 v1) ao
      ], normal)
    where normal = V3 0 1 0

  FaceBottom ->
    ( [ BlockVertex (V3 x     y (z+1)) normal (V2 u0 v0) ao
      , BlockVertex (V3 (x+1) y (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) y z    ) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     y z    ) normal (V2 u0 v1) ao
      ], normal)
    where normal = V3 0 (-1) 0

  FaceNorth ->
    ( [ BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u0 v1) ao
      , BlockVertex (V3 x     y     (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 0 0 1

  FaceSouth ->
    ( [ BlockVertex (V3 x     y     z) normal (V2 u0 v1) ao
      , BlockVertex (V3 (x+1) y     z) normal (V2 u1 v1) ao
      , BlockVertex (V3 (x+1) (y+1) z) normal (V2 u1 v0) ao
      , BlockVertex (V3 x     (y+1) z) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 0 0 (-1)

  FaceEast ->
    ( [ BlockVertex (V3 (x+1) y     z    ) normal (V2 u0 v1) ao
      , BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 1 0 0

  FaceWest ->
    ( [ BlockVertex (V3 x y     (z+1)) normal (V2 u0 v1) ao
      , BlockVertex (V3 x y     z    ) normal (V2 u1 v1) ao
      , BlockVertex (V3 x (y+1) z    ) normal (V2 u1 v0) ao
      , BlockVertex (V3 x (y+1) (z+1)) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 (-1) 0 0

-- | Generate 4 vertices for a greedy-merged quad.
--   Parameters:
--     face       - face direction
--     sliceCoord - coordinate along the face normal
--     u0, v0     - origin in the 2D slice grid
--     du, dv     - width and height of the merged quad
--     tu0, tv0   - UV origin (tile base)
--     tu1, tv1   - UV end (scaled by quad size)
--     lightVal   - light/AO value
greedyFaceVertices :: BlockFace -> Int -> Int -> Int -> Int -> Int
                   -> Float -> Float -> Float -> Float -> Float
                   -> ([BlockVertex], V3 Float)
greedyFaceVertices face sliceCoord u0 v0 du dv tu0 tv0 tu1 tv1 lightVal =
  let fi = fromIntegral
      -- The slice coord, u, and v map differently to (x,y,z) depending on face
  in case face of
    FaceTop ->
      -- slice=Y, u=X, v=Z; face plane at y+1
      let y1 = fi (sliceCoord + 1)
          x0 = fi u0; x1 = fi (u0 + du)
          z0 = fi v0; z1 = fi (v0 + dv)
          normal = V3 0 1 0
      in ( [ BlockVertex (V3 x0 y1 z0) normal (V2 tu0 tv0) lightVal
           , BlockVertex (V3 x1 y1 z0) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x1 y1 z1) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x0 y1 z1) normal (V2 tu0 tv1) lightVal
           ], normal)

    FaceBottom ->
      -- slice=Y, u=X, v=Z; face plane at y
      let y0f = fi sliceCoord
          x0 = fi u0; x1 = fi (u0 + du)
          z0 = fi v0; z1 = fi (v0 + dv)
          normal = V3 0 (-1) 0
      in ( [ BlockVertex (V3 x0 y0f z1) normal (V2 tu0 tv0) lightVal
           , BlockVertex (V3 x1 y0f z1) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x1 y0f z0) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x0 y0f z0) normal (V2 tu0 tv1) lightVal
           ], normal)

    FaceEast ->
      -- slice=X, u=Z, v=Y; face plane at x+1
      let x1 = fi (sliceCoord + 1)
          z0 = fi u0; z1 = fi (u0 + du)
          y0f = fi v0; y1 = fi (v0 + dv)
          normal = V3 1 0 0
      in ( [ BlockVertex (V3 x1 y0f z0) normal (V2 tu0 tv1) lightVal
           , BlockVertex (V3 x1 y0f z1) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x1 y1  z1) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x1 y1  z0) normal (V2 tu0 tv0) lightVal
           ], normal)

    FaceWest ->
      -- slice=X, u=Z, v=Y; face plane at x
      let x0 = fi sliceCoord
          z0 = fi u0; z1 = fi (u0 + du)
          y0f = fi v0; y1 = fi (v0 + dv)
          normal = V3 (-1) 0 0
      in ( [ BlockVertex (V3 x0 y0f z1) normal (V2 tu0 tv1) lightVal
           , BlockVertex (V3 x0 y0f z0) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x0 y1  z0) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x0 y1  z1) normal (V2 tu0 tv0) lightVal
           ], normal)

    FaceNorth ->
      -- slice=Z, u=X, v=Y; face plane at z+1
      let z1 = fi (sliceCoord + 1)
          x0 = fi u0; x1 = fi (u0 + du)
          y0f = fi v0; y1 = fi (v0 + dv)
          normal = V3 0 0 1
      in ( [ BlockVertex (V3 x1 y0f z1) normal (V2 tu0 tv1) lightVal
           , BlockVertex (V3 x0 y0f z1) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x0 y1  z1) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x1 y1  z1) normal (V2 tu0 tv0) lightVal
           ], normal)

    FaceSouth ->
      -- slice=Z, u=X, v=Y; face plane at z
      let z0 = fi sliceCoord
          x0 = fi u0; x1 = fi (u0 + du)
          y0f = fi v0; y1 = fi (v0 + dv)
          normal = V3 0 0 (-1)
      in ( [ BlockVertex (V3 x0 y0f z0) normal (V2 tu0 tv1) lightVal
           , BlockVertex (V3 x1 y0f z0) normal (V2 tu1 tv1) lightVal
           , BlockVertex (V3 x1 y1  z0) normal (V2 tu1 tv0) lightVal
           , BlockVertex (V3 x0 y1  z0) normal (V2 tu0 tv0) lightVal
           ], normal)
