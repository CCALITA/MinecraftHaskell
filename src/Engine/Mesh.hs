module Engine.Mesh
  ( MeshData(..)
  , emptyMesh
  , meshChunk
  , BlockVertex(..)
  ) where

import World.Block
import World.Chunk

import Linear (V2(..), V3(..))
import Data.Word (Word32)
import Data.IORef
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
  vertsRef   <- newIORef ([] :: [BlockVertex])
  indicesRef <- newIORef ([] :: [Word32])
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
        modifyIORef' vertsRef (++ verts)
        modifyIORef' indicesRef (++ newIndices)
        writeIORef vertCount (vc + 4)

  -- Iterate over all blocks
  blocksVec <- readIORef (chunkBlocks chunk)
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
                -- Check each face: emit if neighbor is transparent
                checkFace chunk blocksVec x y z pos FaceTop    bt (x, y+1, z) addFace
                checkFace chunk blocksVec x y z pos FaceBottom bt (x, y-1, z) addFace
                checkFace chunk blocksVec x y z pos FaceNorth  bt (x, y, z+1) addFace
                checkFace chunk blocksVec x y z pos FaceSouth  bt (x, y, z-1) addFace
                checkFace chunk blocksVec x y z pos FaceEast   bt (x+1, y, z) addFace
                checkFace chunk blocksVec x y z pos FaceWest   bt (x-1, y, z) addFace
                go (x + 1) y z
  go 0 0 0

  verts   <- VS.fromList <$> readIORef vertsRef
  indices <- VS.fromList <$> readIORef indicesRef
  pure $ MeshData verts indices

-- Inline helper: check if neighbor is transparent, then add face
checkFace :: Chunk -> a -> Int -> Int -> Int -> V3 Float -> BlockFace -> BlockType
          -> (Int, Int, Int) -> (V3 Float -> BlockFace -> BlockType -> IO ()) -> IO ()
checkFace chunk _ _x _y _z pos face bt (nx, ny, nz) addFace = do
  neighbor <- getBlock chunk nx ny nz
  if isTransparent neighbor
    then addFace pos face bt
    else pure ()
{-# INLINE checkFace #-}

-- | Generate 4 vertices for a block face
faceVertices :: Float -> Float -> Float -> BlockFace
             -> Float -> Float -> Float -> Float  -- u0 v0 u1 v1
             -> Float                              -- ao
             -> ([BlockVertex], V3 Float)
faceVertices x y z face u0 v0 u1 v1 ao = case face of
  FaceTop -> -- +Y face (y+1 plane)
    ( [ BlockVertex (V3 x     (y+1) z    ) normal (V2 u0 v0) ao
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u0 v1) ao
      ], normal)
    where normal = V3 0 1 0

  FaceBottom -> -- -Y face (y plane)
    ( [ BlockVertex (V3 x     y (z+1)) normal (V2 u0 v0) ao
      , BlockVertex (V3 (x+1) y (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) y z    ) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     y z    ) normal (V2 u0 v1) ao
      ], normal)
    where normal = V3 0 (-1) 0

  FaceNorth -> -- +Z face
    ( [ BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u0 v1) ao
      , BlockVertex (V3 x     y     (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 0 0 1

  FaceSouth -> -- -Z face
    ( [ BlockVertex (V3 x     y     z) normal (V2 u0 v1) ao
      , BlockVertex (V3 (x+1) y     z) normal (V2 u1 v1) ao
      , BlockVertex (V3 (x+1) (y+1) z) normal (V2 u1 v0) ao
      , BlockVertex (V3 x     (y+1) z) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 0 0 (-1)

  FaceEast -> -- +X face
    ( [ BlockVertex (V3 (x+1) y     z    ) normal (V2 u0 v1) ao
      , BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u1 v1) ao
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v0) ao
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 1 0 0

  FaceWest -> -- -X face
    ( [ BlockVertex (V3 x y     (z+1)) normal (V2 u0 v1) ao
      , BlockVertex (V3 x y     z    ) normal (V2 u1 v1) ao
      , BlockVertex (V3 x (y+1) z    ) normal (V2 u1 v0) ao
      , BlockVertex (V3 x (y+1) (z+1)) normal (V2 u0 v0) ao
      ], normal)
    where normal = V3 (-1) 0 0
