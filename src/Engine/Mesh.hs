module Engine.Mesh
  ( MeshData(..)
  , NeighborData(..)
  , emptyMesh
  , emptyNeighborData
  , meshChunk
  , meshChunkWithLight
  , meshChunkWithLightSplit
  , BlockVertex(..)
  , computeVertexAO
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

-- | Compute ambient occlusion for a single vertex.
--   Takes three neighbor solidity flags: side1, side2, corner.
--   If both sides are solid, the corner is forced solid (Minecraft AO convention).
--   Returns a factor in [0, 1] where 1.0 = no occlusion, 0.0 = fully occluded.
computeVertexAO :: Bool -> Bool -> Bool -> Float
computeVertexAO side1 side2 corner =
  let cornerEffective = corner || (side1 && side2)
      solidCount = countTrue side1 + countTrue side2 + countTrue cornerEffective
  in fromIntegral (3 - solidCount) / 3.0
  where
    countTrue :: Bool -> Int
    countTrue True  = 1
    countTrue False = 0
{-# INLINE computeVertexAO #-}

-- | Neighbor offsets for AO computation per face vertex.
--   Each face has 4 vertices, each vertex needs 3 neighbors: (side1, side2, corner).
--   Offsets are relative to the block position in (dx, dy, dz) form.
--   The vertex order matches 'faceVertices'.
faceAONeighbors :: BlockFace -> [((Int,Int,Int), (Int,Int,Int), (Int,Int,Int))]
faceAONeighbors FaceTop =
  [ ((-1,1, 0), ( 0,1,-1), (-1,1,-1))
  , (( 1,1, 0), ( 0,1,-1), ( 1,1,-1))
  , (( 1,1, 0), ( 0,1, 1), ( 1,1, 1))
  , ((-1,1, 0), ( 0,1, 1), (-1,1, 1))
  ]
faceAONeighbors FaceBottom =
  [ ((-1,-1, 0), ( 0,-1, 1), (-1,-1, 1))
  , (( 1,-1, 0), ( 0,-1, 1), ( 1,-1, 1))
  , (( 1,-1, 0), ( 0,-1,-1), ( 1,-1,-1))
  , ((-1,-1, 0), ( 0,-1,-1), (-1,-1,-1))
  ]
faceAONeighbors FaceNorth =
  [ (( 1, 0, 1), ( 0,-1, 1), ( 1,-1, 1))
  , ((-1, 0, 1), ( 0,-1, 1), (-1,-1, 1))
  , ((-1, 0, 1), ( 0, 1, 1), (-1, 1, 1))
  , (( 1, 0, 1), ( 0, 1, 1), ( 1, 1, 1))
  ]
faceAONeighbors FaceSouth =
  [ ((-1, 0,-1), ( 0,-1,-1), (-1,-1,-1))
  , (( 1, 0,-1), ( 0,-1,-1), ( 1,-1,-1))
  , (( 1, 0,-1), ( 0, 1,-1), ( 1, 1,-1))
  , ((-1, 0,-1), ( 0, 1,-1), (-1, 1,-1))
  ]
faceAONeighbors FaceEast =
  [ (( 1, 0,-1), ( 1,-1, 0), ( 1,-1,-1))
  , (( 1, 0, 1), ( 1,-1, 0), ( 1,-1, 1))
  , (( 1, 0, 1), ( 1, 1, 0), ( 1, 1, 1))
  , (( 1, 0,-1), ( 1, 1, 0), ( 1, 1,-1))
  ]
faceAONeighbors FaceWest =
  [ ((-1, 0, 1), (-1,-1, 0), (-1,-1, 1))
  , ((-1, 0,-1), (-1,-1, 0), (-1,-1,-1))
  , ((-1, 0,-1), (-1, 1, 0), (-1, 1,-1))
  , ((-1, 0, 1), (-1, 1, 0), (-1, 1, 1))
  ]

-- | Generate mesh for a chunk using naive face culling.
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
            ao = (1.0, 1.0, 1.0, 1.0)
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

-- | Generate mesh for a chunk with light map data and per-vertex ambient occlusion.
--   AO is computed by checking 3 neighbor blocks per vertex (side1, side2, corner).
--   Neighbor data enables cross-chunk face culling.
meshChunkWithLight :: Chunk -> LightMap -> NeighborData -> IO MeshData
meshChunkWithLight chunk lm neighbors = do
  vertsRef   <- newIORef ([] :: [[BlockVertex]])
  indicesRef <- newIORef ([] :: [[Word32]])
  vertCount  <- newIORef (0 :: Word32)

  let addFace :: V3 Float -> BlockFace -> BlockType -> (Float, Float, Float, Float) -> IO ()
      addFace (V3 bx by bz) face bt aoValues = do
        let (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0
            V2 u1 v1 = uv1
            (verts, _normal) = faceVertices bx by bz face u0 v0 u1 v1 aoValues
        vc <- readIORef vertCount
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vertsRef (verts :)
        modifyIORef' indicesRef (newIndices :)
        writeIORef vertCount (vc + 4)

  blocksVec <- freezeBlocks chunk
  let lookupBlock :: Int -> Int -> Int -> BlockType
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

      computeFaceAO :: Int -> Int -> Int -> BlockFace -> Float -> (Float, Float, Float, Float)
      computeFaceAO bx by bz face lightVal =
        let aoNeighbors = faceAONeighbors face
            computeOne ((dx1,dy1,dz1), (dx2,dy2,dz2), (dxc,dyc,dzc)) =
              let s1 = isSolid (lookupBlock (bx+dx1) (by+dy1) (bz+dz1))
                  s2 = isSolid (lookupBlock (bx+dx2) (by+dy2) (bz+dz2))
                  cn = isSolid (lookupBlock (bx+dxc) (by+dyc) (bz+dzc))
                  aoFactor = computeVertexAO s1 s2 cn
              in lightVal * aoFactor
        in case aoNeighbors of
             [n0, n1, n2, n3] -> (computeOne n0, computeOne n1, computeOne n2, computeOne n3)
             _                -> (lightVal, lightVal, lightVal, lightVal)
      {-# INLINE computeFaceAO #-}

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
                            aoValues = computeFaceAO x y z face lightVal
                        addFace pos face bt aoValues
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

-- | Generate split meshes for a chunk: opaque geometry and transparent geometry.
--   Alpha blocks (water, glass, leaves, ice) go into the transparent mesh.
--   Faces between two identical alpha blocks are culled (e.g. water-water).
meshChunkWithLightSplit :: Chunk -> LightMap -> NeighborData -> IO (MeshData, MeshData)
meshChunkWithLightSplit chunk lm neighbors = do
  opaqueVertsRef   <- newIORef ([] :: [[BlockVertex]])
  opaqueIndicesRef <- newIORef ([] :: [[Word32]])
  opaqueVertCount  <- newIORef (0 :: Word32)
  transVertsRef    <- newIORef ([] :: [[BlockVertex]])
  transIndicesRef  <- newIORef ([] :: [[Word32]])
  transVertCount   <- newIORef (0 :: Word32)

  let addFaceTo :: IORef [[BlockVertex]] -> IORef [[Word32]] -> IORef Word32
                -> V3 Float -> BlockFace -> BlockType
                -> (Float, Float, Float, Float) -> IO ()
      addFaceTo vRef iRef vcRef (V3 bx by bz) face bt aoValues = do
        let (uv0, uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0
            V2 u1 v1 = uv1
            (verts, _normal) = faceVertices bx by bz face u0 v0 u1 v1 aoValues
        vc <- readIORef vcRef
        let newIndices = [ vc, vc+1, vc+2, vc+2, vc+3, vc ]
        modifyIORef' vRef (verts :)
        modifyIORef' iRef (newIndices :)
        writeIORef vcRef (vc + 4)

  blocksVec <- freezeBlocks chunk
  let lookupBlock' :: Int -> Int -> Int -> BlockType
      lookupBlock' nx ny nz
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
      {-# INLINE lookupBlock' #-}

      computeFaceAO' :: Int -> Int -> Int -> BlockFace -> Float -> (Float, Float, Float, Float)
      computeFaceAO' bx by bz face lightVal =
        let aoNeighbors = faceAONeighbors face
            computeOne ((dx1,dy1,dz1), (dx2,dy2,dz2), (dxc,dyc,dzc)) =
              let s1 = isSolid (lookupBlock' (bx+dx1) (by+dy1) (bz+dz1))
                  s2 = isSolid (lookupBlock' (bx+dx2) (by+dy2) (bz+dz2))
                  cn = isSolid (lookupBlock' (bx+dxc) (by+dyc) (bz+dzc))
                  aoFactor = computeVertexAO s1 s2 cn
              in lightVal * aoFactor
        in case aoNeighbors of
             [n0, n1, n2, n3] -> (computeOne n0, computeOne n1, computeOne n2, computeOne n3)
             _                -> (lightVal, lightVal, lightVal, lightVal)
      {-# INLINE computeFaceAO' #-}

      go' !x !y !z
        | y >= chunkHeight = pure ()
        | z >= chunkDepth  = go' 0 (y + 1) 0
        | x >= chunkWidth  = go' 0 y (z + 1)
        | otherwise = do
            let bt = toEnum . fromIntegral $ blocksVec `UV.unsafeIndex` blockIndex x y z
            if bt == Air
              then go' (x + 1) y z
              else do
                let bx = fromIntegral x
                    by = fromIntegral y
                    bz = fromIntegral z
                    pos = V3 bx by bz
                    alpha = isAlphaBlock bt
                    (vRef, iRef, vcRef) = if alpha
                      then (transVertsRef, transIndicesRef, transVertCount)
                      else (opaqueVertsRef, opaqueIndicesRef, opaqueVertCount)
                    checkFaceLight' nx ny nz face = do
                      let neighbor = lookupBlock' nx ny nz
                      -- Emit face when neighbor is transparent, but skip
                      -- faces between two identical alpha blocks (e.g. water-water)
                      when (isTransparent neighbor && (not alpha || bt /= neighbor)) $ do
                        lightLevel <- getTotalLight lm nx ny nz
                        let lightVal = max 0.1 (fromIntegral lightLevel / 15.0)
                            aoValues = computeFaceAO' x y z face lightVal
                        addFaceTo vRef iRef vcRef pos face bt aoValues
                checkFaceLight' x (y+1) z FaceTop
                checkFaceLight' x (y-1) z FaceBottom
                checkFaceLight' x y (z+1) FaceNorth
                checkFaceLight' x y (z-1) FaceSouth
                checkFaceLight' (x+1) y z FaceEast
                checkFaceLight' (x-1) y z FaceWest
                go' (x + 1) y z
  go' 0 0 0

  opaqueVerts   <- VS.fromList . concat . reverse <$> readIORef opaqueVertsRef
  opaqueIndices <- VS.fromList . concat . reverse <$> readIORef opaqueIndicesRef
  transVerts    <- VS.fromList . concat . reverse <$> readIORef transVertsRef
  transIndices  <- VS.fromList . concat . reverse <$> readIORef transIndicesRef
  pure (MeshData opaqueVerts opaqueIndices, MeshData transVerts transIndices)

meshChunkWithLightSplit :: Chunk -> LightMap -> NeighborData -> IO (MeshData, MeshData)
meshChunkWithLightSplit chunk lm neighbors = do
  oVR <- newIORef ([] :: [[BlockVertex]]); oIR <- newIORef ([] :: [[Word32]]); oVC <- newIORef (0 :: Word32)
  tVR <- newIORef ([] :: [[BlockVertex]]); tIR <- newIORef ([] :: [[Word32]]); tVC <- newIORef (0 :: Word32)
  let addF ref iref vcref (V3 bx by bz) face bt aoV = do
        let (uv0,uv1) = tileUV (blockFaceTexCoords bt face)
            V2 u0 v0 = uv0; V2 u1 v1 = uv1
            (vs,_) = faceVertices bx by bz face u0 v0 u1 v1 aoV
        vc <- readIORef vcref
        modifyIORef' ref (vs :); modifyIORef' iref ([vc,vc+1,vc+2,vc+2,vc+3,vc] :); writeIORef vcref (vc+4)
  blocksVec <- freezeBlocks chunk
  let lb nx ny nz
        | ny<0||ny>=chunkHeight = Air
        | nx>=chunkWidth = case ndEast neighbors of {Just nv -> toEnum.fromIntegral$ nv`UV.unsafeIndex`blockIndex 0 ny nz; Nothing -> Air}
        | nx<0 = case ndWest neighbors of {Just nv -> toEnum.fromIntegral$ nv`UV.unsafeIndex`blockIndex(chunkWidth-1)ny nz; Nothing -> Air}
        | nz>=chunkDepth = case ndNorth neighbors of {Just nv -> toEnum.fromIntegral$ nv`UV.unsafeIndex`blockIndex nx ny 0; Nothing -> Air}
        | nz<0 = case ndSouth neighbors of {Just nv -> toEnum.fromIntegral$ nv`UV.unsafeIndex`blockIndex nx ny(chunkDepth-1); Nothing -> Air}
        | otherwise = toEnum.fromIntegral$ blocksVec`UV.unsafeIndex`blockIndex nx ny nz
      {-# INLINE lb #-}
      cAO bx by bz face lv = let ns=faceAONeighbors face; co((dx1,dy1,dz1),(dx2,dy2,dz2),(dxc,dyc,dzc))=lv*computeVertexAO(isSolid$lb(bx+dx1)(by+dy1)(bz+dz1))(isSolid$lb(bx+dx2)(by+dy2)(bz+dz2))(isSolid$lb(bx+dxc)(by+dyc)(bz+dzc)) in case ns of {[n0,n1,n2,n3]->(co n0,co n1,co n2,co n3);_->(lv,lv,lv,lv)}
      go !x !y !z
        | y>=chunkHeight = pure()
        | z>=chunkDepth = go 0(y+1)0
        | x>=chunkWidth = go 0 y(z+1)
        | otherwise = do
            let bt = toEnum.fromIntegral$ blocksVec`UV.unsafeIndex`blockIndex x y z
            if bt==Air then go(x+1)y z else do
              let bx=fromIntegral x; by=fromIntegral y; bz=fromIntegral z; pos=V3 bx by bz
                  (vr,ir,vc) = if isTranslucent bt then (tVR,tIR,tVC) else (oVR,oIR,oVC)
                  cfl nx ny nz face = do
                    let nb=lb nx ny nz
                    when(isTransparent nb && (not(isTranslucent bt)||bt/=nb))$ do
                      ll <- getTotalLight lm nx ny nz
                      let lv=max 0.1(fromIntegral ll/15.0)
                      addF vr ir vc pos face bt (cAO x y z face lv)
              cfl x(y+1)z FaceTop; cfl x(y-1)z FaceBottom; cfl x y(z+1)FaceNorth
              cfl x y(z-1)FaceSouth; cfl(x+1)y z FaceEast; cfl(x-1)y z FaceWest
              go(x+1)y z
  go 0 0 0
  ov<-VS.fromList.concat.reverse<$>readIORef oVR; oi<-VS.fromList.concat.reverse<$>readIORef oIR
  tv<-VS.fromList.concat.reverse<$>readIORef tVR; ti<-VS.fromList.concat.reverse<$>readIORef tIR
  pure(MeshData ov oi, MeshData tv ti)

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

-- | Generate 4 vertices for a block face with per-vertex AO values
faceVertices :: Float -> Float -> Float -> BlockFace
             -> Float -> Float -> Float -> Float
             -> (Float, Float, Float, Float)
             -> ([BlockVertex], V3 Float)
faceVertices x y z face u0 v0 u1 v1 (ao0, ao1, ao2, ao3) = case face of
  FaceTop ->
    ( [ BlockVertex (V3 x     (y+1) z    ) normal (V2 u0 v0) ao0
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u1 v0) ao1
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v1) ao2
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u0 v1) ao3
      ], normal)
    where normal = V3 0 1 0
  FaceBottom ->
    ( [ BlockVertex (V3 x     y (z+1)) normal (V2 u0 v0) ao0
      , BlockVertex (V3 (x+1) y (z+1)) normal (V2 u1 v0) ao1
      , BlockVertex (V3 (x+1) y z    ) normal (V2 u1 v1) ao2
      , BlockVertex (V3 x     y z    ) normal (V2 u0 v1) ao3
      ], normal)
    where normal = V3 0 (-1) 0
  FaceNorth ->
    ( [ BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u0 v1) ao0
      , BlockVertex (V3 x     y     (z+1)) normal (V2 u1 v1) ao1
      , BlockVertex (V3 x     (y+1) (z+1)) normal (V2 u1 v0) ao2
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 0 0 1
  FaceSouth ->
    ( [ BlockVertex (V3 x     y     z) normal (V2 u0 v1) ao0
      , BlockVertex (V3 (x+1) y     z) normal (V2 u1 v1) ao1
      , BlockVertex (V3 (x+1) (y+1) z) normal (V2 u1 v0) ao2
      , BlockVertex (V3 x     (y+1) z) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 0 0 (-1)
  FaceEast ->
    ( [ BlockVertex (V3 (x+1) y     z    ) normal (V2 u0 v1) ao0
      , BlockVertex (V3 (x+1) y     (z+1)) normal (V2 u1 v1) ao1
      , BlockVertex (V3 (x+1) (y+1) (z+1)) normal (V2 u1 v0) ao2
      , BlockVertex (V3 (x+1) (y+1) z    ) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 1 0 0
  FaceWest ->
    ( [ BlockVertex (V3 x y     (z+1)) normal (V2 u0 v1) ao0
      , BlockVertex (V3 x y     z    ) normal (V2 u1 v1) ao1
      , BlockVertex (V3 x (y+1) z    ) normal (V2 u1 v0) ao2
      , BlockVertex (V3 x (y+1) (z+1)) normal (V2 u0 v0) ao3
      ], normal)
    where normal = V3 (-1) 0 0
