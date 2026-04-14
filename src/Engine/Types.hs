{-# LANGUAGE DuplicateRecordFields #-}
module Engine.Types
  ( Vertex(..)
  , UniformBufferObject(..)
  , FrameData(..)
  , EngineConfig(..)
  , defaultEngineConfig
  ) where

import Linear (V2, V3, V4, M44)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import GHC.Generics (Generic)

-- | Vertex with position, color, and texture coordinates
data Vertex = Vertex
  { vPosition :: !(V3 Float)
  , vColor    :: !(V3 Float)
  , vTexCoord :: !(V2 Float)
  } deriving stock (Show, Eq, Generic)

instance Storable Vertex where
  sizeOf _ = 3 * sizeOf (undefined :: V3 Float) + sizeOf (undefined :: V2 Float)
  alignment _ = alignment (undefined :: Float)
  peek ptr = do
    let p = castPtr ptr
    pos   <- peekByteOff p 0
    col   <- peekByteOff p 12
    tex   <- peekByteOff p 24
    pure $ Vertex pos col tex
  poke ptr (Vertex pos col tex) = do
    let p = castPtr ptr
    pokeByteOff p 0  pos
    pokeByteOff p 12 col
    pokeByteOff p 24 tex

-- | Uniform buffer for MVP matrices and lighting
data UniformBufferObject = UniformBufferObject
  { uboModel        :: !(M44 Float)
  , uboView         :: !(M44 Float)
  , uboProjection   :: !(M44 Float)
  , uboSunDirection :: !(V4 Float)    -- xyz = sun dir, w = unused (padding)
  , uboAmbientLight :: !Float         -- 0.0-1.0 ambient brightness
  , _uboPad1        :: !Float         -- padding to 16-byte alignment
  , _uboPad2        :: !Float
  , _uboPad3        :: !Float
  } deriving stock (Show, Eq, Generic)

instance Storable UniformBufferObject where
  sizeOf _ = 3 * sizeOf (undefined :: M44 Float) + sizeOf (undefined :: V4 Float) + 4 * sizeOf (undefined :: Float)  -- 192 + 16 + 16 = 224
  alignment _ = alignment (undefined :: Float)
  peek ptr = do
    let p = castPtr ptr
    m <- peekByteOff p 0
    v <- peekByteOff p 64
    proj <- peekByteOff p 128
    sunDir <- peekByteOff p 192
    ambient <- peekByteOff p 208
    pure $ UniformBufferObject m v proj sunDir ambient 0 0 0
  poke ptr (UniformBufferObject m v proj sunDir ambient _ _ _) = do
    let p = castPtr ptr
    pokeByteOff p 0   m
    pokeByteOff p 64  v
    pokeByteOff p 128 proj
    pokeByteOff p 192 sunDir
    pokeByteOff p 208 ambient
    pokeByteOff p 212 (0 :: Float)
    pokeByteOff p 216 (0 :: Float)
    pokeByteOff p 220 (0 :: Float)

-- | Per-frame synchronization and command data
data FrameData = FrameData
  { fdImageAvailableSemaphore :: !()  -- Placeholder: will be Vk.Semaphore
  , fdRenderFinishedSemaphore :: !()
  , fdInFlightFence           :: !()
  } deriving stock (Show, Eq)

-- | Engine configuration
data EngineConfig = EngineConfig
  { ecWindowWidth   :: !Int
  , ecWindowHeight  :: !Int
  , ecWindowTitle   :: !String
  , ecMaxFramesInFlight :: !Int
  , ecEnableValidation  :: !Bool
  } deriving stock (Show, Eq, Generic)

defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig
  { ecWindowWidth       = 1280
  , ecWindowHeight      = 720
  , ecWindowTitle       = "Minecraft Haskell"
  , ecMaxFramesInFlight = 2
  , ecEnableValidation  = False
  }
