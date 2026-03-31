module World.Biome
  ( BiomeType(..)
  , BiomeParams(..)
  , biomeParams
  , selectBiome
  , biomeAt
  ) where

import World.Block (BlockType(..))
import World.Noise (Seed, fractalNoise2D)

-- | Biome types
data BiomeType
  = Plains
  | Forest
  | Desert
  | Mountains
  | Ocean
  | Tundra
  | Savanna
  | Swamp
  | Taiga
  deriving stock (Eq, Ord, Enum, Bounded, Show)

-- | Per-biome terrain parameters
data BiomeParams = BiomeParams
  { bpBaseHeight    :: !Double    -- base terrain height (sea level = 64)
  , bpHeightScale   :: !Double    -- amplitude of terrain variation
  , bpSurfaceBlock  :: !BlockType -- top block
  , bpSubBlock      :: !BlockType -- block below surface (3-5 layers)
  , bpTreeDensity   :: !Double    -- probability of tree per surface block (0-1)
  , bpHasSnow       :: !Bool
  } deriving stock (Show, Eq)

-- | Get parameters for a biome
biomeParams :: BiomeType -> BiomeParams
biomeParams = \case
  Plains    -> BiomeParams 64 6  Grass Dirt  0.002 False
  Forest    -> BiomeParams 66 8  Grass Dirt  0.08  False
  Desert    -> BiomeParams 64 4  Sand  Sand  0.0   False
  Mountains -> BiomeParams 80 40 Stone Stone 0.005 False
  Ocean     -> BiomeParams 40 6  Sand  Sand  0.0   False
  Tundra    -> BiomeParams 64 4  Snow  Dirt  0.001 True
  Savanna   -> BiomeParams 66 5  Grass Dirt  0.01  False
  Swamp     -> BiomeParams 60 3  Grass Dirt  0.02  False
  Taiga     -> BiomeParams 66 10 Grass Dirt  0.04  True

-- | Select biome from temperature and humidity values.
--   Temperature and humidity are in [-1, 1].
selectBiome :: Double -> Double -> BiomeType
selectBiome temp humidity
  | temp < -0.5 && humidity > 0.0  = Tundra
  | temp < -0.5                    = Taiga
  | temp < 0.0  && humidity > 0.3  = Swamp
  | temp < 0.0  && humidity < -0.3 = Mountains
  | temp < 0.0                     = Forest
  | temp < 0.5  && humidity > 0.2  = Plains
  | temp < 0.5  && humidity < -0.4 = Ocean
  | temp < 0.5                     = Savanna
  | humidity < -0.2                = Desert
  | otherwise                      = Plains

-- | Get biome at a world (x, z) position using temperature/humidity noise maps.
biomeAt :: Seed -> Double -> Double -> BiomeType
biomeAt seed wx wz =
  let -- Temperature and humidity use different seeds and large scale
      tempSeed = seed + 1000
      humSeed  = seed + 2000
      scale    = 0.003  -- large-scale variation (~300 blocks per biome)
      temp     = fractalNoise2D tempSeed 4 2.0 0.5 (wx * scale) (wz * scale)
      humidity = fractalNoise2D humSeed  4 2.0 0.5 (wx * scale) (wz * scale)
  in selectBiome temp humidity
