module Engine.TerrainColor
  ( biomeGrassColor
  , biomeFoliageColor
  , biomeWaterColor
  ) where

import World.Biome (BiomeType(..))

-- | Grass tint color per biome, as an (R, G, B) triple in [0,1].
biomeGrassColor :: BiomeType -> (Float, Float, Float)
biomeGrassColor = \case
  Plains    -> (0.55, 0.78, 0.33)  -- bright green
  Forest    -> (0.40, 0.70, 0.25)  -- rich green
  Desert    -> (0.75, 0.72, 0.30)  -- yellow-green
  Mountains -> (0.50, 0.65, 0.35)  -- muted green
  Ocean     -> (0.45, 0.70, 0.30)  -- medium green (islands)
  Tundra    -> (0.60, 0.72, 0.55)  -- pale frosty green
  Savanna   -> (0.70, 0.72, 0.28)  -- dry golden-green
  Swamp     -> (0.40, 0.55, 0.20)  -- murky dark green
  Taiga     -> (0.35, 0.55, 0.28)  -- dark conifer green

-- | Foliage (leaves, vines) tint color per biome.
biomeFoliageColor :: BiomeType -> (Float, Float, Float)
biomeFoliageColor = \case
  Plains    -> (0.47, 0.70, 0.27)
  Forest    -> (0.35, 0.62, 0.20)
  Desert    -> (0.68, 0.65, 0.25)
  Mountains -> (0.42, 0.58, 0.30)
  Ocean     -> (0.40, 0.65, 0.25)
  Tundra    -> (0.52, 0.65, 0.48)
  Savanna   -> (0.62, 0.65, 0.22)
  Swamp     -> (0.35, 0.48, 0.18)
  Taiga     -> (0.30, 0.50, 0.22)

-- | Water tint color per biome.
biomeWaterColor :: BiomeType -> (Float, Float, Float)
biomeWaterColor = \case
  Plains    -> (0.24, 0.45, 0.82)  -- standard blue
  Forest    -> (0.24, 0.45, 0.82)  -- standard blue
  Desert    -> (0.32, 0.55, 0.72)  -- light warm blue
  Mountains -> (0.20, 0.40, 0.78)  -- cool blue
  Ocean     -> (0.15, 0.35, 0.80)  -- deep ocean blue
  Tundra    -> (0.30, 0.50, 0.75)  -- icy pale blue
  Savanna   -> (0.28, 0.50, 0.70)  -- warm muted blue
  Swamp     -> (0.38, 0.50, 0.35)  -- murky green-brown
  Taiga     -> (0.22, 0.42, 0.70)  -- dark cool blue
