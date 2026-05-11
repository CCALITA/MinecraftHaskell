module UI.BiomeDisplay
  ( biomeDisplayName
  , biomeNameFadeAlpha
  ) where

import World.Biome (BiomeType(..))

-- | Human-readable display name for each biome type.
biomeDisplayName :: BiomeType -> String
biomeDisplayName = \case
  Plains    -> "Plains"
  Forest    -> "Forest"
  Desert    -> "Desert"
  Mountains -> "Mountains"
  Ocean     -> "Ocean"
  Tundra    -> "Tundra"
  Savanna   -> "Savanna"
  Swamp     -> "Swamp"
  Taiga     -> "Taiga"

-- | Fade duration in seconds for the biome name overlay.
fadeDuration :: Float
fadeDuration = 3.0

-- | Compute the alpha value for the biome name overlay given the
--   time remaining on the display timer. Fades linearly from 1.0
--   to 0.0 over 'fadeDuration' seconds, clamped to [0, 1].
biomeNameFadeAlpha :: Float -> Float
biomeNameFadeAlpha timer
  | timer <= 0         = 0.0
  | timer >= fadeDuration = 1.0
  | otherwise          = timer / fadeDuration
