module World.BiomeFeatures
  ( BiomeFeature(..)
  , TreeType(..)
  , biomeFeatures
  , treeDensity
  , oreMinY
  , oreMaxY
  , oreDensity
  , decorationChance
  , structureChance
  , featureTrees
  , featureOres
  , featureDecorations
  , featureStructures
  , treeLogBlock
  , treeLeafBlock
  ) where

import World.Biome (BiomeType(..))
import World.Block (BlockType(..))

-- | Types of trees that can be placed in the world.
data TreeType = OakTree | BirchTree | SpruceTree | JungleTree
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Configurable biome generation features.
data BiomeFeature
  = TreeFeature !TreeType !Float
    -- ^ Tree type and placement density (probability per surface block).
  | OreFeature !BlockType !Int !Int !Float
    -- ^ Ore block, minimum Y, maximum Y, noise density threshold.
  | DecorationFeature !BlockType !Float
    -- ^ Surface decoration block and chance per surface block.
  | StructureFeature !String !Float
    -- ^ Named structure and chance per chunk.
  deriving stock (Show, Eq)

-- | Extract tree density from a TreeFeature.
treeDensity :: BiomeFeature -> Float
treeDensity (TreeFeature _ d)  = d
treeDensity _                  = 0

-- | Extract minimum Y from an OreFeature.
oreMinY :: BiomeFeature -> Int
oreMinY (OreFeature _ minY _ _) = minY
oreMinY _                       = 0

-- | Extract maximum Y from an OreFeature.
oreMaxY :: BiomeFeature -> Int
oreMaxY (OreFeature _ _ maxY _) = maxY
oreMaxY _                       = 0

-- | Extract density threshold from an OreFeature.
oreDensity :: BiomeFeature -> Float
oreDensity (OreFeature _ _ _ d) = d
oreDensity _                    = 0

-- | Extract chance from a DecorationFeature.
decorationChance :: BiomeFeature -> Float
decorationChance (DecorationFeature _ c) = c
decorationChance _                       = 0

-- | Extract chance from a StructureFeature.
structureChance :: BiomeFeature -> Float
structureChance (StructureFeature _ c) = c
structureChance _                      = 0

-- | Filter only tree features from a feature list.
featureTrees :: [BiomeFeature] -> [BiomeFeature]
featureTrees = filter isTree
  where
    isTree (TreeFeature _ _) = True
    isTree _                 = False

-- | Filter only ore features from a feature list.
featureOres :: [BiomeFeature] -> [BiomeFeature]
featureOres = filter isOre
  where
    isOre (OreFeature {}) = True
    isOre _               = False

-- | Filter only decoration features from a feature list.
featureDecorations :: [BiomeFeature] -> [BiomeFeature]
featureDecorations = filter isDeco
  where
    isDeco (DecorationFeature _ _) = True
    isDeco _                       = False

-- | Filter only structure features from a feature list.
featureStructures :: [BiomeFeature] -> [BiomeFeature]
featureStructures = filter isStruct
  where
    isStruct (StructureFeature _ _) = True
    isStruct _                      = False

-- | Map a tree type to its log block.
treeLogBlock :: TreeType -> BlockType
treeLogBlock OakTree    = OakLog
treeLogBlock BirchTree  = OakLog   -- TODO: add BirchLog BlockType
treeLogBlock SpruceTree = OakLog   -- TODO: add SpruceLog BlockType
treeLogBlock JungleTree = OakLog   -- TODO: add JungleLog BlockType

-- | Map a tree type to its leaf block.
treeLeafBlock :: TreeType -> BlockType
treeLeafBlock OakTree    = OakLeaves
treeLeafBlock BirchTree  = OakLeaves  -- TODO: add BirchLeaves BlockType
treeLeafBlock SpruceTree = OakLeaves  -- TODO: add SpruceLeaves BlockType
treeLeafBlock JungleTree = OakLeaves  -- TODO: add JungleLeaves BlockType

-- | Common ore features shared across most biomes.
commonOres :: [BiomeFeature]
commonOres =
  [ OreFeature CoalOre    4 80 0.70
  , OreFeature IronOre    4 64 0.75
  , OreFeature GoldOre    4 32 0.80
  , OreFeature DiamondOre 4 16 0.85
  ]

-- | Get the list of generation features for a given biome.
biomeFeatures :: BiomeType -> [BiomeFeature]
biomeFeatures Plains =
  [ TreeFeature OakTree 0.002
  , DecorationFeature Grass 0.03
  ] <> commonOres
    <> [ StructureFeature "village" 0.005 ]

biomeFeatures Forest =
  [ TreeFeature OakTree 0.06
  , TreeFeature BirchTree 0.02
  , DecorationFeature Grass 0.05
  ] <> commonOres

biomeFeatures Desert =
  [ DecorationFeature Cactus 0.01
  , DecorationFeature Sand 0.02
  ] <> commonOres
    <> [ StructureFeature "desert_temple" 0.002
       , StructureFeature "desert_well" 0.004
       ]

biomeFeatures Mountains =
  [ TreeFeature SpruceTree 0.005
  , DecorationFeature Snow 0.04
  , DecorationFeature Gravel 0.02
  ] <> commonOres
    <> [ OreFeature IronOre 32 96 0.72 ]

biomeFeatures Ocean =
  [ DecorationFeature Clay 0.03
  , DecorationFeature Gravel 0.02
  ] <> commonOres

biomeFeatures Tundra =
  [ TreeFeature SpruceTree 0.001
  , DecorationFeature Snow 0.06
  ] <> commonOres

biomeFeatures Savanna =
  [ TreeFeature OakTree 0.01
  , DecorationFeature Grass 0.04
  ] <> commonOres
    <> [ StructureFeature "village" 0.003 ]

biomeFeatures Swamp =
  [ TreeFeature OakTree 0.02
  , DecorationFeature Clay 0.02
  , DecorationFeature Grass 0.03
  ] <> commonOres
    <> [ StructureFeature "witch_hut" 0.001 ]

biomeFeatures Taiga =
  [ TreeFeature SpruceTree 0.04
  , DecorationFeature Snow 0.03
  , DecorationFeature Grass 0.02
  ] <> commonOres
