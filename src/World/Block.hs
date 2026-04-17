module World.Block
  ( BlockType(..)
  , BlockFace(..)
  , BlockProperties(..)
  , blockProperties
  , isTransparent
  , isSolid
  , isGravityAffected
  , isLeafBlock
  , blockFaceTexCoords
  , allBlockFaces
  ) where

import Data.Word (Word8)
import Linear (V2(..))

-- | Block types represented as Word8 for compact chunk storage
data BlockType
  = Air
  | Stone
  | Dirt
  | Grass
  | Sand
  | Gravel
  | OakLog
  | OakLeaves
  | Water
  | Lava
  | Cobblestone
  | OakPlanks
  | Glass
  | Bedrock
  | IronOre
  | CoalOre
  | GoldOre
  | DiamondOre
  | Snow
  | Clay
  | CraftingTable
  | Furnace
  | Chest
  | Torch
  | StoneBrick
  | Brick
  | TNT
  | Obsidian
  | OakDoorClosed
  | OakDoorOpen
  | Ladder
  | Bed
  | OakFence
  | Farmland
  | WheatCrop
  | OakSapling
  | Lever
  | RedstoneDust
  deriving stock (Eq, Ord, Enum, Bounded, Show, Read)

-- | Convert BlockType to/from Word8 for chunk storage
blockToWord8 :: BlockType -> Word8
blockToWord8 = fromIntegral . fromEnum

word8ToBlock :: Word8 -> BlockType
word8ToBlock = toEnum . fromIntegral

-- | Six faces of a cube
data BlockFace
  = FaceTop      -- +Y
  | FaceBottom   -- -Y
  | FaceNorth    -- +Z
  | FaceSouth    -- -Z
  | FaceEast     -- +X
  | FaceWest     -- -X
  deriving stock (Eq, Ord, Enum, Bounded, Show)

allBlockFaces :: [BlockFace]
allBlockFaces = [FaceTop, FaceBottom, FaceNorth, FaceSouth, FaceEast, FaceWest]

-- | Block properties
data BlockProperties = BlockProperties
  { bpSolid       :: !Bool      -- blocks movement
  , bpTransparent :: !Bool      -- light passes through, adjacent faces rendered
  , bpLightEmit   :: !Word8     -- light level emitted (0-15)
  , bpHardness    :: !Float     -- time to break (seconds)
  } deriving stock (Show, Eq)

-- | Get properties for a block type
blockProperties :: BlockType -> BlockProperties
blockProperties = \case
  Air          -> BlockProperties False True  0  0
  Stone        -> BlockProperties True  False 0  1.5
  Dirt         -> BlockProperties True  False 0  0.5
  Grass        -> BlockProperties True  False 0  0.6
  Sand         -> BlockProperties True  False 0  0.5
  Gravel       -> BlockProperties True  False 0  0.6
  OakLog       -> BlockProperties True  False 0  2.0
  OakLeaves    -> BlockProperties True  True  0  0.2
  Water        -> BlockProperties False True  0  0
  Lava         -> BlockProperties False True  15 0
  Cobblestone  -> BlockProperties True  False 0  2.0
  OakPlanks    -> BlockProperties True  False 0  2.0
  Glass        -> BlockProperties True  True  0  0.3
  Bedrock      -> BlockProperties True  False 0  (-1)  -- unbreakable
  IronOre      -> BlockProperties True  False 0  3.0
  CoalOre      -> BlockProperties True  False 0  3.0
  GoldOre      -> BlockProperties True  False 0  3.0
  DiamondOre   -> BlockProperties True  False 0  3.0
  Snow         -> BlockProperties True  False 0  0.1
  Clay         -> BlockProperties True  False 0  0.6
  CraftingTable -> BlockProperties True  False 0  2.5
  Furnace      -> BlockProperties True  False 13 3.5
  Chest        -> BlockProperties True  False 0  2.5
  Torch        -> BlockProperties False True  14 0
  StoneBrick   -> BlockProperties True  False 0  1.5
  Brick        -> BlockProperties True  False 0  2.0
  TNT          -> BlockProperties True  False 0  0
  Obsidian     -> BlockProperties True  False 0  50.0
  OakDoorClosed -> BlockProperties True  False 0  3.0
  OakDoorOpen  -> BlockProperties False True  0  3.0
  Ladder       -> BlockProperties False True  0  0.4
  Bed          -> BlockProperties True  False 0  0.2
  OakFence     -> BlockProperties True  True  0  2.0
  Farmland     -> BlockProperties True  False 0  0.6
  WheatCrop    -> BlockProperties False True  0  0
  OakSapling   -> BlockProperties False True  0  0
  Lever        -> BlockProperties True  False 0  0.5
  RedstoneDust -> BlockProperties False True  0  0

isTransparent :: BlockType -> Bool
isTransparent = bpTransparent . blockProperties

isSolid :: BlockType -> Bool
isSolid = bpSolid . blockProperties

-- | Whether a block is affected by gravity (falls when air is below)
isGravityAffected :: BlockType -> Bool
isGravityAffected Sand   = True
isGravityAffected Gravel = True
isGravityAffected _      = False

-- | Whether a block is a leaf block (subject to decay)
isLeafBlock :: BlockType -> Bool
isLeafBlock OakLeaves = True
isLeafBlock _         = False

-- | Texture atlas coordinates for each block face.
--   Returns (u, v) tile position in a 16x16 texture atlas.
--   Each tile is 1/16th of the atlas (16px in a 256px atlas).
blockFaceTexCoords :: BlockType -> BlockFace -> V2 Int
blockFaceTexCoords blockType face = case blockType of
  Air         -> V2 0 0  -- never rendered
  Stone       -> V2 1 0
  Dirt        -> V2 2 0
  Grass       -> case face of
    FaceTop    -> V2 0 0   -- grass top
    FaceBottom -> V2 2 0   -- dirt
    _          -> V2 3 0   -- grass side
  Sand        -> V2 4 0
  Gravel      -> V2 5 0
  OakLog      -> case face of
    FaceTop    -> V2 5 1   -- log cross-section
    FaceBottom -> V2 5 1
    _          -> V2 4 1   -- log bark
  OakLeaves   -> V2 6 1
  Water       -> V2 13 0
  Lava        -> V2 14 0
  Cobblestone -> V2 0 1
  OakPlanks   -> V2 4 0
  Glass       -> V2 1 3
  Bedrock     -> V2 1 1
  IronOre     -> V2 1 2
  CoalOre     -> V2 2 2
  GoldOre     -> V2 0 2
  DiamondOre  -> V2 2 3
  Snow        -> V2 2 4
  Clay        -> V2 8 4
  CraftingTable -> case face of
    FaceTop    -> V2 11 3  -- crafting table top
    FaceBottom -> V2 4 0   -- planks bottom
    _          -> V2 11 2  -- crafting table side
  Furnace     -> case face of
    FaceTop    -> V2 14 3  -- furnace top
    FaceBottom -> V2 14 3
    FaceSouth  -> V2 12 2  -- furnace front
    _          -> V2 13 2  -- furnace side
  Chest       -> V2 9 1
  Torch       -> V2 0 5
  StoneBrick  -> V2 6 3
  Brick       -> V2 7 0
  TNT         -> case face of
    FaceTop    -> V2 9 0
    FaceBottom -> V2 10 0
    _          -> V2 8 0
  Obsidian    -> V2 11 1
  OakDoorClosed -> V2 12 1
  OakDoorOpen -> V2 13 1
  Ladder      -> V2 14 1
  Bed         -> V2 15 1
  OakFence    -> V2 3 2
  Farmland    -> case face of
    FaceTop    -> V2 4 2
    FaceBottom -> V2 2 0  -- dirt bottom
    _          -> V2 2 0  -- dirt sides
  WheatCrop   -> V2 5 2
  OakSapling  -> V2 6 2
  Lever       -> V2 7 2
  RedstoneDust -> V2 8 2
