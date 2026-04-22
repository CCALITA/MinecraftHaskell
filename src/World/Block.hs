module World.Block
  ( BlockType(..)
  , BlockFace(..)
  , BlockProperties(..)
  , blockProperties
  , isTransparent
  , isSolid
  , isGravityAffected
  , isLeafBlock
  , isWheatCropBlock
  , blockCollisionHeight
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
  | Wool
  | FenceGateClosed
  | FenceGateOpen
  | Lever
  | RedstoneDust
  | TrapdoorClosed
  | TrapdoorOpen
  | StoneStairs
  | OakStairs
  | IronDoorClosed
  | IronDoorOpen
  | Fire
  | Cactus
  | SugarCane
  | StoneSlab
  | OakSlab
  | Piston
  | PistonHead
  | Rail
  | Dispenser
  | EnchantingTable
  | Netherrack
  | SoulSand
  | Glowstone
  | NetherBrick
  | NetherPortal
  | RedstoneOre
  | LapisOre
  | EmeraldOre
  | MossyCobblestone
  | MossyStoneBrick
  | BirchLog
  | BirchLeaves
  | BirchPlanks
  | SpruceLog
  | SpruceLeaves
  | SprucePlanks
  | JungleLog
  | JungleLeaves
  | JunglePlanks
  | TallGrass
  | Dandelion
  | Rose
  | BrownMushroom
  | RedMushroom
  | WheatCrop1
  | WheatCrop2
  | WheatCrop3
  | WheatCrop4
  | WheatCrop5
  | WheatCrop6
  | WheatCrop7
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
  Wool         -> BlockProperties True  False 0  0.8
  FenceGateClosed -> BlockProperties True  True  0  2.0
  FenceGateOpen   -> BlockProperties False True  0  2.0
  Lever        -> BlockProperties True  False 0  0.5
  RedstoneDust -> BlockProperties False True  0  0
  TrapdoorClosed -> BlockProperties True  True  0  3.0
  TrapdoorOpen   -> BlockProperties False True  0  3.0
  StoneStairs  -> BlockProperties True  False 0  1.5
  OakStairs    -> BlockProperties True  False 0  2.0
  IronDoorClosed -> BlockProperties True  False 0  5.0
  IronDoorOpen -> BlockProperties False True  0  5.0
  Fire         -> BlockProperties False True  15 0.0
  Cactus       -> BlockProperties True  True  0  0.4
  SugarCane    -> BlockProperties False True  0  0
  StoneSlab    -> BlockProperties True  True  0  1.5
  OakSlab      -> BlockProperties True  True  0  2.0
  Piston       -> BlockProperties True  False 0  0.5
  PistonHead   -> BlockProperties True  False 0  0.5
  Rail         -> BlockProperties False True  0  0.7
  Dispenser    -> BlockProperties True  False 0  3.5
  EnchantingTable -> BlockProperties True  False 7  5.0
  Netherrack      -> BlockProperties True  False 0  0.4
  SoulSand        -> BlockProperties True  False 0  0.5
  Glowstone       -> BlockProperties True  False 15 0.3
  NetherBrick     -> BlockProperties True  False 0  2.0
  NetherPortal    -> BlockProperties False True  11 0    -- unbreakable portal
  RedstoneOre     -> BlockProperties True  False 0  3.0
  LapisOre        -> BlockProperties True  False 0  3.0
  EmeraldOre      -> BlockProperties True  False 0  3.0
  MossyCobblestone -> BlockProperties True  False 0  2.0
  MossyStoneBrick -> BlockProperties True  False 0  1.5
  BirchLog         -> BlockProperties True  False 0  2.0
  BirchLeaves      -> BlockProperties True  True  0  0.2
  BirchPlanks      -> BlockProperties True  False 0  2.0
  SpruceLog        -> BlockProperties True  False 0  2.0
  SpruceLeaves     -> BlockProperties True  True  0  0.2
  SprucePlanks     -> BlockProperties True  False 0  2.0
  JungleLog        -> BlockProperties True  False 0  2.0
  JungleLeaves     -> BlockProperties True  True  0  0.2
  JunglePlanks     -> BlockProperties True  False 0  2.0
  TallGrass        -> BlockProperties False True  0  0.0
  Dandelion        -> BlockProperties False True  0  0.0
  Rose             -> BlockProperties False True  0  0.0
  BrownMushroom    -> BlockProperties False True  0  0.0
  RedMushroom      -> BlockProperties False True  0  0.0
  WheatCrop1       -> BlockProperties False True  0  0.0
  WheatCrop2       -> BlockProperties False True  0  0.0
  WheatCrop3       -> BlockProperties False True  0  0.0
  WheatCrop4       -> BlockProperties False True  0  0.0
  WheatCrop5       -> BlockProperties False True  0  0.0
  WheatCrop6       -> BlockProperties False True  0  0.0
  WheatCrop7       -> BlockProperties False True  0  0.0

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
isLeafBlock OakLeaves    = True
isLeafBlock BirchLeaves  = True
isLeafBlock SpruceLeaves = True
isLeafBlock JungleLeaves = True
isLeafBlock _            = False

-- | Whether a block is a wheat crop at any growth stage
isWheatCropBlock :: BlockType -> Bool
isWheatCropBlock WheatCrop  = True
isWheatCropBlock WheatCrop1 = True
isWheatCropBlock WheatCrop2 = True
isWheatCropBlock WheatCrop3 = True
isWheatCropBlock WheatCrop4 = True
isWheatCropBlock WheatCrop5 = True
isWheatCropBlock WheatCrop6 = True
isWheatCropBlock WheatCrop7 = True
isWheatCropBlock _          = False

-- | Collision height for a block type. Slabs are half-height (0.5), all others are 1.0.
blockCollisionHeight :: BlockType -> Float
blockCollisionHeight StoneStairs = 0.5
blockCollisionHeight OakStairs   = 0.5
blockCollisionHeight StoneSlab = 0.5
blockCollisionHeight OakSlab   = 0.5
blockCollisionHeight _         = 1.0

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
  Wool        -> V2 0 3
  FenceGateClosed -> V2 7 2
  FenceGateOpen   -> V2 8 2
  Lever       -> V2 9 2
  RedstoneDust -> V2 10 2
  TrapdoorClosed -> V2 14 2
  TrapdoorOpen   -> V2 15 2
  StoneStairs  -> V2 1 0
  OakStairs    -> V2 4 0
  IronDoorClosed -> V2 1 5
  IronDoorOpen -> V2 2 5
  Fire        -> V2 11 2
  Cactus      -> V2 3 5
  SugarCane   -> V2 4 5
  StoneSlab   -> V2 1 0
  OakSlab     -> V2 4 0
  Piston      -> case face of
    FaceTop    -> V2 5 5   -- stone face on top
    FaceBottom -> V2 4 0   -- wooden base on bottom
    _          -> V2 6 5   -- wooden side
  PistonHead  -> V2 7 5    -- flat wooden panel
  Rail        -> V2 8 5
  Dispenser   -> case face of
    FaceSouth  -> V2 9 5  -- front face with opening
    _          -> V2 14 3 -- stone-like sides (reuse furnace top)
  EnchantingTable -> case face of
    FaceTop    -> V2 5 5   -- enchanting table top (diamond-studded)
    FaceBottom -> V2 11 1  -- obsidian bottom
    _          -> V2 6 5   -- enchanting table side
  Netherrack       -> V2 10 5
  SoulSand         -> V2 11 5
  Glowstone        -> V2 12 5
  NetherBrick      -> V2 13 5
  NetherPortal     -> V2 14 5
  RedstoneOre      -> V2 3 3
  LapisOre         -> V2 4 3
  EmeraldOre       -> V2 5 3
  MossyCobblestone -> V2 3 4
  MossyStoneBrick  -> V2 4 4
  BirchLog      -> case face of
    FaceTop    -> V2 1 6   -- birch log cross-section
    FaceBottom -> V2 1 6
    _          -> V2 0 6   -- birch log bark
  BirchLeaves  -> V2 2 6
  BirchPlanks  -> V2 3 6
  SpruceLog    -> case face of
    FaceTop    -> V2 5 6   -- spruce log cross-section
    FaceBottom -> V2 5 6
    _          -> V2 4 6   -- spruce log bark
  SpruceLeaves -> V2 6 6
  SprucePlanks -> V2 7 6
  JungleLog    -> case face of
    FaceTop    -> V2 9 6   -- jungle log cross-section
    FaceBottom -> V2 9 6
    _          -> V2 8 6   -- jungle log bark
  JungleLeaves -> V2 10 6
  JunglePlanks -> V2 11 6
  TallGrass    -> V2 12 6
  Dandelion    -> V2 13 6
  Rose         -> V2 14 6
  BrownMushroom -> V2 15 6
  RedMushroom  -> V2 0 7
  WheatCrop1   -> V2 1 7
  WheatCrop2   -> V2 2 7
  WheatCrop3   -> V2 3 7
  WheatCrop4   -> V2 4 7
  WheatCrop5   -> V2 5 7
  WheatCrop6   -> V2 6 7
  WheatCrop7   -> V2 7 7
