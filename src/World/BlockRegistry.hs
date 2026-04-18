module World.BlockRegistry
  ( BlockDef(..)
  , BlockRegistry
  , newBlockRegistry
  , registerBlock
  , lookupBlock
  , registeredBlocks
  , mkBlockDef
  , defaultBlockRegistry
  ) where

import World.Block (BlockType(..), BlockProperties(..), BlockFace(..), blockProperties, blockFaceTexCoords, blockCollisionHeight)
import Game.Item (Item(..), ToolType(..), blockDrops, blockPreferredTool, blockRequiredHarvestLevel)
import Linear (V2(..))
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Data.Word (Word8)

-- | Complete definition for a block type, consolidating properties
-- from Block.hs and Item.hs into a single record.
data BlockDef = BlockDef
  { bdProperties      :: !BlockProperties
  , bdTexCoords       :: !(BlockFace -> V2 Int)
  , bdDrops           :: ![(Item, Int)]
  , bdPreferredTool   :: !(Maybe ToolType)
  , bdHarvestLevel    :: !Int
  , bdCollisionHeight :: !Float
  }

-- | Mutable registry mapping Word8 block IDs to their definitions.
type BlockRegistry = IORef (HM.HashMap Word8 BlockDef)

-- | Convert a BlockType to its Word8 key for the registry map.
blockKey :: BlockType -> Word8
blockKey = fromIntegral . fromEnum

-- | Create an empty block registry.
newBlockRegistry :: IO BlockRegistry
newBlockRegistry = newIORef HM.empty

-- | Register a block definition for a given block type.
-- Overwrites any previous definition for the same type.
registerBlock :: BlockRegistry -> BlockType -> BlockDef -> IO ()
registerBlock reg bt def =
  modifyIORef' reg (HM.insert (blockKey bt) def)

-- | Look up the definition for a block type.
lookupBlock :: BlockRegistry -> BlockType -> IO (Maybe BlockDef)
lookupBlock reg bt =
  HM.lookup (blockKey bt) <$> readIORef reg

-- | List all registered (blockId, definition) pairs.
registeredBlocks :: BlockRegistry -> IO [(Word8, BlockDef)]
registeredBlocks reg = HM.toList <$> readIORef reg

-- | Build a BlockDef from the existing pattern-match functions in
-- Block.hs and Item.hs. Useful for populating the registry from
-- current block types and for tests.
mkBlockDef :: BlockType -> BlockDef
mkBlockDef bt = BlockDef
  { bdProperties      = blockProperties bt
  , bdTexCoords       = blockFaceTexCoords bt
  , bdDrops           = blockDrops bt
  , bdPreferredTool   = blockPreferredTool bt
  , bdHarvestLevel    = blockRequiredHarvestLevel bt
  , bdCollisionHeight = blockCollisionHeight bt
  }

-- | Pre-populate a registry with all existing block types.
-- Builds the entire map in one pass rather than inserting one-by-one.
defaultBlockRegistry :: IO BlockRegistry
defaultBlockRegistry =
  newIORef $ HM.fromList
    [ (blockKey bt, mkBlockDef bt) | bt <- [minBound .. maxBound] ]
