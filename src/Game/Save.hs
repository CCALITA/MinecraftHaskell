module Game.Save
  ( SaveData(..)
  , saveWorld
  , loadWorld
  , savePlayer
  , loadPlayer
  , saveTileEntities
  , loadTileEntities
  , listSaves
  , nextSaveName
  , worldSavePath
  , playerSavePath
  ) where

import World.Block (BlockType(..))
import World.Chunk (Chunk(..), ChunkPos, chunkWidth, chunkDepth, chunkHeight, newChunk, blockIndex, freezeBlocks)
import World.World (World(..))
import Game.Player (Player(..))
import Game.Item (Item(..), ToolType(..), ToolMaterial(..), MaterialType(..))
import Game.Inventory (ItemStack(..))
import Game.TileEntity

import Control.Monad (unless, when, mapM_, filterM)

import Control.Concurrent.STM (readTVarIO, atomically, writeTVar)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.IORef
import Data.Word (Word8)
import Data.Binary (Binary(..), encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Data.List (sort, stripPrefix)
import Linear (V2(..), V3(..))
import GHC.Generics (Generic)

-- Binary instances for tile entity serialization
instance Binary BlockType
instance Binary ToolType
instance Binary ToolMaterial
instance Binary MaterialType
instance Binary Item
instance Binary ItemStack
instance Binary FurnaceVariant
instance Binary FurnaceState
instance Binary HopperDirection
instance Binary TileEntity

instance Binary HopperState where
  put hs = do
    put (V.toList (hsSlots hs))
    put (hsDirection hs)
    put (hsCooldown hs)
  get = do
    slots <- get
    dir <- get
    cd <- get
    pure HopperState { hsSlots = V.fromList slots, hsDirection = dir, hsCooldown = cd }

-- | Top-level save data
data SaveData = SaveData
  { sdPlayerPos    :: !(Float, Float, Float)
  , sdPlayerYaw    :: !Float
  , sdPlayerPitch  :: !Float
  , sdPlayerFlying :: !Bool
  , sdWorldSeed    :: !Int
  , sdDayTime      :: !Float
  } deriving stock (Show, Eq, Generic)

instance Binary SaveData

-- | Serialized chunk data
data ChunkSave = ChunkSave
  { csPos    :: !(Int, Int)        -- chunk position
  , csBlocks :: ![Word8]           -- flat block array
  } deriving stock (Show, Eq, Generic)

instance Binary ChunkSave

-- | Save directory path
worldSavePath :: FilePath -> FilePath
worldSavePath saveDir = saveDir </> "world"

playerSavePath :: FilePath -> FilePath
playerSavePath saveDir = saveDir </> "player.dat"

-- | Save the entire world to disk
saveWorld :: FilePath -> World -> IO ()
saveWorld saveDir world = do
  let chunkDir = worldSavePath saveDir </> "chunks"
  createDirectoryIfMissing True chunkDir

  chunks <- readTVarIO (worldChunks world)
  mapM_ (\(pos, chunk) -> saveChunk chunkDir pos chunk) (HM.toList chunks)

-- | Save a single chunk
saveChunk :: FilePath -> ChunkPos -> Chunk -> IO ()
saveChunk chunkDir (V2 cx cz) chunk = do
  blocks <- freezeBlocks chunk
  let cs = ChunkSave
        { csPos    = (cx, cz)
        , csBlocks = UV.toList blocks
        }
      filename = chunkDir </> show cx ++ "_" ++ show cz ++ ".chunk"
  BL.writeFile filename (encode cs)

-- | Load chunks from disk into the world. Returns True if save exists.
loadWorld :: FilePath -> World -> IO Bool
loadWorld saveDir world = do
  let chunkDir = worldSavePath saveDir </> "chunks"
  dirExists <- doesDirectoryExist chunkDir
  if not dirExists
    then pure False
    else do
      files <- listDirectory chunkDir
      let chunkFiles = filter (\f -> takeExtension f == ".chunk") files
      loadedChunks <- mapM (loadChunkFile chunkDir) chunkFiles
      let validChunks = [c | Just c <- loadedChunks]
      unless (null validChunks) $ atomically $ do
        let chunkMap = HM.fromList [(chunkPos c, c) | c <- validChunks]
        writeTVar (worldChunks world) chunkMap
      pure (not (null validChunks))

-- | Load a single chunk from a file
loadChunkFile :: FilePath -> FilePath -> IO (Maybe Chunk)
loadChunkFile chunkDir filename = do
  let path = chunkDir </> filename
  bytes <- BL.readFile path
  let cs = decode bytes :: ChunkSave
      (cx, cz) = csPos cs
      pos = V2 cx cz
  chunk <- newChunk pos
  -- Write blocks into the mutable vector
  let blocks = csBlocks cs
  mapM_ (\(i, b) -> MV.write (chunkBlocks chunk) i b) (zip [0..] blocks)
  writeIORef (chunkDirty chunk) True
  pure (Just chunk)

-- | Save player state
savePlayer :: FilePath -> Player -> IO ()
savePlayer saveDir player = do
  createDirectoryIfMissing True saveDir
  let V3 px py pz = plPos player
      sd = SaveData
        { sdPlayerPos    = (px, py, pz)
        , sdPlayerYaw    = plYaw player
        , sdPlayerPitch  = plPitch player
        , sdPlayerFlying = plFlying player
        , sdWorldSeed    = 12345
        , sdDayTime      = 0.25
        }
  BL.writeFile (playerSavePath saveDir) (encode sd)

-- | Load player state
loadPlayer :: FilePath -> IO (Maybe Player)
loadPlayer saveDir = do
  let path = playerSavePath saveDir
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- BL.readFile path
      let sd = decode bytes :: SaveData
          (px, py, pz) = sdPlayerPos sd
          player = Player
            { plPos       = V3 px py pz
            , plVelocity  = V3 0 0 0
            , plYaw       = sdPlayerYaw sd
            , plPitch     = sdPlayerPitch sd
            , plOnGround  = False
            , plFlying    = sdPlayerFlying sd
            , plSprinting = False
            , plHealth    = 20
            , plHunger    = 20
            , plFallDist  = 0
            }
      pure (Just player)

-- | Serialized tile entity data
data TileEntitySave = TileEntitySave
  { tesEntries :: ![(Int, Int, Int, TileEntity)]  -- (x, y, z, tile entity)
  } deriving stock (Show, Eq, Generic)

instance Binary TileEntitySave

-- | Save tile entities to disk
saveTileEntities :: FilePath -> TileEntityMap -> IO ()
saveTileEntities saveDir teRef = do
  teMap <- readIORef teRef
  let entries = [(x, y, z, te) | (V3 x y z, te) <- HM.toList teMap]
      tes = TileEntitySave entries
      path = saveDir </> "tile_entities.dat"
  createDirectoryIfMissing True saveDir
  BL.writeFile path (encode tes)

-- | Load tile entities from disk
loadTileEntities :: FilePath -> TileEntityMap -> IO ()
loadTileEntities saveDir teRef = do
  let path = saveDir </> "tile_entities.dat"
  exists <- doesFileExist path
  when exists $ do
    bytes <- BL.readFile path
    let tes = decode bytes :: TileEntitySave
        teMap = HM.fromList [(V3 x y z, te) | (x, y, z, te) <- tesEntries tes]
    writeIORef teRef teMap

-- | List available save directories under "saves/"
listSaves :: IO [String]
listSaves = do
  let savesRoot = "saves"
  exists <- doesDirectoryExist savesRoot
  if not exists
    then pure []
    else do
      dirs <- listDirectory savesRoot
      validDirs <- filterM (\d -> do
        let path = savesRoot </> d
        isDir <- doesDirectoryExist path
        if not isDir then pure False
        else do
          hasPlayer <- doesFileExist (path </> "player.dat")
          hasWorld <- doesDirectoryExist (path </> "world")
          pure (hasPlayer || hasWorld)
        ) dirs
      pure (sort validDirs)

-- | Generate the next available save name (world1, world2, ...)
nextSaveName :: IO String
nextSaveName = do
  existing <- listSaves
  let nums = [n | name <- existing
                 , Just rest <- [stripPrefix "world" name]
                 , [(n, "")] <- [reads rest :: [(Int, String)]]]
      nextN = if null nums then 1 else maximum nums + 1
  pure ("world" ++ show nextN)
