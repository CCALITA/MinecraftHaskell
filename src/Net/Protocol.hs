module Net.Protocol
  ( Packet(..)
  , PacketId
  , ClientId
  , encodePacket
  , decodePacket
  , sendPacket
  , recvPacket
  ) where

import World.Block (BlockType(..))
import Linear (V2(..), V3(..))
import Data.Binary (Binary(..), Get, Put, encode, decode, putWord8, getWord8)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32)
import System.IO (Handle, hFlush)
import GHC.Generics (Generic)

-- | Unique client identifier
type ClientId = Int

-- | Packet ID for routing
type PacketId = Word8

-- | All packet types (both client→server and server→client)
data Packet
  -- Client → Server
  = PktLogin !String                              -- username
  | PktPlayerPosition !(V3 Float) !Float !Float   -- pos, yaw, pitch
  | PktBlockBreak !(V3 Int)                       -- block position
  | PktBlockPlace !(V3 Int) !BlockType            -- position, block type
  | PktChatMessage !String                        -- chat text

  -- Server → Client
  | PktLoginAccept !ClientId                      -- assigned client ID
  | PktPlayerSpawn !ClientId !(V3 Float) !String  -- other player joined
  | PktPlayerMove !ClientId !(V3 Float) !Float !Float -- other player moved
  | PktPlayerLeave !ClientId                      -- other player left
  | PktBlockUpdate !(V3 Int) !BlockType           -- block changed
  | PktChunkData !(V2 Int) ![Word8]              -- chunk pos + block data
  | PktServerChat !String                         -- server/broadcast message
  | PktDisconnect !String                         -- kick with reason
  deriving stock (Show, Eq, Generic)

instance Binary BlockType where
  put = putWord8 . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> getWord8

instance Binary Packet where
  put = \case
    PktLogin name           -> putWord8 0x01 >> Bin.put name
    PktPlayerPosition p y h -> putWord8 0x02 >> Bin.put p >> Bin.put y >> Bin.put h
    PktBlockBreak pos       -> putWord8 0x03 >> Bin.put pos
    PktBlockPlace pos bt    -> putWord8 0x04 >> Bin.put pos >> Bin.put bt
    PktChatMessage msg      -> putWord8 0x05 >> Bin.put msg

    PktLoginAccept cid      -> putWord8 0x81 >> Bin.put cid
    PktPlayerSpawn c p n    -> putWord8 0x82 >> Bin.put c >> Bin.put p >> Bin.put n
    PktPlayerMove c p y h   -> putWord8 0x83 >> Bin.put c >> Bin.put p >> Bin.put y >> Bin.put h
    PktPlayerLeave cid      -> putWord8 0x84 >> Bin.put cid
    PktBlockUpdate pos bt   -> putWord8 0x85 >> Bin.put pos >> Bin.put bt
    PktChunkData cp blocks  -> putWord8 0x86 >> Bin.put cp >> Bin.put blocks
    PktServerChat msg       -> putWord8 0x87 >> Bin.put msg
    PktDisconnect reason    -> putWord8 0x88 >> Bin.put reason

  get = do
    tag <- getWord8
    case tag of
      0x01 -> PktLogin <$> Bin.get
      0x02 -> PktPlayerPosition <$> Bin.get <*> Bin.get <*> Bin.get
      0x03 -> PktBlockBreak <$> Bin.get
      0x04 -> PktBlockPlace <$> Bin.get <*> Bin.get
      0x05 -> PktChatMessage <$> Bin.get

      0x81 -> PktLoginAccept <$> Bin.get
      0x82 -> PktPlayerSpawn <$> Bin.get <*> Bin.get <*> Bin.get
      0x83 -> PktPlayerMove <$> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get
      0x84 -> PktPlayerLeave <$> Bin.get
      0x85 -> PktBlockUpdate <$> Bin.get <*> Bin.get
      0x86 -> PktChunkData <$> Bin.get <*> Bin.get
      0x87 -> PktServerChat <$> Bin.get
      0x88 -> PktDisconnect <$> Bin.get
      _    -> fail $ "Unknown packet ID: " ++ show tag

-- | Encode a packet to bytes (length-prefixed)
encodePacket :: Packet -> BL.ByteString
encodePacket pkt =
  let body = encode pkt
      len  = fromIntegral (BL.length body) :: Word32
  in encode len <> body

-- | Decode a packet from a handle (reads length prefix, then body)
decodePacket :: Handle -> IO (Maybe Packet)
decodePacket handle = do
  -- Read 4-byte length prefix
  lenBytes <- BS.hGet handle 4
  if BS.length lenBytes < 4
    then pure Nothing
    else do
      let len = decode (BL.fromStrict lenBytes) :: Word32
      bodyBytes <- BS.hGet handle (fromIntegral len)
      if BS.length bodyBytes < fromIntegral len
        then pure Nothing
        else pure $ Just $ decode (BL.fromStrict bodyBytes)

-- | Send a packet over a handle
sendPacket :: Handle -> Packet -> IO ()
sendPacket handle pkt = do
  BL.hPut handle (encodePacket pkt)
  hFlush handle

-- | Receive a packet from a handle
recvPacket :: Handle -> IO (Maybe Packet)
recvPacket = decodePacket
