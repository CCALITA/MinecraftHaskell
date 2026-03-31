module Net.Server
  ( ServerConfig(..)
  , ServerState(..)
  , ConnectedClient(..)
  , defaultServerConfig
  , startServer
  , stopServer
  , broadcastPacket
  , sendToClient
  ) where

import Net.Protocol
import World.Block (BlockType(..))
import Linear (V3(..), V2(..))

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException, finally)
import Control.Monad (forever, forM_, void, when)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Network.Socket
import Network.Socket.ByteString (recv)
import System.IO (Handle, hClose, hSetBuffering, BufferMode(..), IOMode(..))

-- | Server configuration
data ServerConfig = ServerConfig
  { scHost       :: !String
  , scPort       :: !Int
  , scMaxPlayers :: !Int
  , scMotd       :: !String
  } deriving stock (Show, Eq)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { scHost       = "0.0.0.0"
  , scPort       = 25565
  , scMaxPlayers = 20
  , scMotd       = "Minecraft Haskell Server"
  }

-- | A connected client
data ConnectedClient = ConnectedClient
  { ccId       :: !ClientId
  , ccName     :: !String
  , ccHandle   :: !Handle
  , ccPosition :: !(TVar (V3 Float))
  , ccYaw      :: !(TVar Float)
  , ccPitch    :: !(TVar Float)
  , ccThread   :: !ThreadId
  }

-- | Server state
data ServerState = ServerState
  { ssConfig    :: !ServerConfig
  , ssClients   :: !(TVar (IM.IntMap ConnectedClient))
  , ssNextId    :: !(IORef ClientId)
  , ssSocket    :: !(TVar (Maybe Socket))
  , ssRunning   :: !(TVar Bool)
  }

-- | Start the server, listening for connections
startServer :: ServerConfig -> (ServerState -> Packet -> ClientId -> IO ()) -> IO ServerState
startServer config onPacket = do
  clients <- newTVarIO IM.empty
  nextId  <- newIORef 1
  sockVar <- newTVarIO Nothing
  running <- newTVarIO True

  let ss = ServerState
        { ssConfig  = config
        , ssClients = clients
        , ssNextId  = nextId
        , ssSocket  = sockVar
        , ssRunning = running
        }

  -- Start accept loop in background
  void $ forkIO $ do
    let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
    addr:_ <- getAddrInfo (Just hints) (Just (scHost config)) (Just (show (scPort config)))
    bracket (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      atomically $ writeTVar sockVar (Just sock)
      putStrLn $ "Server listening on " ++ scHost config ++ ":" ++ show (scPort config)
      acceptLoop ss sock onPacket

  pure ss

-- | Accept incoming connections
acceptLoop :: ServerState -> Socket -> (ServerState -> Packet -> ClientId -> IO ()) -> IO ()
acceptLoop ss sock onPacket = do
  isRunning <- readTVarIO (ssRunning ss)
  when isRunning $ do
    (conn, _addr) <- accept sock
    handle <- socketToHandle conn ReadWriteMode
    hSetBuffering handle NoBuffering

    -- Read login packet
    mPkt <- recvPacket handle
    case mPkt of
      Just (PktLogin name) -> do
        cid <- readIORef (ssNextId ss)
        writeIORef (ssNextId ss) (cid + 1)

        pos <- newTVarIO (V3 0 80 0)
        yaw <- newTVarIO 0
        pitch <- newTVarIO 0

        -- Spawn client handler thread
        tid <- forkIO $ clientHandler ss cid handle onPacket

        let client = ConnectedClient
              { ccId       = cid
              , ccName     = name
              , ccHandle   = handle
              , ccPosition = pos
              , ccYaw      = yaw
              , ccPitch    = pitch
              , ccThread   = tid
              }

        atomically $ modifyTVar (ssClients ss) (IM.insert cid client)

        -- Send login accept
        sendPacket handle (PktLoginAccept cid)

        -- Notify other players
        broadcastPacket ss (PktPlayerSpawn cid (V3 0 80 0) name)

        putStrLn $ "Player " ++ name ++ " connected (ID " ++ show cid ++ ")"

      _ -> hClose handle  -- invalid first packet

    acceptLoop ss sock onPacket

-- | Handle packets from a single client
clientHandler :: ServerState -> ClientId -> Handle -> (ServerState -> Packet -> ClientId -> IO ()) -> IO ()
clientHandler ss cid handle onPacket =
  (forever $ do
    mPkt <- recvPacket handle
    case mPkt of
      Nothing -> fail "Connection closed"
      Just pkt -> onPacket ss pkt cid
  ) `catch` (\(_ :: SomeException) -> do
    -- Client disconnected
    atomically $ modifyTVar (ssClients ss) (IM.delete cid)
    broadcastPacket ss (PktPlayerLeave cid)
    hClose handle
    putStrLn $ "Client " ++ show cid ++ " disconnected"
  )

-- | Stop the server
stopServer :: ServerState -> IO ()
stopServer ss = do
  atomically $ writeTVar (ssRunning ss) False
  -- Close all client connections
  clients <- readTVarIO (ssClients ss)
  forM_ (IM.elems clients) $ \client -> do
    sendPacket (ccHandle client) (PktDisconnect "Server shutting down")
      `catch` (\(_ :: SomeException) -> pure ())
    killThread (ccThread client)
    hClose (ccHandle client)
  -- Close server socket
  mSock <- readTVarIO (ssSocket ss)
  case mSock of
    Just sock -> close sock
    Nothing   -> pure ()

-- | Broadcast a packet to all connected clients
broadcastPacket :: ServerState -> Packet -> IO ()
broadcastPacket ss pkt = do
  clients <- readTVarIO (ssClients ss)
  forM_ (IM.elems clients) $ \client ->
    sendPacket (ccHandle client) pkt
      `catch` (\(_ :: SomeException) -> pure ())

-- | Send a packet to a specific client
sendToClient :: ServerState -> ClientId -> Packet -> IO ()
sendToClient ss cid pkt = do
  clients <- readTVarIO (ssClients ss)
  case IM.lookup cid clients of
    Just client -> sendPacket (ccHandle client) pkt
      `catch` (\(_ :: SomeException) -> pure ())
    Nothing -> pure ()
