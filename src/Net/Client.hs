module Net.Client
  ( ClientConfig(..)
  , ClientState(..)
  , defaultClientConfig
  , connectToServer
  , disconnectFromServer
  , sendClientPacket
  , startReceiveLoop
  ) where

import Net.Protocol
import Linear (V3(..))

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException)
import Control.Monad (forever, void)
import Data.IORef
import Network.Socket
import System.IO (Handle, hClose, hSetBuffering, BufferMode(..), IOMode(..))

-- | Client configuration
data ClientConfig = ClientConfig
  { clHost :: !String
  , clPort :: !Int
  , clName :: !String
  } deriving stock (Show, Eq)

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clHost = "127.0.0.1"
  , clPort = 25565
  , clName = "Player"
  }

-- | Client connection state
data ClientState = ClientState
  { csHandle     :: !Handle
  , csClientId   :: !(TVar (Maybe ClientId))   -- assigned after login
  , csConnected  :: !(TVar Bool)
  , csRecvThread :: !(IORef (Maybe ThreadId))
  }

-- | Connect to a server, send login, return client state
connectToServer :: ClientConfig -> IO (Maybe ClientState)
connectToServer config = do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just (clHost config)) (Just (show (clPort config)))
  case addrs of
    [] -> do
      putStrLn "Failed to resolve server address"
      pure Nothing
    (addr:_) -> do
      sock <- openSocket addr
      connect sock (addrAddress addr)
        `catch` (\(e :: SomeException) -> do
          close sock
          putStrLn $ "Connection failed: " ++ show e
        )
      handle <- socketToHandle sock ReadWriteMode
      hSetBuffering handle NoBuffering

      -- Send login
      sendPacket handle (PktLogin (clName config))

      -- Wait for login accept
      mPkt <- recvPacket handle
      case mPkt of
        Just (PktLoginAccept cid) -> do
          cidVar    <- newTVarIO (Just cid)
          connected <- newTVarIO True
          recvTid   <- newIORef Nothing
          putStrLn $ "Connected to server as client " ++ show cid
          pure $ Just ClientState
            { csHandle     = handle
            , csClientId   = cidVar
            , csConnected  = connected
            , csRecvThread = recvTid
            }
        Just (PktDisconnect reason) -> do
          putStrLn $ "Server rejected connection: " ++ reason
          hClose handle
          pure Nothing
        _ -> do
          putStrLn "Unexpected response from server"
          hClose handle
          pure Nothing

-- | Disconnect from server
disconnectFromServer :: ClientState -> IO ()
disconnectFromServer cs = do
  atomically $ writeTVar (csConnected cs) False
  mTid <- readIORef (csRecvThread cs)
  case mTid of
    Just tid -> killThread tid
    Nothing  -> pure ()
  hClose (csHandle cs)
    `catch` (\(_ :: SomeException) -> pure ())

-- | Send a packet to the server
sendClientPacket :: ClientState -> Packet -> IO ()
sendClientPacket cs pkt = do
  connected <- readTVarIO (csConnected cs)
  if connected
    then sendPacket (csHandle cs) pkt
      `catch` (\(_ :: SomeException) ->
        atomically $ writeTVar (csConnected cs) False
      )
    else pure ()

-- | Start the receive loop in a background thread.
--   Calls the callback for each received packet.
startReceiveLoop :: ClientState -> (Packet -> IO ()) -> IO ()
startReceiveLoop cs onPacket = do
  tid <- forkIO $ receiveLoop cs onPacket
  writeIORef (csRecvThread cs) (Just tid)

receiveLoop :: ClientState -> (Packet -> IO ()) -> IO ()
receiveLoop cs onPacket =
  (forever $ do
    mPkt <- recvPacket (csHandle cs)
    case mPkt of
      Nothing  -> fail "Connection lost"
      Just pkt -> onPacket pkt
  ) `catch` (\(_ :: SomeException) -> do
    atomically $ writeTVar (csConnected cs) False
    putStrLn "Disconnected from server"
  )
