{-# LANGUAGE TypeFamilies #-}
module Database.Riak.Connection (
  ConnectionSettings(..),
  riakPool,
  RiakConnectionPool,
  RiakConnection(..),
  Connection,
  connect,
  disconnect,
  runRequest,
  decodeRecv
) where
import qualified Network.Socket as S
import Data.IORef
import Data.Maybe
import Data.Pool
import Data.Time.Clock
import Database.Riak.Messages
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
import Network.Socket.ByteString.Lazy
import Text.ProtocolBuffers.Get

data ConnectionSettings = ConnectionSettings
  { host               :: Maybe String
  , port               :: Maybe Int
  , connectionTimeout  :: Maybe NominalDiffTime
  , connectionsPerPool :: Maybe Int
  , pools              :: Maybe Int
  }

riakPool :: ConnectionSettings -> IO RiakConnectionPool
riakPool s = fmap RiakConnectionPool $ createPool (connect (host s) (port s)) disconnect 
  (fromMaybe 3 $ pools s)
  (fromMaybe 30 $ connectionTimeout s)
  (fromMaybe 10 $ connectionsPerPool s)

newtype RiakConnectionPool = RiakConnectionPool
  { fromRiakConnectionPool :: Pool Connection
  }

class RiakConnection c where
  withConnection :: c -> (Connection -> IO a) -> IO a

data Connection = Connection
  { connSocket :: S.Socket
  , connLeftovers :: IORef L.ByteString
  }

connect :: Maybe S.HostName -> Maybe Int -> IO Connection
connect h p = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_ADDRCONFIG], S.addrSocketType = S.Stream }
  ais <- S.getAddrInfo (Just hints) h (Just $ maybe "8087" show p)
  let ai = head ais
  s <- S.socket (S.addrFamily ai) (S.addrSocketType ai) (S.addrProtocol ai)
  S.setSocketOption s S.NoDelay 1
  S.connect s $ S.addrAddress ai
  left <- newIORef L.empty
  return $! Connection s left

disconnect = S.close . connSocket

instance RiakConnection Connection where
  withConnection c f = f c

instance RiakConnection RiakConnectionPool where
  withConnection p = withResource (fromRiakConnectionPool p)

runRequest b c = do
  sendRequest c b
  decodeRecv c

decodeRecv c = do
  bs <- recv (connSocket c) window
  let (value, remainder) = getResponse bs
  writeIORef (connLeftovers c) remainder
  return $! value
  where 
    window = 16384

sendRequest c r = send (connSocket c) $ toLazyByteString $ fromRequest r
