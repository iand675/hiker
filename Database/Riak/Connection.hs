{-# LANGUAGE TypeFamilies #-}
module Database.Riak.Connection where
import qualified Network.Socket as S
import Data.IORef
import Database.Riak.Messages
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Builder
import Network.Socket.ByteString.Lazy
import Text.ProtocolBuffers.Get

data Connection = Connection
  { connSocket :: S.Socket
  , connLeftovers :: IORef L.ByteString
  }

connect = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_ADDRCONFIG], S.addrSocketType = S.Stream }
  ais <- S.getAddrInfo (Just hints) (Just "localhost") (Just "8087")
  let ai = head ais
  s <- S.socket (S.addrFamily ai) (S.addrSocketType ai) (S.addrProtocol ai)
  S.setSocketOption s S.NoDelay 1
  S.connect s $ S.addrAddress ai
  left <- newIORef L.empty
  return $! Connection s left

disconnect = S.close . connSocket

withConnection :: (Connection -> IO a) -> IO a
withConnection f = do
  conn <- connect
  result <- f conn
  disconnect conn
  return result

request b c = do
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
