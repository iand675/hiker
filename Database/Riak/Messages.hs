{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, BangPatterns #-}

module Database.Riak.Messages where
import Control.Applicative
import Control.Exception
import Data.Attoparsec.Binary
import qualified Data.Attoparsec.Lazy as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as B
import Data.Monoid hiding (All)
import Data.Data
import Data.Typeable
import Data.Word
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage hiding (Get, Put)

import Database.Riak.Protocol.DeleteRequest
import Database.Riak.Protocol.ErrorResponse
import Database.Riak.Protocol.GetBucketRequest
import Database.Riak.Protocol.GetBucketResponse
import Database.Riak.Protocol.GetClientIDResponse
import Database.Riak.Protocol.GetRequest
import Database.Riak.Protocol.GetResponse
import Database.Riak.Protocol.GetServerInfoResponse
import Database.Riak.Protocol.IndexRequest
import Database.Riak.Protocol.IndexResponse
import Database.Riak.Protocol.ListBucketsResponse
import Database.Riak.Protocol.ListKeysRequest
import qualified Database.Riak.Protocol.ListKeysResponse as LK
import Database.Riak.Protocol.MapReduceRequest
import qualified Database.Riak.Protocol.MapReduceResponse as MR
import Database.Riak.Protocol.PutRequest
import Database.Riak.Protocol.PutResponse
import Database.Riak.Protocol.SearchQueryRequest
import Database.Riak.Protocol.SearchQueryResponse
import Database.Riak.Protocol.SetBucketRequest
import Database.Riak.Protocol.SetClientIDRequest

data RiakException = ProtocolError String
                   | UnexpectedResponse Response
  deriving (Show, Typeable)

instance Exception RiakException

newtype Request = Request { fromRequest :: B.Builder }
newtype VClock = VClock { fromVClock :: L.ByteString }
newtype BucketName = BucketName { fromBucketName :: L.ByteString }
  deriving (Show, Read, Eq, Ord)
newtype Key = Key { fromKey :: L.ByteString }
  deriving (Show, Read, Eq, Ord)
newtype ClientId = ClientId { fromClientId :: L.ByteString }
  deriving (Show, Read, Eq, Ord)

data Quorum = One
            | Quorum
            | All
            | Default
            | Count !Int

quorum :: Quorum -> Word32
quorum One       = 4294967294
quorum Quorum    = 4294967293
quorum All       = 4294967292
quorum Default   = 4294967291
quorum (Count c) = fromIntegral c

fromQuorum :: Word32 -> Quorum
fromQuorum 4294967294 = One
fromQuorum 4294967293 = Quorum
fromQuorum 4294967292 = All
fromQuorum 4294967291 = Default
fromQuorum c = Count $ fromIntegral c

data Response = Error !ErrorResponse
              | Pong
              | GetClientId !GetClientIDResponse
              | SetClientId
              | GetServerInfo !GetServerInfoResponse
              | Get GetResponse
              | Put PutResponse
              | Delete
              | ListBuckets !ListBucketsResponse
              | ListKeys ![LK.ListKeysResponse]
              | GetBucket !GetBucketResponse
              | SetBucket
              | MapReduce ![MR.MapReduceResponse]
              | Index !IndexResponse
              | SearchQuery !SearchQueryResponse
  deriving (Show, Eq)

getResponse :: L.ByteString -> (Response, L.ByteString)
getResponse bs = case P.parse parseResponse bs of
  P.Fail r _ e -> throw $ ProtocolError e
  P.Done r msg -> (msg, r)

parseResponse :: P.Parser Response
parseResponse = do
  len <- anyWord32be
  code <- P.anyWord8
  let pbs = P.take $ fromIntegral $ len - 1 
  case code of
    0x00 -> (Error . protoGet) <$> pbs
    0x02 -> pure Pong
    0x04 -> (GetClientId . protoGet) <$> pbs
    0x06 -> pure SetClientId
    0x08 -> (GetServerInfo . protoGet) <$> pbs
    0x0A -> (Get . protoGet) <$> pbs
    0x0C -> (Put . protoGet) <$> pbs
    0x0E -> pure Delete
    0x10 -> (ListBuckets . protoGet) <$> pbs
    0x12 -> do
      val <- pbs
      let first = protoGet val
      if LK.done first == (Just True)
        then return $ ListKeys [first]
        else parseKeyList (first :)
    0x14 -> (GetBucket . protoGet) <$> pbs
    0x16 -> pure SetBucket
    0x18 -> do
      val <- pbs
      let first = protoGet val
      if MR.done first == (Just True)
        then return $ MapReduce [first]
        else parseMapReduceResults (first :)
    0x1A -> (Index . protoGet) <$> pbs
    0x1C -> (SearchQuery . protoGet) <$> pbs

parseKeyList f = do
  len <- anyWord32be
  code <- P.anyWord8
  pbs <- P.take $ fromIntegral len
  case code of
    0x12 -> let next = protoGet pbs in if LK.done next == (Just True)
      then return $ ListKeys $ f [next]
      else parseKeyList $ f . (next :)
    _ -> throw $ ProtocolError "Unexpected response while parsing key list."

parseMapReduceResults f = do
  len <- anyWord32be
  code <- P.anyWord8
  pbs <- P.take $ fromIntegral len
  case code of
    0x18 -> let next = protoGet pbs in if MR.done next == (Just True)
      then return $ MapReduce $ f [next]
      else parseMapReduceResults $ f . (next :)
    _ -> throw $ ProtocolError "Unexpected response while parsing MapReduce results."

protoGet :: (Wire msg, ReflectDescriptor msg) => ByteString -> msg
protoGet = either (throw . ProtocolError) fst . messageGet . L.fromStrict

ping :: Request
ping = emptyRequest 0x01

getClientId :: Request
getClientId = emptyRequest 0x03

setClientId :: SetClientIDRequest -> Request
setClientId = protoRequest 0x05

getServerInfo :: Request
getServerInfo = emptyRequest 0x07

get :: GetRequest -> Request
get = protoRequest 0x09

put :: PutRequest -> Request
put = protoRequest 0x0B

delete :: DeleteRequest -> Request
delete = protoRequest 0x0D

listBuckets :: Request
listBuckets = emptyRequest 0x0F

listKeys :: ListKeysRequest -> Request
listKeys = protoRequest 0x11

getBucket :: GetBucketRequest -> Request
getBucket = protoRequest 0x13

setBucket :: SetBucketRequest -> Request
setBucket = protoRequest 0x15

mapReduce :: MapReduceRequest -> Request
mapReduce = protoRequest 0x17

index :: IndexRequest -> Request
index = protoRequest 0x19

searchQuery :: SearchQueryRequest -> Request
searchQuery = protoRequest 0x1B

emptyRequest :: Word8 -> Request
emptyRequest = Request . (B.word32BE 1 <>) . B.word8

protoRequest :: (ReflectDescriptor msg, Wire msg) => Word8 -> msg -> Request
protoRequest code msg = Request ((B.word32BE $ fromIntegral $ 1 + messageSize msg) <> B.word8 code <> (B.lazyByteString $ messagePut msg))
