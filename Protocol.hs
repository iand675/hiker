{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Protocol where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Proxy
import Data.Word

data Error
data Ping
data GetClientId
data SetClientId
data GetServerInfo
data Get
data Put
data Delete
data ListBuckets
data ListKeys
data GetBucket
data SetBucket
data MapReduce
data Index
data SearchQuery

newtype Riak a = Riak (IO a)
  deriving (Monad, MonadIO, Functor, Applicative)

sendRequest :: Request a => Proxy a -> RequestValue a -> Riak ()
sendRequest = undefined

receiveResponse :: Response a => Proxy a -> Riak (ResponseValue a)
receiveResponse = undefined

execute' :: (Request a, Response a) => Proxy a -> RequestValue a -> Riak (ResponseValue a)
execute' p r = sendRequest p r >> receiveResponse p

class Request a where
  type RequestValue a
  requestCode :: Proxy a -> Word8

class Response a where
  type ResponseValue a
  responseCode :: Proxy a -> Word8

instance Request Ping where
  type RequestValue Ping = P.PingRequest
  requestCode = const 0x01

instance Request GetClientId where
  type RequestValue GetClientId = P.GetClientIdRequest
  requestCode = const 0x03

instance Request SetClientId where
  type RequestValue SetClientId = P.SetClientIdRequest
  requestCode = const 0x05

instance Request GetServerInfo where
  type RequestValue GetServerInfo = P.GetServerInfoRequest
  requestCode = const 0x07

instance Request Get where
  type RequestValue Get = P.GetRequest
  requestCode = const 0x09

instance Request Put where
  type RequestValue Put = P.PutRequest
  requestCode = const 0x0B

instance Request Delete where
  type RequestValue Delete = P.DeleteRequest
  requestCode = const 0x0D

instance Request ListBuckets where
  type RequestValue ListBuckets = P.ListBucketsRequest
  requestCode = const 0x0F

instance Request ListKeys where
  type RequestValue ListKeys = P.ListKeysRequest
  requestCode = const 0x11

instance Request GetBucket where
  type RequestValue GetBucket = P.GetBucketRequest
  requestCode = const 0x13

instance Request SetBucket where
  type RequestValue SetBucket = P.SetBucketRequest
  requestCode = const 0x15

instance Request MapReduce where
  type RequestValue MapReduce = P.MapReduceRequest
  requestCode = const 0x17

instance Request Index where
  type RequestValue Index = P.IndexRequest
  requestCode = const 0x19

instance Request SearchQuery where
  type RequestValue SearchQuery = P.SearchQueryRequest
  requestCode = const 0x1B

instance Response Error where
  type ResponseValue Error = P.ErrorResponse
  responseCode = const 0x00

instance Response Ping where
  type ResponseValue Ping = P.PingResponse
  responseCode = const 0x02

instance Response GetClientId where
  type ResponseValue GetClientId = P.GetClientIdResponse
  responseCode = const 0x04

instance Response SetClientId where
  type ResponseValue SetClientId = P.SetClientIdResponse
  responseCode = const 0x06

instance Response GetServerInfo where
  type ResponseValue GetServerInfo = P.GetServerInfoResponse
  responseCode = const 0x08

instance Response Get where
  type ResponseValue Get = P.GetResponse
  responseCode = const 0x0A

instance Response Put where
  type ResponseValue Put = P.PutResponse
  responseCode = const 0x0C

instance Response Delete where
  type ResponseValue Delete = P.DeleteResponse
  responseCode = const 0x0E

instance Response ListBuckets where
  type ResponseValue ListBuckets = P.ListBucketsResponse
  responseCode = const 0x10

instance Response ListKeys where
  type ResponseValue ListKeys = P.ListKeysResponse
  responseCode = const 0x12

instance Response GetBucket where
  type ResponseValue GetBucket = P.GetBucketResponse
  responseCode = const 0x14

instance Response SetBucket where
  type ResponseValue SetBucket = P.SetBucketResponse
  responseCode = const 0x16

instance Response MapReduce where
  type ResponseValue MapReduce = P.MapReduceResponse
  responseCode = const 0x18

instance Response Index where
  type ResponseValue Index = P.IndexResponse
  responseCode = const 0x1A

instance Response SearchQuery where
  type ResponseValue SearchQuery = P.SearchQueryResponse
  responseCode = const 0x1C




