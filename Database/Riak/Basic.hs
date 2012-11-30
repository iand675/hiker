{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Riak.Basic (
	runRiak,
	Riak,
	Pong(..),
	Basic(..),
	run,
	ping,
	getClientId,
	setClientId,
	getServerInfo,
	get,
	put,
	delete,
	listBuckets,
	listKeys,
	getBucket,
	setBucket,
	mapReduce,
	indexQuery,
	searchQuery
) where
import           Control.Applicative
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy                               as L
import qualified Data.Foldable                                      as F
import qualified Data.Sequence                                      as S
import           Database.Riak.Connection
import qualified Database.Riak.Messages                             as M

import           Database.Riak.Protocol.BucketProps
import           Database.Riak.Protocol.Content
import           Database.Riak.Protocol.DeleteRequest
import           Database.Riak.Protocol.ErrorResponse
import           Database.Riak.Protocol.GetBucketRequest
import           Database.Riak.Protocol.GetBucketResponse
import           Database.Riak.Protocol.GetClientIDResponse
import           Database.Riak.Protocol.GetRequest
import           Database.Riak.Protocol.GetResponse
import           Database.Riak.Protocol.GetServerInfoResponse
import           Database.Riak.Protocol.IndexRequest
import           Database.Riak.Protocol.IndexRequest.IndexQueryType
import           Database.Riak.Protocol.IndexResponse
import           Database.Riak.Protocol.ListBucketsResponse
import           Database.Riak.Protocol.ListKeysRequest
import qualified Database.Riak.Protocol.ListKeysResponse            as LK
import qualified Database.Riak.Protocol.MapReduceRequest            as MR
import qualified Database.Riak.Protocol.MapReduceResponse           as MR
import           Database.Riak.Protocol.PutRequest
import           Database.Riak.Protocol.PutResponse
import           Database.Riak.Protocol.SearchQueryRequest
import           Database.Riak.Protocol.SearchQueryResponse
import           Database.Riak.Protocol.SetBucketRequest
import           Database.Riak.Protocol.SetClientIDRequest          hiding
                                                                     (clientId)

runRiak :: Connection -> Riak a -> IO a
runRiak c a = runReaderT (unwrapRiak a) c

newtype Riak a = Riak { unwrapRiak :: ReaderT Connection IO a }
	deriving (Monad, Applicative, Functor)

data Pong = Pong
	deriving (Read, Show, Eq)

class Basic a where
	basic :: a

instance Basic (L.ByteString -> Content) where
	basic b = Content b Nothing Nothing Nothing Nothing S.empty Nothing Nothing S.empty S.empty Nothing

instance Basic (L.ByteString -> L.ByteString -> GetRequest) where
	basic b k = GetRequest b k Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Basic (L.ByteString -> L.ByteString -> DeleteRequest) where
	basic b k = DeleteRequest b k Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Basic (L.ByteString -> GetBucketRequest) where
	basic = GetBucketRequest

instance Basic (L.ByteString -> L.ByteString -> IndexQueryType -> IndexRequest) where
	basic b k q = IndexRequest b k q Nothing Nothing Nothing

instance Basic (L.ByteString -> ListKeysRequest) where
	basic = ListKeysRequest

instance Basic (L.ByteString -> L.ByteString -> MR.MapReduceRequest) where
	basic = MR.MapReduceRequest

instance Basic (L.ByteString -> Content -> PutRequest) where
	basic b c = PutRequest b Nothing Nothing c Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Basic (L.ByteString -> L.ByteString -> SearchQueryRequest) where
	basic q i = SearchQueryRequest q i Nothing Nothing Nothing Nothing Nothing Nothing S.empty Nothing

instance Basic (L.ByteString -> BucketProps -> SetBucketRequest) where
	basic = SetBucketRequest

instance Basic (L.ByteString -> SetClientIDRequest) where
	basic = SetClientIDRequest

run :: M.Request -> Riak M.Response
run x = Riak $ ask >>= liftIO . runRequest x

ping :: Riak Pong
ping = do
	resp <- run M.ping
	case resp of
		M.Pong -> return Pong
		_ -> throw $ M.UnexpectedResponse resp

getClientId :: Riak M.ClientId
getClientId = do
	resp <- run M.getClientId
	case resp of
		M.GetClientId resp -> return $ M.ClientId $ clientId $ resp
		_ -> throw $ M.UnexpectedResponse resp

setClientId :: L.ByteString -> Riak ()
setClientId bs = do
	resp <- run $ M.setClientId $ SetClientIDRequest bs
	case resp of
		M.SetClientId -> return ()
		_ -> throw $ M.UnexpectedResponse resp

getServerInfo :: Riak GetServerInfoResponse
getServerInfo = do
	resp <- run M.getServerInfo
	case resp of
		M.GetServerInfo r -> return r
		_ -> throw $ M.UnexpectedResponse resp

get :: GetRequest -> Riak GetResponse
get req = do
	resp <- run $ M.get req
	case resp of
		M.Get r -> return r
		_ -> throw $ M.UnexpectedResponse resp

put :: PutRequest -> Riak PutResponse
put req = do
	resp <- run $ M.put req
	case resp of
		M.Put r -> return r
		_ -> throw $ M.UnexpectedResponse resp

delete :: DeleteRequest -> Riak ()
delete req = do
	resp <- run $ M.delete req
	case resp of
		M.Delete -> return ()
		_ -> throw $ M.UnexpectedResponse resp

listBuckets :: Riak [M.BucketName]
listBuckets = do
	resp <- run M.listBuckets
	case resp of
		M.ListBuckets resp -> return $! map M.BucketName $ F.toList $ buckets resp
		_ -> throw $ M.UnexpectedResponse resp

listKeys :: M.BucketName -> Riak [M.Key]
listKeys req = do
	resp <- run $ M.listKeys $ ListKeysRequest $ M.fromBucketName req
	case resp of
		M.ListKeys resps -> return $! concatMap (map M.Key . F.toList . LK.keys) resps
		_ -> throw $ M.UnexpectedResponse resp

getBucket :: M.BucketName -> Riak GetBucketResponse
getBucket req = do
	resp <- run $ M.getBucket $ GetBucketRequest $ M.fromBucketName req
	case resp of
		M.GetBucket resp -> return resp
		_ -> throw $ M.UnexpectedResponse resp

setBucket :: SetBucketRequest -> Riak ()
setBucket req = do
	resp <- run $ M.setBucket req
	case resp of
		M.SetBucket -> return ()
		_ -> throw $ M.UnexpectedResponse resp

mapReduce :: MR.MapReduceRequest -> Riak [MR.MapReduceResponse]
mapReduce req = do
	resp <- run $ M.mapReduce req
	case resp of
		M.MapReduce resps -> return resps
		_ -> throw $ M.UnexpectedResponse resp

indexQuery :: IndexRequest -> Riak IndexResponse
indexQuery req = do
	resp <- run $ M.index req
	case resp of
		M.Index resp -> return resp
		_ -> throw $ M.UnexpectedResponse resp

searchQuery :: SearchQueryRequest -> Riak SearchQueryResponse
searchQuery req = do
	resp <- run $ M.searchQuery req
	case resp of
		M.SearchQuery resp -> return resp
		_ -> throw $ M.UnexpectedResponse resp


