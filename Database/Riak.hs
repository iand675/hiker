module Database.Riak (
	M.RiakException(..),
	M.VClock(..),
	M.BucketName(..),
	M.Key(..),
	M.ClientId(..),
	M.Quorum(..),
	B.Riak,
	B.Basic(..),
	B.Pong(..),
	B.ping,
	B.getClientId,
	B.setClientId,
	B.getServerInfo,
	B.get,
	B.put,
	B.delete,
	B.listBuckets,
	B.listKeys,
	B.getBucket,
	B.setBucket,
	B.mapReduce,
	B.indexQuery,
	B.searchQuery,
	B.runRiak,
	numFound,
	maxScore,
	documents,
	start,
	sort,
	rows,
	query,
	presort,
	operation,
	fieldsLimit,
	searchFilter,
	df,
	returnHead,
	returnBody,
	ifNotModified,
	ifNoneMatch,
	response,
	phase,
	request,
	buckets,
	tag,
	rangeMin,
	rangeMax,
	qtype,
	serverVersion,
	node,
	unchanged,
	notFoundOk,
	ifModified,
	onlyHead,
	deletedVClock,
	basicQuorum,
	errorMessage,
	errorCode,
	rw,
	vTag,
	userMeta,
	links,
	lastModUsecs,
	lastMod,
	indexes,
	deleted,
	contentEncoding,
	charset,
	nVal,
	allowMult,
	r,
	pr,
	keys,
	contentType,
	done,
	value,
	w,
	pw,
	dw,
	vClock,
	key,
	content,
	indexValue,
	bucket,
	clientId,
	props,
	L.HasR,
	L.HasPR,
	L.HasKeys,
	L.HasContentType,
	L.HasDone,
	L.HasValue,
	L.HasW,
	L.HasPW,
	L.HasDW,
	L.HasVClock,
	L.HasKey,
	L.HasContent,
	L.HasIndex,
	L.HasBucket,
	L.HasClientId,
	L.HasProps,
	BucketProps,
	Content,
	DeleteRequest,
	ErrorResponse,
	GetBucketRequest,
	GetBucketResponse,
	GetClientIDResponse,
	GetRequest,
	GetResponse,
	GetServerInfoResponse,
	IndexRequest,
	IndexQueryType,
	IndexResponse,
	Link,
	ListBucketsResponse,
	ListKeysRequest,
	ListKeysResponse,
	MapReduceRequest,
	MapReduceResponse,
	Pair,
	PutRequest,
	PutResponse,
	SearchDocument,
	SearchQueryRequest,
	SearchQueryResponse,
	SetBucketRequest,
	SetClientIDRequest,
	module Database.Riak.Connection
) where
import qualified Control.Lens as Lens

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import Data.Word

import qualified Database.Riak.Basic as B
import Database.Riak.Connection
import qualified Database.Riak.Messages as M
import qualified Database.Riak.Lens as L
import Database.Riak.Protocol.BucketProps           (BucketProps)
import Database.Riak.Protocol.Content               (Content)
import Database.Riak.Protocol.DeleteRequest       	(DeleteRequest)
import Database.Riak.Protocol.ErrorResponse         (ErrorResponse)
import Database.Riak.Protocol.GetBucketRequest    	(GetBucketRequest)
import Database.Riak.Protocol.GetBucketResponse   	(GetBucketResponse)
import Database.Riak.Protocol.GetClientIDResponse   (GetClientIDResponse)
import Database.Riak.Protocol.GetRequest          	(GetRequest)
import Database.Riak.Protocol.GetResponse         	(GetResponse)
import Database.Riak.Protocol.GetServerInfoResponse (GetServerInfoResponse)
import Database.Riak.Protocol.IndexRequest        	(IndexRequest)
import Database.Riak.Protocol.IndexRequest.IndexQueryType (IndexQueryType)
import Database.Riak.Protocol.IndexResponse       	(IndexResponse)
import Database.Riak.Protocol.Link                	(Link)
import Database.Riak.Protocol.ListBucketsResponse	(ListBucketsResponse)
import Database.Riak.Protocol.ListKeysRequest     	(ListKeysRequest)
import Database.Riak.Protocol.ListKeysResponse    	(ListKeysResponse)
import Database.Riak.Protocol.MapReduceRequest    	(MapReduceRequest)
import Database.Riak.Protocol.MapReduceResponse   	(MapReduceResponse)
import Database.Riak.Protocol.Pair                	(Pair)
import Database.Riak.Protocol.PutRequest          	(PutRequest)
import Database.Riak.Protocol.PutResponse         	(PutResponse)
import Database.Riak.Protocol.SearchDocument        (SearchDocument)
import Database.Riak.Protocol.SearchQueryRequest  	(SearchQueryRequest)
import Database.Riak.Protocol.SearchQueryResponse   (SearchQueryResponse)
import Database.Riak.Protocol.SetBucketRequest    	(SetBucketRequest)
import Database.Riak.Protocol.SetClientIDRequest  	(SetClientIDRequest)

numFound :: Lens.Simple Lens.Lens SearchQueryResponse (Maybe Word32)
numFound = L.numFound

maxScore :: Lens.Simple Lens.Lens SearchQueryResponse (Maybe Float)
maxScore = L.maxScore

documents :: Lens.Simple Lens.Lens SearchQueryResponse (Seq SearchDocument)
documents = L.documents

start :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe Word32)
start = L.start

sort :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe ByteString)
sort = L.sort

rows :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe Word32)
rows = L.rows

query :: Lens.Simple Lens.Lens SearchQueryRequest ByteString
query = L.searchQuery

presort :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe ByteString)
presort = L.presort

operation :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe ByteString)
operation = L.operation

fieldsLimit :: Lens.Simple Lens.Lens SearchQueryRequest (Seq ByteString)
fieldsLimit = L.fieldsLimit

searchFilter :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe ByteString)
searchFilter = L.searchFilter

df :: Lens.Simple Lens.Lens SearchQueryRequest (Maybe ByteString)
df = L.df

returnHead :: Lens.Simple Lens.Lens PutRequest (Maybe Bool)
returnHead = L.returnHead

returnBody :: Lens.Simple Lens.Lens PutRequest (Maybe Bool)
returnBody = L.returnBody

ifNotModified :: Lens.Simple Lens.Lens PutRequest (Maybe Bool)
ifNotModified = L.ifNotModified

ifNoneMatch :: Lens.Simple Lens.Lens PutRequest (Maybe Bool)
ifNoneMatch = L.ifNoneMatch

response :: Lens.Simple Lens.Lens MapReduceResponse (Maybe ByteString)
response = L.response

phase :: Lens.Simple Lens.Lens MapReduceResponse (Maybe Word32)
phase = L.phase

request :: Lens.Simple Lens.Lens MapReduceRequest ByteString
request = L.request

buckets :: Lens.Simple Lens.Lens ListBucketsResponse (Seq ByteString)
buckets = L.buckets

tag :: Lens.Simple Lens.Lens Link (Maybe ByteString)
tag = L.tag

rangeMin :: Lens.Simple Lens.Lens IndexRequest (Maybe ByteString)
rangeMin = L.rangeMin

rangeMax :: Lens.Simple Lens.Lens IndexRequest (Maybe ByteString)
rangeMax = L.rangeMax

qtype :: Lens.Simple Lens.Lens IndexRequest IndexQueryType
qtype = L.qtype

serverVersion :: Lens.Simple Lens.Lens GetServerInfoResponse (Maybe ByteString)
serverVersion = L.serverVersion

node :: Lens.Simple Lens.Lens GetServerInfoResponse (Maybe ByteString)
node = L.node

unchanged :: Lens.Simple Lens.Lens GetResponse (Maybe Bool)
unchanged = L.unchanged

notFoundOk :: Lens.Simple Lens.Lens GetRequest (Maybe Bool)
notFoundOk = L.notfoundOk

ifModified :: Lens.Simple Lens.Lens GetRequest (Maybe ByteString)
ifModified = L.ifModified

onlyHead :: Lens.Simple Lens.Lens GetRequest (Maybe Bool)
onlyHead = L.onlyHead

deletedVClock :: Lens.Simple Lens.Lens GetRequest (Maybe Bool)
deletedVClock = L.deletedVClock

basicQuorum :: Lens.Simple Lens.Lens GetRequest (Maybe Bool)
basicQuorum = L.basicQuorum

errorMessage :: Lens.Simple Lens.Lens ErrorResponse ByteString
errorMessage = L.errmsg

errorCode :: Lens.Simple Lens.Lens ErrorResponse Word32
errorCode = L.errcode

rw :: Lens.Simple Lens.Lens DeleteRequest (Maybe Word32)
rw = L.rw

vTag :: Lens.Simple Lens.Lens Content (Maybe ByteString)
vTag = L.vtag

userMeta :: Lens.Simple Lens.Lens Content (Seq Pair)
userMeta = L.usermeta

links :: Lens.Simple Lens.Lens Content (Seq Link)
links = L.links

lastModUsecs :: Lens.Simple Lens.Lens Content (Maybe Word32)
lastModUsecs = L.lastModUsecs

lastMod :: Lens.Simple Lens.Lens Content (Maybe Word32)
lastMod = L.lastMod

indexes :: Lens.Simple Lens.Lens Content (Seq Pair)
indexes = L.indexes

deleted :: Lens.Simple Lens.Lens Content (Maybe Bool)
deleted = L.deleted

contentEncoding :: Lens.Simple Lens.Lens Content (Maybe ByteString)
contentEncoding = L.contentEncoding

charset :: Lens.Simple Lens.Lens Content (Maybe ByteString)
charset = L.charset

nVal :: Lens.Simple Lens.Lens BucketProps (Maybe Word32)
nVal = L.nVal

allowMult :: Lens.Simple Lens.Lens BucketProps (Maybe Bool)
allowMult = L.allowMult

r :: L.HasR a => Lens.Simple Lens.Lens a (Maybe M.Quorum)
r = L.r

pr :: L.HasPR a => Lens.Simple Lens.Lens a (Maybe M.Quorum)
pr = L.pr

keys :: L.HasKeys a => Lens.Simple Lens.Lens a (Seq ByteString)
keys = L.keys

contentType :: L.HasContentType a => Lens.Simple Lens.Lens a (L.ContentTypeValue a)
contentType = L.contentType

done :: L.HasDone a => Lens.Simple Lens.Lens a (Maybe Bool)
done = L.done

value :: L.HasValue a => Lens.Simple Lens.Lens a (L.Value a)
value = L.value

w :: L.HasW a => Lens.Simple Lens.Lens a (Maybe M.Quorum)
w = L.w

pw :: L.HasPW a => Lens.Simple Lens.Lens a (Maybe M.Quorum)
pw = L.pw

dw :: L.HasDW a => Lens.Simple Lens.Lens a (Maybe M.Quorum)
dw = L.dw

vClock :: L.HasVClock a => Lens.Simple Lens.Lens a (Maybe ByteString)
vClock = L.vclock

key :: L.HasKey a => Lens.Simple Lens.Lens a (L.KeyValue a)
key = L.key

content :: L.HasContent a => Lens.Simple Lens.Lens a (L.ContentValue a)
content = L.content

indexValue :: L.HasIndex a => Lens.Simple Lens.Lens a ByteString
indexValue = L.index

bucket :: L.HasBucket a => Lens.Simple Lens.Lens a (L.BucketValue a)
bucket = L.bucket

clientId :: L.HasClientId a => Lens.Simple Lens.Lens a ByteString
clientId = L.clientId

props :: L.HasProps a => Lens.Simple Lens.Lens a BucketProps
props = L.props
