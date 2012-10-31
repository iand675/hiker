{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Database.Riak.Lens (
	SearchQueryResponse,
	Database.Riak.Lens.numFound,
	Database.Riak.Lens.maxScore,
	Database.Riak.Lens.documents,
	Database.Riak.Lens.start,
	Database.Riak.Lens.sort,
	Database.Riak.Lens.rows,
	Database.Riak.Lens.searchQuery,
	Database.Riak.Lens.presort,
	Database.Riak.Lens.operation,
	Database.Riak.Lens.fieldsLimit,
	Database.Riak.Lens.searchFilter,
	Database.Riak.Lens.df,
	Database.Riak.Lens.returnHead,
	Database.Riak.Lens.returnBody,
	Database.Riak.Lens.ifNotModified,
	Database.Riak.Lens.ifNoneMatch,
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
	basicQuorum
) where
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Word
import Data.Sequence (Seq)
import qualified Database.Riak.Messages as M

import Database.Riak.Protocol.BucketProps
import qualified Database.Riak.Protocol.Content             as Content
import qualified Database.Riak.Protocol.DeleteRequest       as DelReq
import Database.Riak.Protocol.ErrorResponse
import qualified Database.Riak.Protocol.GetBucketRequest    as GetBktReq
import qualified Database.Riak.Protocol.GetBucketResponse   as GetBktResp
import qualified Database.Riak.Protocol.GetClientIDResponse as GetClientIDResp
import qualified Database.Riak.Protocol.GetRequest          as GetReq
import qualified Database.Riak.Protocol.GetResponse         as GetResp
import Database.Riak.Protocol.GetServerInfoResponse
import qualified Database.Riak.Protocol.IndexRequest        as IxReq
import qualified Database.Riak.Protocol.IndexRequest.IndexQueryType as IxType
import qualified Database.Riak.Protocol.IndexResponse       as IxResp
import qualified Database.Riak.Protocol.Link                as Link
import Database.Riak.Protocol.ListBucketsResponse
import qualified Database.Riak.Protocol.ListKeysRequest     as LKReq
import qualified Database.Riak.Protocol.ListKeysResponse    as LKResp
import qualified Database.Riak.Protocol.MapReduceRequest    as MRReq
import qualified Database.Riak.Protocol.MapReduceResponse   as MRResp
import qualified Database.Riak.Protocol.Pair                as Pair
import qualified Database.Riak.Protocol.PutRequest          as PutReq
import qualified Database.Riak.Protocol.PutResponse         as PutResp
import qualified Database.Riak.Protocol.SearchQueryRequest  as SearchReq
import Database.Riak.Protocol.SearchQueryResponse
import qualified Database.Riak.Protocol.SetBucketRequest    as SetBktReq
import qualified Database.Riak.Protocol.SetClientIDRequest  as SetClientIDReq

let overloaded =  [ "r", "pr", "w", "pw", "dw", "vclock"
                  , "key", "content", "index", "props", "bucket"
                  , "clientId", "done", "keys", "contentType"
                  , "value" ] in
		fmap concat $ mapM (makeLensesWith (set lensField (\n -> if elem n overloaded then Nothing else Just n) lensRules))
		[ ''BucketProps
		, ''Content.Content
		, ''DelReq.DeleteRequest
		, ''ErrorResponse
		, ''GetBktReq.GetBucketRequest
		, ''GetBktResp.GetBucketResponse
		, ''GetClientIDResp.GetClientIDResponse
		, ''GetReq.GetRequest
		, ''GetResp.GetResponse
		, ''GetServerInfoResponse
		, ''IxReq.IndexRequest
		, ''IxType.IndexQueryType
		, ''IxResp.IndexResponse
		, ''Link.Link
		, ''ListBucketsResponse
		, ''LKReq.ListKeysRequest
		, ''LKResp.ListKeysResponse
		, ''MRReq.MapReduceRequest
		, ''MRResp.MapReduceResponse
		, ''Pair.Pair
		, ''PutReq.PutRequest
		, ''PutResp.PutResponse
		, ''SearchReq.SearchQueryRequest
		, ''SearchQueryResponse
		, ''SetBktReq.SetBucketRequest
		, ''SetClientIDReq.SetClientIDRequest
		]

searchQuery :: Simple Lens SearchReq.SearchQueryRequest ByteString
searchQuery = q

operation :: Simple Lens SearchReq.SearchQueryRequest (Maybe ByteString)
operation = op

fieldsLimit :: Simple Lens SearchReq.SearchQueryRequest (Seq ByteString)
fieldsLimit = fl

searchFilter :: Simple Lens SearchReq.SearchQueryRequest (Maybe ByteString)
searchFilter = Database.Riak.Lens.filter

onlyHead :: Simple Lens GetReq.GetRequest (Maybe Bool)
onlyHead = Database.Riak.Lens.head

deletedVClock :: Simple Lens GetReq.GetRequest (Maybe Bool)
deletedVClock = Database.Riak.Lens.deletedvclock

class HasR a where
	r :: Simple Lens a (Maybe M.Quorum)

instance HasR GetReq.GetRequest where
	r = lens (fmap M.fromQuorum . GetReq.r) (\gr q -> gr { GetReq.r = fmap M.quorum q })

instance HasR DelReq.DeleteRequest where
	r = lens (fmap M.fromQuorum . DelReq.r) (\dr q -> dr { DelReq.r = fmap M.quorum q })

class HasPR a where
	pr :: Simple Lens a (Maybe M.Quorum)

instance HasPR GetReq.GetRequest where
	pr = lens (fmap M.fromQuorum . GetReq.pr) (\gr q -> gr { GetReq.pr = fmap M.quorum q })

instance HasPR DelReq.DeleteRequest where
	pr = lens (fmap M.fromQuorum . DelReq.pr) (\dr q -> dr { DelReq.pr = fmap M.quorum q })

class HasKeys a where
	keys :: Simple Lens a (Seq ByteString)

instance HasKeys LKResp.ListKeysResponse where
	keys = lens LKResp.keys (\x k -> x { LKResp.keys = k })

instance HasKeys IxResp.IndexResponse where
	keys = lens IxResp.keys (\x k -> x { IxResp.keys = k })

class HasContentType a where
	type ContentTypeValue a
	contentType :: Simple Lens a (ContentTypeValue a)

instance HasContentType MRReq.MapReduceRequest where
	type ContentTypeValue MRReq.MapReduceRequest = ByteString
	contentType = lens MRReq.contentType (\x c -> x { MRReq.contentType = c }) 

instance HasContentType Content.Content where
	type ContentTypeValue Content.Content = Maybe ByteString
	contentType = lens Content.contentType (\x c -> x { Content.contentType = c })

class HasDone a where
	done :: Simple Lens a (Maybe Bool)

instance HasDone MRResp.MapReduceResponse where
	done = lens MRResp.done (\x d -> x { MRResp.done = d })

instance HasDone LKResp.ListKeysResponse where
	done = lens LKResp.done (\x d -> x { LKResp.done = d })

class HasValue a where
	type Value a
	value :: Simple Lens a (Value a)

instance HasValue Pair.Pair where
	type Value Pair.Pair = Maybe ByteString
	value = lens Pair.value (\x v -> x { Pair.value = v })

instance HasValue Content.Content where
	type Value Content.Content = ByteString
	value = lens Content.value (\x v -> x { Content.value = v})

class HasW a where
	w :: Simple Lens a (Maybe M.Quorum)

instance HasW DelReq.DeleteRequest where
	w = lens (fmap M.fromQuorum . DelReq.w) (\dr q -> dr { DelReq.w = fmap M.quorum q })

instance HasW PutReq.PutRequest where
	w = lens (fmap M.fromQuorum . PutReq.w) (\pr q -> pr { PutReq.w = fmap M.quorum q })

class HasPW a where
	pw :: Simple Lens a (Maybe M.Quorum)

instance HasPW DelReq.DeleteRequest where
	pw = lens (fmap M.fromQuorum . DelReq.pw) (\dr q -> dr { DelReq.pw = fmap M.quorum q })

instance HasPW PutReq.PutRequest where
	pw = lens (fmap M.fromQuorum . PutReq.pw) (\pr q -> pr { PutReq.pw = fmap M.quorum q })

class HasDW a where
	dw :: Simple Lens a (Maybe M.Quorum)

instance HasDW DelReq.DeleteRequest where
	dw = lens (fmap M.fromQuorum . DelReq.pw) (\dr q -> dr { DelReq.dw = fmap M.quorum q })

instance HasDW PutReq.PutRequest where
	dw = lens (fmap M.fromQuorum . PutReq.pw) (\pr q -> pr { PutReq.dw = fmap M.quorum q })

class HasVClock a where
	vclock :: Simple Lens a (Maybe ByteString)

instance HasVClock DelReq.DeleteRequest where
	vclock = lens DelReq.vclock (\d v -> d { DelReq.vclock = v })

instance HasVClock GetResp.GetResponse where
	vclock = lens GetResp.vclock (\d v -> d { GetResp.vclock = v })

instance HasVClock PutReq.PutRequest where
	vclock = lens PutReq.vclock (\d v -> d { PutReq.vclock = v })

instance HasVClock PutResp.PutResponse where
	vclock = lens PutResp.vclock (\d v -> d { PutResp.vclock = v })

class HasKey a where
	type KeyValue a
	key :: Simple Lens a (KeyValue a)

instance HasKey Pair.Pair where
	type KeyValue Pair.Pair = ByteString
	key = lens Pair.key (\x k -> x { Pair.key = k })

instance HasKey Link.Link where
	type KeyValue Link.Link = Maybe ByteString
	key = lens Link.key (\x k -> x { Link.key = k })

instance HasKey IxReq.IndexRequest where
	type KeyValue IxReq.IndexRequest = Maybe ByteString
	key = lens IxReq.key (\x k -> x { IxReq.key = k })

instance HasKey GetReq.GetRequest where
	type KeyValue GetReq.GetRequest = ByteString
	key = lens GetReq.key (\x k -> x { GetReq.key = k })

instance HasKey DelReq.DeleteRequest where
	type KeyValue DelReq.DeleteRequest = ByteString
	key = lens DelReq.key (\x k -> x { DelReq.key = k })

instance HasKey PutReq.PutRequest where
	type KeyValue PutReq.PutRequest = Maybe ByteString
	key = lens PutReq.key (\x k -> x { PutReq.key = k })

instance HasKey PutResp.PutResponse where
	type KeyValue PutResp.PutResponse = Maybe ByteString
	key = lens PutResp.key (\x k -> x { PutResp.key = k })

class HasContent a where
	type ContentValue a
	content :: Simple Lens a (ContentValue a)

instance HasContent GetResp.GetResponse where
	type ContentValue GetResp.GetResponse = Seq Content.Content
	content = lens GetResp.content (\x c -> x { GetResp.content = c })

instance HasContent PutReq.PutRequest where
	type ContentValue PutReq.PutRequest = Content.Content
	content = lens PutReq.content (\x c -> x { PutReq.content = c })

instance HasContent PutResp.PutResponse where
	type ContentValue PutResp.PutResponse = Seq Content.Content
	content = lens PutResp.content (\x c -> x { PutResp.content = c })

class HasIndex a where
	index :: Simple Lens a ByteString

instance HasIndex SearchReq.SearchQueryRequest where
	index = lens SearchReq.index (\x i -> x { SearchReq.index = i })

instance HasIndex IxReq.IndexRequest where
	index = lens IxReq.index (\x i -> x { IxReq.index = i }) 

class HasProps a where
	props :: Simple Lens a BucketProps

instance HasProps GetBktResp.GetBucketResponse where
	props = lens GetBktResp.props (\g p -> g { GetBktResp.props = p })

instance HasProps SetBktReq.SetBucketRequest where
	props = lens SetBktReq.props (\s p -> s { SetBktReq.props = p })

class HasBucket a where
	type BucketValue a
	bucket :: Simple Lens a (BucketValue a)

instance HasBucket Link.Link where
	type BucketValue Link.Link = Maybe ByteString
	bucket = lens Link.bucket (\l b -> l { Link.bucket = b })

instance HasBucket LKReq.ListKeysRequest where
	type BucketValue LKReq.ListKeysRequest = ByteString
	bucket = lens LKReq.bucket (\l b -> l { LKReq.bucket = b })

instance HasBucket IxReq.IndexRequest where
	type BucketValue IxReq.IndexRequest = ByteString
	bucket = lens IxReq.bucket (\l b -> l { IxReq.bucket = b })

instance HasBucket GetReq.GetRequest where
	type BucketValue GetReq.GetRequest = ByteString
	bucket = lens GetReq.bucket (\g b -> g { GetReq.bucket = b })

instance HasBucket GetBktReq.GetBucketRequest where
	type BucketValue GetBktReq.GetBucketRequest = ByteString
	bucket = lens GetBktReq.bucket (\g b -> g { GetBktReq.bucket = b })

instance HasBucket DelReq.DeleteRequest where
	type BucketValue DelReq.DeleteRequest = ByteString
	bucket = lens DelReq.bucket (\g b -> g { DelReq.bucket = b })

instance HasBucket PutReq.PutRequest where
	type BucketValue PutReq.PutRequest = ByteString
	bucket = lens PutReq.bucket (\g b -> g { PutReq.bucket = b })

class HasClientId a where
	clientId :: Simple Lens a ByteString

instance HasClientId GetClientIDResp.GetClientIDResponse where
	clientId = lens GetClientIDResp.clientId (\gr cid -> gr { GetClientIDResp.clientId = cid })

instance HasClientId SetClientIDReq.SetClientIDRequest where
	clientId = lens SetClientIDReq.clientId (\gr cid -> gr { SetClientIDReq.clientId = cid })
