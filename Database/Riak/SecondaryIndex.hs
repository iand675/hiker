{-# LANGUAGE TypeFamilies, FlexibleContexts, EmptyDataDecls, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Database.Riak.SecondaryIndex (
  BinaryIndex,
  IntegerIndex,
  Indexable (indexName, indexValue),
  KeyType,
  ConcreteRep,
  IndexName(..),
  getIndexName,
  getIndexValue,
  findSingle,
  findRange
) where
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Database.Riak.Protocol.IndexRequest
import Database.Riak.Protocol.IndexRequest.IndexQueryType
import Database.Riak.Messages (BucketName(..))

data BinaryIndex
data IntegerIndex

class IndexType a where
	type ConcreteRep a
	repToByteString :: a -> ConcreteRep a -> ByteString
	toIndexSuffix :: a -> ByteString

instance IndexType BinaryIndex where
	type ConcreteRep BinaryIndex = ByteString
	repToByteString = const id
	toIndexSuffix = const "_bin"

instance IndexType IntegerIndex where
	type ConcreteRep IntegerIndex = Int
	repToByteString = const (C.pack . show)
	toIndexSuffix = const "_int"

newtype IndexName a = IndexName { fromIndexName :: ByteString }

class Indexable a where
	type KeyType a
	indexName :: IndexName a
	indexValue :: IndexType (KeyType a) => a -> (ConcreteRep (KeyType a))
	-- 'private' instances functions that shouldn't be exported or overridden
	indexName' :: (IndexType (KeyType a)) => a -> ByteString
	indexName' i = C.append (fromIndexName (indexName :: IndexName a)) $ toIndexSuffix (undefined :: KeyType a)
	indexValue' :: IndexType (KeyType a) => a -> ByteString
	indexValue' x = repToByteString (undefined :: KeyType a) $ indexValue x

getIndexName :: (IndexType (KeyType a), Indexable a) => a -> ByteString
getIndexName = indexName'

getIndexValue :: (IndexType (KeyType a), Indexable a) => a -> ByteString
getIndexValue = indexValue'

instance Indexable String where
	type KeyType String = BinaryIndex
	indexName = IndexName "string"
	indexValue = C.pack

instance Indexable Int where
	type KeyType Int = IntegerIndex
	indexName = IndexName "int"
	indexValue = id

findSingle :: (Indexable a, IndexType (KeyType a)) => BucketName -> a -> IndexRequest
findSingle (BucketName b) x = IndexRequest b (getIndexName x) Eq (Just $ getIndexValue x) Nothing Nothing

findRange :: (Indexable a, IndexType (KeyType a)) => BucketName -> a -> a -> IndexRequest
findRange (BucketName b) x y = IndexRequest b (getIndexName x) Range Nothing (Just $ getIndexValue x) (Just $ getIndexValue y)
