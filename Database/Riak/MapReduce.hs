{-# LANGUAGE GADTs, ScopedTypeVariables, Rank2Types #-}
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Set (Set, toList)
import Data.Text (Text)
import Database.Riak.Messages (Key(..), BucketName(..))

infixr 9 $>
--	toJSON (Finalize t)     = toJSON t
--	toJSON (a :> (Finalize b)) = toJSON $ [to]
--	toJSON (a :> b) = 

newtype IndexName = IndexName { fromIndexName :: ByteString }

data ErlangQuery = ErlangQuery { erlangModule :: ByteString
                               , function :: ByteString }

data JavascriptQuery = SourceQuery ByteString
                     | Stored BucketName Key
                     | BuiltIn ByteString

data Inputs = Index { ixBucket :: Maybe BucketName
                    , index    :: Maybe IndexName
                    , ixStart  :: Maybe ByteString
                    , ixEnd    :: Maybe ByteString
                    , ixKey    :: Maybe Key
                    }
            | KeyFilter { keyFilterBucket :: BucketName
                        , keyFilters :: Filter String ()
                        }
            | Inputs { inputs :: [(BucketName, Key, Maybe ByteString)] }

instance ToJSON Inputs where
	toJSON (Index bucket index start end key) = object [ "bucket" .= bucket, "index" .= index, "key" .= key, "start" .= start, "end" .= end ]
	toJSON (KeyFilter bucket filter) = object [ "bucket" .= bucket, "key_filters" .= fromFilter filter ]
	toJSON (Inputs inputs) = toJSON $ map detuple inputs
		where
			detuple (b, k, Nothing) = [toJSON b, toJSON k]
			detuple (b, k, Just d)  = [toJSON b, toJSON k, toJSON d]

instance ToJSON BucketName where
	toJSON = toJSON . fromBucketName

instance ToJSON Key where
	toJSON = toJSON . fromKey

instance ToJSON IndexName where
	toJSON = toJSON . fromIndexName

-- data MapReduce = MapReduce { mrInputs :: Inputs
 --                          , mrQuery :: [Phase] }

data Phase a = Map { mapSource :: a
	               , mapArg    :: ByteString
	               , mapKeep   :: Maybe Bool
	               }
	         | Reduce { reduceSource :: a 
		              , reduceArg    :: ByteString
		              , reduceKeep   :: Maybe Bool
		              }
             | Link { bucket   :: Maybe ByteString
                    , tag      :: Maybe ByteString 
                    , linkKeep :: Maybe Bool
                    }

instance ToJSON (Phase a) where
	toJSON (Map src a k)    = object $ [ "map" .= object []]
	toJSON (Reduce src a k) = object $ [ "reduce" .= object []]
	toJSON (Link bkt tag k) = object $ [ "link" .= object [ "bucket" .= bkt, "tag" .= tag, "keep" .= k ]]

{-
data ReducePhase a = { language :: a
                     , 
                     }
-}

data Transform from to where
	IntToString :: Transform Int String
	StringToInt :: Transform String Int
	FloatToString :: Transform Double String
	StringToFloat :: Transform String Double
	ToUpper :: Transform String String
	ToLower :: Transform String String
	Tokenize :: String -> Int -> Transform String String
	UrlDecode :: Transform String String

newtype Filter a b = Filter { fromFilter :: [Value] }

finalizeFilter :: Filter String a -> Filter String ()
finalizeFilter = Filter . fromFilter

($>) :: (ToJSON a, ToJSON b, ToJSON c) => Transform a b -> Filter b c -> Filter a c
t $> (Filter ts) = Filter (toJSON t : ts)

predicate :: ToJSON b => Predicate a b -> Filter a b
predicate p = (Filter $ (:[]) $ toJSON p) :: Filter a b

data Predicate a b where
	GreaterThan :: Num b => b -> Predicate b b
	LessThan :: Num b => b -> Predicate b b
	GreaterThanEq :: Num b => b -> Predicate b b
	LessThanEq :: Num b => b -> Predicate b b
	Between :: Num b => b -> b -> Bool -> Predicate b b
	Matches :: String -> Predicate String String
	NotEqual :: b -> Predicate b b
	Equal :: b -> Predicate b b
	SetMember :: Set b -> Predicate b b
	SimilarTo :: String -> Int -> Predicate String String
	StartsWith :: String -> Predicate String String
	EndsWith :: String -> Predicate String String
	And :: (ToJSON b, ToJSON c) => Filter a b -> Filter a c -> Predicate a d
	Or :: (ToJSON b, ToJSON c) => Filter a b -> Filter a c -> Predicate a d
	Not :: ToJSON b => Predicate a b -> Predicate a b

instance ToJSON b => ToJSON (Predicate a b) where
	toJSON (GreaterThan n)   = toJSON [toJSON ("greater_than" :: Text), toJSON n]
	toJSON (LessThan n)      = toJSON [toJSON ("less_than" :: Text), toJSON n]
	toJSON (GreaterThanEq n) = toJSON [toJSON ("greater_than_eq" :: Text), toJSON n]
	toJSON (LessThanEq n)    = toJSON [toJSON ("less_than_eq" :: Text), toJSON n]
	toJSON (Between x y i)   = toJSON [toJSON ("between" :: Text), toJSON x, toJSON y, toJSON i]
	toJSON (Matches str)     = toJSON [toJSON ("matches" :: Text), toJSON str]
	toJSON (NotEqual x)      = toJSON [toJSON ("neq" :: Text), toJSON x]
	toJSON (Equal x)         = toJSON [toJSON ("eq" :: Text), toJSON x]
	toJSON (SetMember s)     = toJSON [toJSON ("set_member" :: Text), toJSON $ toList s]
	toJSON (SimilarTo s d)   = toJSON [toJSON ("similar_to" :: Text), toJSON s, toJSON d]
	toJSON (StartsWith str)  = toJSON [toJSON ("starts_with" :: Text), toJSON str]
	toJSON (EndsWith str)    = toJSON [toJSON ("ends_with" :: Text), toJSON str]
	toJSON (And l r)         = toJSON [toJSON ("and" :: Text), toJSON l, toJSON r]
	toJSON (Or l r)          = toJSON [toJSON ("or" :: Text), toJSON l, toJSON r]
	toJSON (Not t)           = toJSON [toJSON ("not" :: Text), toJSON t]

instance (ToJSON a, ToJSON b) => ToJSON (Transform a b) where
	toJSON IntToString      = toJSON ["int_to_string" :: Text]
	toJSON StringToInt      = toJSON ["string_to_int" :: Text]
	toJSON FloatToString    = toJSON ["float_to_string" :: Text]
	toJSON StringToFloat    = toJSON ["string_to_float" :: Text]
	toJSON ToUpper          = toJSON ["to_upper" :: Text]
	toJSON ToLower          = toJSON ["to_lower" :: Text]
	toJSON (Tokenize str i) = toJSON [toJSON str, toJSON i]
	toJSON UrlDecode        = toJSON ["url_decode" :: Text]

instance (ToJSON b) => ToJSON (Filter a b) where
	toJSON = toJSON . fromFilter

-- instance ToJSON MapReduce where
--	toJSON (MapReduce inputs query) = object [ "inputs" .= inputs, "query" .= query ]


