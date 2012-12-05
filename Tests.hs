{-# LANGUAGE OverloadedStrings #-}
module Tests where
import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString.Lazy.Char8
import qualified Data.Sequence as S
import Database.Riak
import Database.Riak.Protocol.BucketProps
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.HUnit

withRiak action = bracket
  (connect (Just "127.0.0.1") Nothing)
  disconnect
  (\c -> runRiak c action)

main = hspec $ do
  describe "Protocol Buffers API" $ do
    pingSpecs
    clientIdSpecs
    getServerInfoSpecs
    basicCommands
    listBucketsSpecs
    listKeysSpecs
    getBucketSpecs
    setBucketSpecs
    mapReduceSpecs
    indexQuerySpecs
    searchQuerySpecs
  describe "HTTP API" $ do
    return ()


pingSpecs = describe "Database.Riak.ping" $ do
  it "pongs" $ withRiak ping `shouldReturn` Pong

clientIdSpecs = describe "Database.Riak.(get|set)ClientId" $ do
  it "returns the set client id when asked" $
    withRiak (setClientId "Dr. Who" >> getClientId) `shouldReturn` ClientId "Dr. Who"

getServerInfoSpecs = describe "Database.Riak.getServerInfo" $ do
  it "returns server info without exploding" $ do
    withRiak $ void getServerInfo

basicCommands = describe "Basic Commands" $ do
  getSpecs
  putSpecs
  deleteSpecs

getSpecs = describe "Database.Riak.get" $ do
  it "can handle a get with no contents" $ do
    result <- withRiak $ get $ basic ("random-bucket-whatever" :: ByteString) ("supercalifragilisticexpialidocious" :: ByteString)
    assertEqual "result is empty" (content ^$ result) S.empty

putSpecs = describe "Database.Riak.put" $ do
  return ()

deleteSpecs = describe "Database.Riak.delete" $ do
  return ()

listBucketsSpecs = describe "Database.Riak.listBuckets" $ do
  it "lists buckets without exploding" $ do
    withRiak $ void listBuckets

listKeysSpecs = describe "Database.Riak.listKeys" $ do
  it "lists empty bucket without exploding" $ do
    withRiak $ void $ listKeys $ BucketName "jumping-beans-123-zippity-dee"
  it "lists bucket with 1 element without exploding" $ do
    withRiak $ void $ listKeys $ BucketName "one-element-bucket"
  it "lists buckets with lots of keys without exploding" $ do
    withRiak $ void $ listKeys $ BucketName "many-element-bucket"
    withRiak $ void $ listKeys $ BucketName "task"

getBucketSpecs = describe "Database.Riak.getBucket" $ do
  it "doesn't explode." $ do
    withRiak $ void $ getBucket $ BucketName "thingum"

setBucketSpecs = describe "Database.Riak.setBucket" $ do
  it "sets values like a boss" $ do
    withRiak $ void $ setBucket $ basic ("thingum" :: ByteString) (BucketProps (Just 3) (Just False))

mapReduceSpecs = describe "Database.Riak.mapReduce" $ do
  return ()

indexQuerySpecs = describe "Database.Riak.indexQuery" $ do
  return ()

searchQuerySpecs = describe "Database.Riak.searchQuery" $ do
  return ()
