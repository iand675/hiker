{-# LANGUAGE OverloadedStrings #-}
module Operations where
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.Char8
import Database.Riak
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
  return ()

putSpecs = describe "Database.Riak.put" $ do
  return ()

deleteSpecs = describe "Database.Riak.delete" $ do
  return ()

listBucketsSpecs = describe "Database.Riak.listBuckets" $ do
  return ()

listKeysSpecs = describe "Database.Riak.listKeys" $ do
  return ()

getBucketSpecs = describe "Database.Riak.getBucket" $ do
  return ()

setBucketSpecs = describe "Database.Riak.setBucket" $ do
  return ()

mapReduceSpecs = describe "Database.Riak.mapReduce" $ do
  return ()

indexQuerySpecs = describe "Database.Riak.indexQuery" $ do
  return ()

searchQuerySpecs = describe "Database.Riak.searchQuery" $ do
  return ()
