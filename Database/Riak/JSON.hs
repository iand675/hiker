{-# LANGUAGE OverloadedStrings #-}
module Database.Riak.JSON where
import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Database.Riak.Basic
import Database.Riak.Lens (contentType)
import Database.Riak.Protocol.Content hiding (contentType)

jsonContent :: ToJSON a => a -> Content
jsonContent = set contentType (Just "application/json") . basic . encode

getJSON :: FromJSON a => Getter ByteString (Maybe a)
getJSON = to decode

setJSON :: ToJSON a => Setter a ByteString a ByteString
setJSON = sets (encode .)
