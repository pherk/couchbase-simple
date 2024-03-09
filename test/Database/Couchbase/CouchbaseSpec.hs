{-# LANGUAGE OverloadedStrings #-}
module Database.Couchbase.CouchbaseSpec (spec) where

import Control.Monad
import Control.Exception (bracket)
import           Data.IORef
import qualified Data.ByteString.UTF8    as BU
import qualified Data.Text.Encoding      as TE
import           Database.Couchbase
import qualified Database.Couchbase.Core as Core
import           Database.Couchbase.Commands
import qualified Database.Couchbase.Connection as C
import qualified Database.Couchbase.ConnectionContext as CC
import           Database.Couchbase.Types 
import Foreign.Ptr
import Test.Hspec

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket (C.connect connectInfo) C.disconnect



spec :: Spec
spec = do
--    test_connect
    test_set
    test_get
    test_del
    test_query
    test_query_utf8

connectInfo :: C.ConnectInfo
connectInfo = C.ConnInfo
     { C.connectHost           = CC.HostName "192.168.178.24"
     , C.connectPort           = CC.PortNumber 8091
     , C.connectUser           = Nothing
     , C.connectAuth           = Nothing
     , C.connectBucket         = Just "nabu"
     , C.connectDatabase       = 0
     , C.connectMaxConnections = 50
     , C.connectMaxIdleTime    = 30
     , C.connectTimeout        = Nothing
     }

test_connect :: Spec
test_connect = 
  around withDatabaseConnection $ do
  describe "connect ping" $
    it "ping cb" $ \conn -> do
      (runCouchbase conn $ do ping) `shouldReturn` (Right Pong)

test_set :: Spec
test_set = 
  around withDatabaseConnection $ do
  describe "set" $
    it "set key value" $ \conn -> do
      (runCouchbase conn $ do set "key" "value") `shouldReturn` (Right Ok)

test_get :: Spec
test_get = 
  around withDatabaseConnection $ do
  describe "get" $
    it "get key" $ \conn -> do
      (runCouchbase conn $ do get "key") `shouldReturn` (Right (Just "value"))

test_del :: Spec
test_del = 
  around withDatabaseConnection $ do
  describe "del" $
    it "del key" $ \conn -> do
      (runCouchbase conn $ do del "key") `shouldReturn` (Right Ok)

test_query :: Spec
test_query = 
  around withDatabaseConnection $ do
  describe "query" $
    it "query: use keys key" $ \conn -> do
      let q = "select nabu.* from nabu USE KEYS [\"key\"]"
      (runCouchbase conn $ do 
          del "key"
          set "key" "{\"value\": 12345}"
          query q) `shouldReturn` (Right (Just ["{\"value\":12345}"]))
                              
test_query_utf8 :: Spec
test_query_utf8 = 
  around withDatabaseConnection $ do
  describe "query" $
    it "query: use keys key, value utf8: äöüßÄÖÜ" $ \conn -> do
      let q = "select nabu.* from nabu USE KEYS [\"key\"]"
      (runCouchbase conn $ do 
          del "key"
          set "key" (BU.fromString "{\"value\": \"äöüßÄÖÜ\"}")
          query q) `shouldReturn` (Right (Just ["{\"value\":\"\195\164\195\182\195\188\195\159\195\132\195\150\195\156\"}"]))

