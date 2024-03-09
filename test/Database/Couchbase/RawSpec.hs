{-# LANGUAGE OverloadedStrings #-}

module Database.Couchbase.RawSpec (spec) where

import Control.Monad
import Control.Concurrent.MVar
import Database.Couchbase.Raw
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import           Data.IORef
import Foreign.Ptr
import Test.Hspec

spec :: Spec
spec = do
    test_connect
    test_ping
    test_get
    test_remove
    test_store
    test_query


assertLcbSuccess :: Monad m => String -> LcbStatus -> m ()
assertLcbSuccess msg err =
  when (err /= LcbSuccess) $ error msg


defaultParams :: ConnectionParams
defaultParams =
  ConnectionParams
  { cpConnectionString = "couchbase://192.168.178.24:8091/nabu"
  , cpUser = "admin"
  , cpPassword = "kikl968"
  , cpLcbType = LcbTypeBucket
  }


withConnection :: (Lcb -> IO ()) -> IO ()
withConnection f = do
  (err, lcb) <- lcbCreate defaultParams
  assertLcbSuccess "lcbCreate() failed" err
  lcbConnect lcb >>= assertLcbSuccess "lcbConnect() failed"
  lcbWait lcb LcbWaitDefault >>= assertLcbSuccess "lcbWait() failed"
  lcbGetBootstrapStatus lcb >>= assertLcbSuccess "lcbGetBootstrapStatus() failed"
  f lcb


setValue :: BU.ByteString -> BU.ByteString -> IO ()
setValue k v =
  withConnection $ \lcb -> do
    lcbStore lcb Nothing LcbStoreUpsert Nothing k v >>= assertLcbSuccess "lcbStore() failed"
    lcbWait lcb LcbWaitDefault >>= assertLcbSuccess "lcbWait() failed"


removeKey :: BU.ByteString -> IO ()
removeKey k =
  withConnection $ \lcb -> do
    lcbRemove lcb Nothing k >>= assertLcbSuccess "lcbRemove() failed"
    lcbWait lcb LcbWaitDefault >>= assertLcbSuccess "lcbWait() failed"


test_connect :: Spec
test_connect =
  describe "connect" $
    it "connect: to cb" $ do
      (err, lcb) <- lcbCreate defaultParams
      err `shouldBe` LcbSuccess
      lcbConnect lcb `shouldReturn` LcbSuccess
      lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
      lcbGetBootstrapStatus lcb `shouldReturn` LcbSuccess

test_ping :: Spec
test_ping =
  describe "ping" $
    it "ping: to cb" $ do
      withConnection $ \lcb ->
        lcbPing lcb Nothing `shouldReturn` LcbSuccess

test_get :: Spec
test_get =
  describe "lcbGet" $ do
    it "LcbGet: succeeds" $ do
      setValue "key" "value"
      withConnection $ \lcb ->
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess

    it "LcbGet: calls callback" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \resp -> do
          lcbRespGetGetStatus resp `shouldReturn` LcbSuccess
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

test_remove :: Spec
test_remove =
  describe "lcbRemove" $ do
    it "LcbRemove: succeeds" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbRemove lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "lcbRemove: calls callback" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbInstallRemoveCallback lcb $ \resp ->
          lcbRespRemoveGetStatus resp `shouldReturn` LcbSuccess
        lcbRemove lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "lcbRemove: actually removes key" $ do
      setValue "key" "value"
      withConnection $ \lcb -> do
        lcbRemove lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetStatus resp `shouldReturn` LcbErrDocumentNotFound
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "lcbRemove: returns error in callback when there is nothing to remove" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbInstallRemoveCallback lcb $ \resp ->
          lcbRespRemoveGetStatus resp `shouldReturn` LcbErrDocumentNotFound
        lcbRemove lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

test_store :: Spec
test_store =
  describe "lcbStore" $ do
    it "LcbStore: replaces existing key with LcbStoreReplace" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing LcbStoreReplace Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback with LcbStoreReplace when there is no key to replace" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp ->
          lcbRespStoreGetStatus resp `shouldReturn` LcbErrDocumentNotFound
        lcbStore lcb Nothing LcbStoreReplace Nothing "key" "value" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: replaces existing key with LcbStoreUpsert" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing LcbStoreUpsert Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: sets value with LcbUpsert even when there is no existing key" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing LcbStoreUpsert Nothing "key" "value" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: adds value with LcbStoreInsert" $ do
      removeKey "key"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing  LcbStoreInsert Nothing "key" "value" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback with LcbStoreUpsert when there is the key in database already" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp ->
          lcbRespStoreGetStatus resp `shouldReturn` LcbErrDocumentExists
        lcbStore lcb Nothing LcbStoreInsert Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: calls callback" $ do
      withConnection $ \lcb -> do
        lcbInstallStoreCallback lcb $ \resp -> do
          lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
          lcbRespStoreGetOp resp `shouldReturn` (LcbSuccess, LcbStoreUpsert)
        lcbStore lcb Nothing LcbStoreUpsert Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: appends value with LcbStoreAppend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing LcbStoreAppend Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value1value2"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: prepends value with LcbStorePrepend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbStore lcb Nothing LcbStorePrepend Nothing "key" "value2" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbInstallGetCallback lcb $ \resp ->
          lcbRespGetGetValue resp `shouldReturn` "value2value1"
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: changes CAS with LcbStoreReplace" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s,oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` (LcbSuccess, oldCas)
          lcbStore lcb Nothing  LcbStoreReplace (Just oldCas) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: changes CAS with LcbStoreUpsert" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` (LcbSuccess, oldCas)
          lcbStore lcb Nothing  LcbStoreUpsert (Just oldCas) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: changes CAS with LcbStoreAppend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` (LcbSuccess, oldCas)
          lcbStore lcb Nothing LcbStoreAppend (Just oldCas) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: changes CAS with LcbStorePrepend" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp -> do
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
            lcbRespStoreGetCas resp `shouldNotReturn` (LcbSuccess, oldCas)
          lcbStore lcb Nothing LcbStorePrepend (Just oldCas) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback when you try LcbStoreReplace with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing LcbStoreReplace (Just (oldCas - 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
{- Upsert ignores CAS, CAS should only used for Update Ops (Replace, Append, Prepend) 
    it "LcbStore: returns error in callback when you try LcbStoreUpsert with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrDocumentExists
          lcbStore lcb Nothing LcbStoreUpsert (Just (oldCas - 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
-}
    it "LcbStore: returns error in callback when you try LcbStoreAppend with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing  LcbStoreAppend (Just (oldCas - 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback when you try LcbStorePrepend with CAS - 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing  LcbStorePrepend (Just (oldCas - 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback when you try LcbStoreReplace with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing  LcbStoreReplace (Just (oldCas + 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
--
--  "LcbStore: try LcbStoreUpsert with CAS + 1, ignores CAS; should be used only for Update Ops (Replace, Append, Prepend)"
--  produces coredump in libcouchbase
--
{-
    it "LcbStore: try LcbStoreUpsert with CAS + 1, ignores CAS; should be used only for Update Ops (Replace, Append, Prepend)" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbSuccess
          lcbStore lcb Nothing  LcbStoreUpsert (Just (oldCas + 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
-}
    it "LcbStore: returns error in callback when you try LcbStoreAppend with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing LcbStoreAppend (Just (oldCas + 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

    it "LcbStore: returns error in callback when you try LcbStorePrepend with CAS + 1" $ do
      setValue "key" "value1"
      withConnection $ \lcb -> do
        lcbInstallGetCallback lcb $ \respGet -> do
          (s, oldCas) <- lcbRespGetGetCas respGet
          lcbInstallStoreCallback lcb $ \resp ->
            lcbRespStoreGetStatus resp `shouldReturn` LcbErrCasMismatch
          lcbStore lcb Nothing  LcbStorePrepend (Just (oldCas + 1)) "key" "value2" `shouldReturn` LcbSuccess
          lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        lcbGet lcb Nothing "key" `shouldReturn` LcbSuccess
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess

test_query :: Spec
test_query =
  describe "lcbQuery" $ do
    it "lcbQuery: query existing key, value with umlauts" $ do
      -- value has to be in JSON format otherwise it is stored as binary
      setValue "key" (BU.fromString "{\"value\": \"äöüßÄÖÜ\"}")
      meta <- newEmptyMVar
      -- {-# NOINLINE result #-}
      result <- newIORef []
      withConnection $ \lcb -> do
--only with index        lcbQuery lcb Nothing "select nabu.* from nabu n where META(n).id='key'" $ \resp -> do
        lcbQuery lcb Nothing "select nabu.* from nabu USE KEYS [\"key\"]" $ qcbw meta result
        lcbWait lcb LcbWaitDefault `shouldReturn` LcbSuccess
        -- putStrLn "lcbQuery: meta:"
        takeMVar meta >>= B8.putStrLn
        readIORef result >>= print


qcbw m r = qcb m r

qcb :: MVar BU.ByteString -> IORef [BU.ByteString] -> LcbQueryCallback 
qcb m r resp = do
  lcbRespQueryGetStatus resp `shouldReturn` LcbSuccess
  row <- lcbRespqueryGetRow resp 
  isFinal <- lcbRespqueryIsFinal resp
  rs <- readIORef r
  case isFinal of
    0 -> writeIORef r  $ (row : rs)
    _ -> putMVar m row
