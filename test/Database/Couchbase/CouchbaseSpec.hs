module Database.Couchbase.CouchbaseSpec (spec) where

import Control.Monad
import Control.Exception (bracket)
import qualified Data.ByteString as B
import           Data.IORef
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
    test_connect

connectInfo :: C.ConnectInfo
connectInfo = C.ConnInfo
     { C.connectHost           = CC.HostName "couchbase://192.168.178.24"
     , C.connectPort           = CC.PortNumber 8091
     , C.connectUser           = Just "erlang"
     , C.connectAuth           = Just "5RZz(8e^y.N(+y_H"
     , C.connectBucket         = Just "nabu"
     , C.connectDatabase       = 0
     , C.connectMaxConnections = 50
     , C.connectMaxIdleTime    = 30
     , C.connectTimeout        = Nothing
     }

test_connect :: Spec
test_connect = 
  around withDatabaseConnection $ do
  describe "connect" $
    it "ping cb" $ \conn -> do
      (runCouchbase conn $ do ping) `shouldReturn` (Right (Pong))

