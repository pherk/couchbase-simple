-- |Maintains a persistent connection pool to a Couchbase database server.
-- connPool <- newConnPool 0 50 (connectCB3 "user.db") disconnect
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Database.Couchbase.Connection where

import Data.IORef
import Control.Concurrent
import Control.Exception (bracket)
import Control.Applicative
import Control.Monad (ap,liftM)
import Control.Monad.Trans (MonadIO (..))
import qualified Data.ByteString as BS (ByteString, length)
import qualified Data.ByteString.UTF8 as UTF8 (fromString, toString)
import           Data.Maybe (fromJust)
import           Data.Pool(Pool, withResource, createPool, destroyAllResources)

import Control.Exception
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad(when)
import Control.Concurrent.MVar(MVar, newMVar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import Data.Functor(void)
import qualified Data.IntMap.Strict as IntMap
import Data.Typeable
import qualified Data.Time as Time
import qualified Data.Map.Strict as HM

import qualified Database.Couchbase.Raw as Raw
import qualified Database.Couchbase.ConnectionContext as CC
import qualified Database.Couchbase.ProtocolPipelining as PP
import           Database.Couchbase.Core(Couchbase, runCouchbaseInternal)
import           Database.Couchbase.Protocol(Reply(..))




-- |A computation that interacts with a Couchbase database.  This monad
-- encapsulates the 'IO' monad, a persistent connnection via libCouchbase3 to a
-- Couchbase database and enough information to re-open the connection
-- if it is closed.
data CouchMonad a = CouchMonad (Connection -> IO (a,Connection))

instance Applicative CouchMonad where
  pure = return
  (<*>) = ap

instance Functor CouchMonad where
  fmap = liftM

instance Monad CouchMonad where

  return a = CouchMonad $ \conn -> return (a,conn)

  (CouchMonad m) >>= k = CouchMonad $ \conn -> do
    (a,conn') <- m conn
    let (CouchMonad m') = k a
    m' conn'

#if MIN_VERSION_base(4,13,0)
instance MonadFail CouchMonad where
#endif
  fail msg = CouchMonad $ \conn -> do
    fail $ "internal error: " ++ msg

instance MonadIO CouchMonad where

  liftIO m = CouchMonad $ \conn -> m >>= \a -> return (a,conn)

{-
makeURL :: String -- ^path
        -> [(String,String)]
        -> CouchMonad URI
makeURL path query = CouchMonad $ \conn -> do
  return ( (ccURI conn) { uriPath = '/':path
                        , uriQuery = '?':(urlEncodeVars query) 
                        }
         ,conn )

getConn :: CouchMonad (HandleStream BS.ByteString)
getConn = CouchMonad $ \conn -> do
  r <- readIORef (ccConn conn)
  return (r,conn)
  
getConnAuth :: CouchMonad (Maybe Authority)
getConnAuth = CouchMonad $ \conn -> return ((ccAuth conn),conn)
-}
--------------------------------------------------------------------------------
-- Connection
--

-- |A threadsafe pool of connections to a Couchbase server. Use the
--  'connect' function to create one.
data Connection = Connection (Pool PP.Connection)

-- |Information for connnecting to a Couchbase server.
--
-- It is recommended to not use the 'ConnInfo' data constructor directly.
-- Instead use 'defaultConnectInfo' and update it with record syntax. For
-- example to connect to a password protected Couchbase server running on localhost
-- and listening to the default port:
--
-- @
-- myConnectInfo :: ConnectInfo
-- myConnectInfo = defaultConnectInfo {connectAuth = Just \"secret\"}
-- @
--
data ConnectInfo = ConnInfo
    { connectHost           :: CC.HostName
    , connectPort           :: CC.PortID
    , connectUser           :: Maybe String
    , connectAuth           :: Maybe String
    -- ^ When the server is protected by a password, set 'connectAuth' to 'Just'
    --   the password. Each connection will then authenticate by the 'auth'
    --   command.
    , connectBucket         :: Maybe String
    , connectDatabase       :: Integer
    -- ^ Each connection will 'select' the database with the given index.
    , connectMaxConnections :: Int
    -- ^ Maximum number of connections to keep open. The smallest acceptable
    --   value is 1.
    , connectMaxIdleTime    :: Time.NominalDiffTime
    -- ^ Amount of time for which an unused connection is kept open. The
    --   smallest acceptable value is 0.5 seconds. If the @timeout@ value in
    --   your redis.conf file is non-zero, it should be larger than
    --   'connectMaxIdleTime'.
    , connectTimeout        :: Maybe Time.NominalDiffTime
    -- ^ Optional timeout until connection to Couchbase gets
    --   established. 'ConnectTimeoutException' gets thrown if no socket
    --   get connected in this interval of time.
    } deriving Show

data ConnectError = ConnectAuthError Raw.LcbStatus
                  | ConnectSelectError Raw.LcbStatus
    deriving (Eq, Show, Typeable)

instance Exception ConnectError

-- |Default information for connecting:
--
-- @
--  connectHost           = HostName \"localhost\"
--  connectPort           = PortNumber 6379 -- Couchbase default port
--  connectAuth           = Nothing         -- No password
--  connectDatabase       = 0               -- SELECT database 0
--  connectMaxConnections = 50              -- Up to 50 connections
--  connectMaxIdleTime    = 30              -- Keep open for 30 seconds
--  connectTimeout        = Nothing         -- Don't add timeout logic
-- @
--
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnInfo
    { connectHost           = CC.HostName "couchbase://localhost"
    , connectPort           = CC.PortNumber 8091
    , connectUser           = Nothing
    , connectAuth           = Nothing
    , connectBucket         = Just "default"
    , connectDatabase       = 0
    , connectMaxConnections = 50
    , connectMaxIdleTime    = 30
    , connectTimeout        = Nothing
    }

createConnection :: ConnectInfo -> IO PP.Connection
createConnection ConnInfo{..} = do
    let timeoutOptUs =
          round . (1000000 *) <$> connectTimeout
    conn <- PP.connect connectHost connectPort connectUser connectAuth connectBucket timeoutOptUs

    runCouchbaseInternal conn $ do return ()
    return conn

-- |Constructs a 'Connection' pool to a Couchbase server designated by the
--  given 'ConnectInfo'. The first connection is not actually established
--  until the first call to the server.
connect :: ConnectInfo -> IO Connection
connect cInfo@ConnInfo{..} = Connection <$>
    createPool (createConnection cInfo) PP.disconnect 1 connectMaxIdleTime connectMaxConnections

{-
-- |Constructs a 'Connection' pool to a Couchbase server designated by the
--  given 'ConnectInfo', then tests if the server is actually there.
--  Throws an exception if the connection to the server can't be
--  established.
checkedConnect :: ConnectInfo -> IO Connection
checkedConnect connInfo = do
    conn <- connect connInfo
    runCouchbase conn $ void _
    return conn
-}

-- |Destroy all idle resources in the pool.
disconnect :: Connection -> IO ()
disconnect (Connection pool) = destroyAllResources pool

-- | Memory bracket around 'connect' and 'disconnect'.
withConnect :: (Catch.MonadMask m, MonadIO m) => ConnectInfo -> (Connection -> m c) -> m c
withConnect connInfo = Catch.bracket (liftIO $ connect connInfo) (liftIO . disconnect)

{-
-- | Memory bracket around 'checkedConnect' and 'disconnect'
withCheckedConnect :: ConnectInfo -> (Connection -> IO c) -> IO c
withCheckedConnect connInfo = bracket (checkedConnect connInfo) disconnect
-}

-- |Interact with a Couchbase bucket specified by the given 'Connection'.
--
--  Each call of 'runCouchbase' takes a network connection from the 'Connection'
--  pool and runs the given 'Couchbase' action. Calls to 'runCouchbase' may thus block
--  while all connections from the pool are in use.
runCouchbase :: Connection -> Couchbase a -> IO a
runCouchbase (Connection pool) cb =
  withResource pool $ \conn -> runCouchbaseInternal conn cb
