-- |Interface to CouchBase
-- modelled after the Redis interface hedis
module Database.Couchbase (
    -- * How To Use This Module
    -- |
    -- Connect to a Couchbase server:
    --
    -- @
    -- -- connects to localhost:6379
    -- conn <- 'checkedConnect' 'defaultConnectInfo'
    -- @
    --
    -- Connect to a Couchbase server using TLS:
    --
    -- @
    -- -- connects to foobar.redis.cache.windows.net:6380
    -- import Network.TLS
    -- import Network.TLS.Extra.Cipher
    -- import Data.X509.CertificateStore
    -- import Data.Default.Class (def)
    -- (Just certStore) <- readCertificateStore "azure-redis.crt"
    -- let tlsParams = (defaultParamsClient "foobar.redis.cache.windows.net" "") { clientSupported = def { supportedCiphers = ciphersuite_strong }, clientShared = def { sharedCAStore = certStore } }
    -- let redisConnInfo = defaultConnectInfo { connectHost = "foobar.redis.cache.windows.net", connectPort = PortNumber 6380, connectTLSParams = Just tlsParams, connectAuth = Just "Foobar!" }
    -- conn <- checkedConnect redisConnInfo
    -- @
    --
    -- Send commands to the server:
    --
    -- @
    -- {-\# LANGUAGE OverloadedStrings \#-}
    -- ...
    -- 'runCouchbase' conn $ do
    --      'set' \"hello\" \"hello\"
    --      set \"world\" \"world\"
    --      hello <- 'get' \"hello\"
    --      world <- get \"world\"
    --      liftIO $ print (hello,world)
    -- @
    --
    -- disconnect all idle resources in the connection pool:
    --
    -- @
    -- 'disconnect' 'conn'
    -- @

    -- ** Error Behavior
    -- |
    --  [Operations against keys holding the wrong kind of value:] Outside of a
    --    transaction, if the Couchbase server returns an 'Error', command functions
    --    will return 'Left' the 'Reply'. The library user can inspect the error
    --    message to gain  information on what kind of error occured.
    --
    --  [Connection to the server lost:] In case of a lost connection, command
    --    functions throw a 'ConnectionLostException'. It can only be caught
    --    outside of 'runCouchbase'.
    --
    --  [Trying to connect to an unreachable server:] When trying to connect to
    --    a server that does not exist or can't be reached, the connection pool
    --    only starts the first connection when actually executing a call to
    --    the server. This can lead to discovering very late that the server is
    --    not available, for example when running a server that logs to Couchbase.
    --    To prevent this, run a 'ping' command directly after connecting or
    --    use the 'checkedConnect' function which encapsulates this behavior.
    --
    --  [Exceptions:] Any exceptions can only be caught /outside/ of 'runCouchbase'.
    --    This way the connection pool can properly close the connection, making
    --    sure it is not left in an unusable state, e.g. closed or inside a
    --    transaction.
    --

    -- * The Couchbase Monad
      Couchbase()
    , runCouchbase,
    unCouchbase, reCouchbase,
    CouchbaseCtx(..), MonadCouchbase(..),

    -- * Connection
    Connection, ConnectError(..)
    , connect
--    , checkedConnect
    , disconnect
    , withConnect
--    , withCheckedConnect:w
    , ConnectInfo(..), defaultConnectInfo, parseConnectInfo
--    , connectCluster,
    , PortID(..)

    -- * Commands
    , module Database.Couchbase.Commands

    -- * Transactions
--    module Database.Couchbase.Transactions,

    -- * Pub\/Sub
--    module Database.Couchbase.PubSub,

    -- * Low-Level Command API
--    sendRequest,
--    Reply(..)
    , Status(..)
    , CouchbaseResult(..)
    , ConnectionLostException(..)
    , ConnectTimeout(..)

    --
--    HashSlot, keyToSlot
) where

import Database.Couchbase.Core
import Database.Couchbase.Connection
    ( runCouchbase
--    , connectCluster
    , defaultConnectInfo
    , ConnectInfo(..)
    , disconnect
--    , checkedConnect
    , connect
    , ConnectError(..)
    , Connection(..)
    , withConnect
--    , withCheckedConnect
    )
import Database.Couchbase.ConnectionContext(PortID(..), ConnectionLostException(..), ConnectTimeout(..))
-- import Database.Couchbase.PubSub
import Database.Couchbase.Protocol
-- import Database.Couchbase.Transactions
import Database.Couchbase.Types
import Database.Couchbase.URL

import Database.Couchbase.Commands
-- import Database.Couchbase.Cluster.HashSlot(HashSlot, keyToSlot)
