{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Couchbase.Core.Internal where

import Control.Monad.Reader
import Data.IORef
import           Database.Couchbase.Protocol
import qualified Database.Couchbase.ProtocolPipelining as PP


-- |Context for normal command execution, outside of transactions. Use
--  'runCouchbase' to run actions of this type.
--
--  In this context, each result is wrapped in an 'Either' to account for the
--  possibility of Couchbase returning an 'Error' reply.
newtype Couchbase a =
  Couchbase (ReaderT CouchbaseEnv IO a)
  deriving (Monad, MonadIO, Functor, Applicative)

deriving instance MonadFail Couchbase

data CouchbaseEnv
    = NonClusteredEnv { envConn :: PP.Connection, envLastReply :: IORef Reply }
