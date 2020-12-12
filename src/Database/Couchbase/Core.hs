{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RecordWildCards,
    MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    DeriveDataTypeable, StandaloneDeriving #-}

module Database.Couchbase.Core (
    Couchbase(), unCouchbase, reCouchbase
  , CouchbaseCtx(..), MonadCouchbase(..)
--    send, recv, sendRequest,
  , runCouchbaseInternal
  , CouchbaseEnv(..)
  , ping
) where

import           Control.Monad.Reader
import qualified Data.ByteString as B
import           Data.IORef
import           Database.Couchbase.Core.Internal
import           Database.Couchbase.Protocol
import qualified Database.Couchbase.ProtocolPipelining as PP
import           Database.Couchbase.Raw as Raw
import           Database.Couchbase.Types


--------------------------------------------------------------------------------
-- The Couchbase Monad
--

-- |This class captures the following behaviour: In a context @m@, a command
--  will return its result wrapped in a \"container\" of type @f@.
--
--  Please refer to the Command Type Signatures section of this page for more
--  information.
class (MonadCouchbase m) => CouchbaseCtx m f | m -> f where
    returnDecode :: CouchbaseResult a => Reply -> m (f a)

class (Monad m) => MonadCouchbase m where
    liftCouchbase :: Couchbase a -> m a


instance CouchbaseCtx Couchbase (Either Reply) where
    returnDecode = return . decode

instance MonadCouchbase Couchbase where
    liftCouchbase = id

-- |Deconstruct Couchbase constructor.
--
--  'unCouchbase' and 'reCouchbase' can be used to define instances for
--  arbitrary typeclasses.
--
--  WARNING! These functions are considered internal and no guarantee
--  is given at this point that they will not break in future.
unCouchbase :: Couchbase a -> ReaderT CouchbaseEnv IO a
unCouchbase (Couchbase r) = r

-- |Reconstruct Couchbase constructor.
reCouchbase :: ReaderT CouchbaseEnv IO a -> Couchbase a
reCouchbase r = Couchbase r

-- |Internal version of 'runCouchbase' that does not depend on the 'Connection'
--  abstraction. Used to run the AUTH command when connecting.
runCouchbaseInternal :: PP.Connection -> Couchbase a -> IO a
runCouchbaseInternal conn (Couchbase cb) = do
  -- Dummy reply in case no request is sent.
  ref <- newIORef (SingleLine "nobody will ever see this")
  r <- runReaderT cb (NonClusteredEnv conn ref)
  -- Evaluate last reply to keep lazy IO inside runCouchbase.
  readIORef ref >>= (`seq` return ())
  return r

setLastReply :: Reply -> ReaderT CouchbaseEnv IO ()
setLastReply r = do
  ref <- asks envLastReply
  lift (writeIORef ref r)

{-
recv :: (MonadCouchbase m) => m Reply
recv = liftCouchbase $ Couchbase $ do
  conn <- asks envConn
  r <- liftIO (PP.recv conn)
  setLastReply r
  return r

send :: (MonadCouchbase m) => [B.ByteString] -> m ()
send req = liftCouchbase $ Couchbase $ do
    conn <- asks envConn
    liftIO $ PP.send conn (renderRequest req)

-- |'sendRequest' can be used to implement commands from experimental
--  versions of Couchbase. An example of how to implement a command is given
--  below.
--
-- @
-- -- |Couchbase DEBUG OBJECT command
-- debugObject :: ByteString -> 'Couchbase' (Either 'Reply' ByteString)
-- debugObject key = 'sendRequest' [\"DEBUG\", \"OBJECT\", key]
-- @
--
sendRequest :: (CouchbaseCtx m f, CouchbaseResult a)
    => [B.ByteString] -> m (f a)
sendRequest req = do
    r' <- liftCouchbase $ Couchbase $ do
        env <- ask
        case env of
            NonClusteredEnv{..} -> do
                r <- liftIO $ PP.request envConn (renderRequest req)
                setLastReply r
                return r
    returnDecode r'
-}
ping :: (CouchbaseCtx m f, CouchbaseResult a)
    => m (f a)
ping = do
     r' <- liftCouchbase $ Couchbase $ do
         env <- ask
         case env of
             NonClusteredEnv{..} -> do
                 r <- liftIO $ do
                     s <- Raw.lcbPing (PP.handle envConn) Nothing
                     case s of
                       Raw.LcbSuccess -> return $ SingleLine "PONG"
                       _              -> return $ Error "LcbNoMatchingServer"
                 setLastReply r
                 return r
     returnDecode r'
