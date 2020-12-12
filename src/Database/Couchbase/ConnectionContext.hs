{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Couchbase.ConnectionContext (
    ConnectionContext(..)
  , ConnectTimeout(..)
  , ConnectionLostException(..)
  , HostName(..)
  , PortID(..)
  , connect
  , disconnect
--  , send
--  , recv
  , errConnClosed
  , flush
  , ioErrorToConnLost
) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import Control.Monad(when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.IORef as IOR
import Control.Concurrent.MVar(newMVar, readMVar, swapMVar)
import Control.Exception(bracketOnError, Exception, throwIO, try)
import           Data.Typeable
import Data.Functor(void)
import System.IO(Handle, hSetBinaryMode, hClose, IOMode(..), hFlush, hIsOpen)
import System.IO.Error(catchIOError)
import qualified Database.Couchbase.Raw as Raw


data ConnectionContext = NormalHandle Raw.Lcb

data Connection = Connection
    { ctx :: ConnectionContext
    , lastRecvRef :: IOR.IORef (Maybe B.ByteString) }

instance Show Connection where
    show Connection{..} = "Connection{ ctx = " ++ "Lcb" ++ ", lastRecvRef = IORef}"

data ConnectPhase
  = PhaseUnknown
  | PhaseResolve
  | PhaseOpenSocket
  deriving (Show)

newtype ConnectTimeout = ConnectTimeout ConnectPhase
    deriving (Show, Typeable)

instance Exception ConnectTimeout

data ConnectionLostException = ConnectionLost deriving Show
instance Exception ConnectionLostException

data HostName = HostName String
    deriving Show

data PortID = PortNumber Int
            | UnixSocket String
    deriving (Eq, Show)

defaultParams :: Raw.ConnectionParams
defaultParams =
   Raw.ConnectionParams
   { connectionString = "couchbase://192.168.178.24:8091/nabu"
   , user = Just "erlang"
   , password = Just "5RZz(8e^y.N(+y_H"
   , lcbType = Raw.LcbTypeBucket
   }


connect :: HostName -> PortID -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe B.ByteString -> Maybe Int -> IO ConnectionContext
connect hostName portId mu mp mb timeoutOpt =
  bracketOnError connect disconnect $ \h -> do
    return $ NormalHandle h
  where
        connect = do
            (s,lcb) <- Raw.lcbCreate defaultParams
            Raw.lcbConnect lcb
            Raw.lcbWait lcb Raw.LcbWaitDefault
            s <-  Raw.lcbGetBootstrapStatus lcb
            case s of
              Raw.LcbSuccess -> return lcb
              _              -> error "failed"
        disconnect = do
            Raw.lcbDestroy 

ioErrorToConnLost :: IO a -> IO a
ioErrorToConnLost a = a `catchIOError` const errConnClosed

errConnClosed :: IO a
errConnClosed = throwIO ConnectionLost



disconnect :: ConnectionContext -> IO ()
disconnect (NormalHandle h) = do
    Raw.lcbDestroy h

flush :: ConnectionContext -> IO ()
flush (NormalHandle h) = do
    s <- Raw.lcbWait h Raw.LcbWaitDefault
    return ()
