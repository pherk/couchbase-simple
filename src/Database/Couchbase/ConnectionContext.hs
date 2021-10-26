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
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.IORef as IOR
import           Data.Maybe (fromMaybe)
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

instance Show HostName where
  show (HostName h) = h

data PortID = PortNumber Int
            | UnixSocket String
    deriving (Eq)

instance Show PortID where
  show (PortNumber p) = show p
  show (UnixSocket s) = s

mkConnectionParams :: HostName -> PortID -> Maybe String -> Maybe String -> Maybe String -> Raw.ConnectionParams
mkConnectionParams host port us pw bu =
   Raw.ConnectionParams
   { cpConnectionString = cs
   , cpUser = user
   , cpPassword = pwd
   , cpLcbType = Raw.LcbTypeBucket
   }
  where cs = "couchbase://" ++ (show host) ++ ":" ++ (show port) ++ "/" ++ bucket
        bucket = fromMaybe "default" bu
        user   = fromMaybe "" us
        pwd    = fromMaybe "" pw


connect :: HostName -> PortID -> Maybe String -> Maybe String -> Maybe String -> Maybe Int -> IO ConnectionContext
connect host port mu mp mb timeoutOpt =
  bracketOnError connect disconnect $ \h -> do
    return $ NormalHandle h
  where
        connect = do
            (s',lcb) <- Raw.lcbCreate connParams
            Raw.lcbConnect lcb
            Raw.lcbWait lcb Raw.LcbWaitDefault
            s <-  Raw.lcbGetBootstrapStatus lcb
            case s of
              Raw.LcbSuccess -> return lcb
              _              -> error "failed"
        disconnect = do
            Raw.lcbDestroy 
        connParams = mkConnectionParams host port mu mp mb

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
