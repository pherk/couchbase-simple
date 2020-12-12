{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances,
    OverloadedStrings #-}

module Database.Couchbase.Types where

import Control.DeepSeq
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Lex.Fractional as F (readSigned, readExponential)
import qualified Data.ByteString.Lex.Integral as I (readSigned, readDecimal)
import GHC.Generics

import Database.Couchbase.Protocol

------------------------------------------------------------------------------
-- Classes of types Couchbase understands
--
class CouchbaseArg a where
    encode :: a -> ByteString

class CouchbaseResult a where
    decode :: Reply -> Either Reply a

------------------------------------------------------------------------------
-- CouchbaseArg instances
--
instance CouchbaseArg ByteString where
    encode = id

instance CouchbaseArg Integer where
    encode = pack . show

instance CouchbaseArg Double where
    encode a
        | isInfinite a && a > 0 = "+inf"
        | isInfinite a && a < 0 = "-inf"
        | otherwise = pack . show $ a

------------------------------------------------------------------------------
-- CouchbaseResult instances
--
data Status = Ok | Pong | Status ByteString
    deriving (Show, Eq, Generic)

instance NFData Status

data CouchbaseType = None | String | Hash | List | Set | ZSet
    deriving (Show, Eq)

instance CouchbaseResult Reply where
    decode = Right

instance CouchbaseResult ByteString where
    decode (SingleLine s)  = Right s
    decode (Bulk (Just s)) = Right s
    decode r               = Left r

instance CouchbaseResult Integer where
    decode (Integer n) = Right n
    decode r           =
        maybe (Left r) (Right . fst) . I.readSigned I.readDecimal =<< decode r

instance CouchbaseResult Double where
    decode r = maybe (Left r) (Right . fst) . F.readSigned F.readExponential =<< decode r

instance CouchbaseResult Status where
    decode (SingleLine s) = Right $ case s of
        "OK"     -> Ok
        "PONG"   -> Pong
        _        -> Status s
    decode r = Left r

instance CouchbaseResult CouchbaseType where
    decode (SingleLine s) = Right $ case s of
        "none"   -> None
        "string" -> String
        "hash"   -> Hash
        "list"   -> List
        "set"    -> Set
        "zset"   -> ZSet
        _        -> error $ "Hedis: unhandled redis type: " ++ show s
    decode r = Left r

instance CouchbaseResult Bool where
    decode (Integer 1)    = Right True
    decode (Integer 0)    = Right False
    decode (Bulk Nothing) = Right False -- Lua boolean false = nil bulk reply
    decode r              = Left r

instance (CouchbaseResult a) => CouchbaseResult (Maybe a) where
    decode (Bulk Nothing)      = Right Nothing
    decode (MultiBulk Nothing) = Right Nothing
    decode r                   = Just <$> decode r

instance
    {-# OVERLAPPABLE #-}
    (CouchbaseResult a) => CouchbaseResult [a] where
    decode (MultiBulk (Just rs)) = mapM decode rs
    decode r                     = Left r
 
instance (CouchbaseResult a, CouchbaseResult b) => CouchbaseResult (a,b) where
    decode (MultiBulk (Just [x, y])) = (,) <$> decode x <*> decode y
    decode r                         = Left r

instance (CouchbaseResult k, CouchbaseResult v) => CouchbaseResult [(k,v)] where
    decode r = case r of
                (MultiBulk (Just rs)) -> pairs rs
                _                     -> Left r
      where
        pairs []         = Right []
        pairs (_:[])     = Left r
        pairs (r1:r2:rs) = do
            k   <- decode r1
            v   <- decode r2
            kvs <- pairs rs
            return $ (k,v) : kvs
