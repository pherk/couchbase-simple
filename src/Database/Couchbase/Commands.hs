module Database.Couchbase.Commands (

  ping -- 
      {-
  -- ** Keys
  , del -- |Delete a key (<http://redis.io/commands/del>). Since Couchbase 1.0.0
  , keys -- |Find all keys matching the given pattern (<http://redis.io/commands/keys>). Since Couchbase 1.0.0
  , move -- |Move a key to another database (<http://redis.io/commands/move>). Since Couchbase 1.0.0

  -- ** Strings
  , append -- |Append a value to a key (<http://redis.io/commands/append>). Since Couchbase 2.0.0
  , get -- |Get the value of a key (<http://redis.io/commands/get>). Since Couchbase 1.0.0
  , quit --
 -}
) where

import Prelude hiding (min,max)
import           Data.ByteString (ByteString)
-- import Database.Couchbase.Core(CouchbaseCtx, sendRequest)
import qualified Database.Couchbase.Core as C
import           Database.Couchbase.Types

-- the ping command. used in 'checkedconnect'.
ping
    :: (C.CouchbaseCtx m f)
    => m (f Status)
ping  = C.ping

    {-
get
    :: (CouchbaseCtx m f)
    => ByteString -- ^ key
    -> m (f (Maybe ByteString))
get key = sendRequest (["GET"] ++ [encode key] )

keys
    :: (CouchbaseCtx m f)
    => ByteString -- ^ pattern
    -> m (f [ByteString])
keys pat = sendRequest (["KEYS"] ++ [encode pat] )

del
    :: (CouchbaseCtx m f)
    => [ByteString] -- ^ key
    -> m (f Integer)
del key = sendRequest (["DEL"] ++ map encode key )

append
    :: (CouchbaseCtx m f)
    => ByteString -- ^ key
    -> ByteString -- ^ value
    -> m (f Integer)
append key value = sendRequest (["APPEND"] ++ [encode key] ++ [encode value] )

move
    :: (CouchbaseCtx m f)
    => ByteString -- ^ key
    -> Integer -- ^ db
    -> m (f Bool)
move key db = sendRequest (["MOVE"] ++ [encode key] ++ [encode db] )


quit
    :: (CouchbaseCtx m f)
    => m (f Status)
quit  = sendRequest (["QUIT"] )
-}
