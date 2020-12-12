module Database.Couchbase.Pool where

import Control.Concurrent
import Control.Exception

data Pool a =
    Pool { poolMin :: Int, poolMax :: Int, poolUsed :: Int, poolFree :: [a] }

newConnPool low high newConn delConn = do
    cs <- handleSqlError . sequence . replicate low newConn
    mPool <- newMVar $ Pool low high 0 cs
    return (mPool, newConn, delConn)

delConnPool (mPool, newConn, delConn) = do
    pool <- takeMVar mPool
    if length (poolFree pool) /= poolUsed pool
      then putMVar mPool pool >> fail "pool in use"
      else mapM_ delConn $ poolFree pool

takeConn (mPool, newConn, delConn) = modifyMVar mPool $ \pool ->
    case poolFree pool of
        conn:cs ->
            return (pool { poolUsed = poolUsed pool + 1, poolFree = cs }, conn)
        _ | poolUsed pool < poolMax pool -> do
            conn <- handleSqlError newConn
            return (pool { poolUsed = poolUsed pool + 1 }, conn)
        _ -> fail "pool is exhausted"

putConn (mPool, newConn, delConn) conn = modifyMVar_ mPool $ \pool ->
    let used = poolUsed pool in
    if used > poolMin conn
      then handleSqlError (delConn conn) >> return (pool { poolUsed = used - 1 })
      else return $ pool { poolUsed = used - 1, poolFree = conn : poolFree pool }

withConn connPool = bracket (takeConn connPool) (putConn connPool)


