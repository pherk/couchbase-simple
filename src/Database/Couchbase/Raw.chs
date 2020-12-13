{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Couchbase.Raw where


import           Control.Monad (liftM)
import           Control.Concurrent.MVar
import           Codec.Binary.UTF8.String (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as B8
import Foreign
import Foreign.C.String
import Foreign.C.Types
import C2HS.C.Extra.Marshal
import Debug.Trace

#define __int128 int


#include <libcouchbase/couchbase.h>
#include <libcouchbase/internalstructs.h>

{# context lib="libcouchbase" #}

{# enum lcb_INSTANCE_TYPE as LcbType {underscoreToCase} deriving (Eq, Show) #}


{# pointer *lcb_CREATEOPTS as LcbCreateOpts foreign finalizer lcb_createopts_destroy newtype #}


{# pointer *lcb_INSTANCE as Lcb foreign finalizer lcb_destroy newtype #}

{# fun lcb_destroy as ^ {`Lcb'} -> `()' #}

{# enum lcb_STATUS as LcbStatus {underscoreToCase} deriving (Eq, Show) #}


-- {# fun lcb_get_errtype as ^ {`LcbStatus'} -> `Int' #}


{# fun lcb_strerror_short as ^ {`LcbStatus'} -> `String' #}

newLcbCreateOpts :: Ptr LcbCreateOpts -> IO LcbCreateOpts
newLcbCreateOpts ptr =
  LcbCreateOpts <$> newForeignPtr lcb_createopts_destroy ptr

peekLcbOptsPtr :: Ptr (Ptr LcbCreateOpts) -> IO LcbCreateOpts
peekLcbOptsPtr ptr =
  peek ptr >>= newLcbCreateOpts

{# fun lcb_createopts_create as ^ {alloca- `LcbCreateOpts' peekLcbOptsPtr*, `LcbType'} -> `LcbStatus' #}
{# fun lcb_createopts_connstr as ^ {`LcbCreateOpts', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_createopts_credentials as ^ {`LcbCreateOpts', `String', `Int', `String', `Int'} -> `LcbStatus' #}

newLcb :: Ptr Lcb -> IO Lcb
newLcb ptr =
  Lcb <$> newForeignPtr lcb_destroy ptr


peekLcb :: Ptr (Ptr Lcb) -> IO Lcb
peekLcb ptr =
  peek ptr >>= newLcb


{# fun lcb_create as lcbCreateRaw {alloca- `Lcb' peekLcb*, `LcbCreateOpts'} -> `LcbStatus' #}

data ConnectionParams =
  ConnectionParams
  { connectionString :: String
  , user :: Maybe String
  , password :: Maybe String
  , lcbType :: LcbType
  } deriving (Show)


lcbCreate :: ConnectionParams -> IO (LcbStatus, Lcb)
lcbCreate params = do 
  (s,o) <- lcbCreateoptsCreate  $ lcbType params
  -- putStrLn ("lcbCreateoptsCreate : " ++ show s)
  s' <- lcbCreateoptsConnstr o cs cslen 
  -- putStrLn ("lcbCreateoptsCreate : " ++ show s')
  s'' <- lcbCreateoptsCredentials o us uslen pws pwslen 
  -- putStrLn ("lcbCreateoptsCreate : " ++ show s'')
  lcbCreateRaw o
  where cs = "couchbase://192.168.178.24:8091/nabu"
        cslen = length cs
        us = "admin" -- "erlang"
        uslen = length us
        pws = "kikl968" -- "5RZz(8e^y.N(+y_H"
        pwslen = length pws


{# fun lcb_connect as ^ {`Lcb'} -> `LcbStatus' #}

{# fun lcb_get_bootstrap_status as ^ {`Lcb'} -> `LcbStatus' #}

{# enum lcb_RESPFLAGS as LcbRespFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum lcb_CALLBACK_TYPE as LcbCallbackType {underscoreToCase} deriving (Eq, Show) #}
{# enum lcb_DURABILITY_LEVEL as LcbDurabilityLevel {underscoreToCase} deriving (Eq, Show) #}


type LcbCallbackRaw =
  Ptr Lcb -> CInt -> Ptr () -> IO ()


foreign import ccall "wrapper"
  mkLcbCallbackFunPtr :: LcbCallbackRaw -> IO (FunPtr LcbCallbackRaw)


type LcbRespCallback =
  LcbCallbackType -> Ptr () -> IO ()


withLcbCallback :: LcbRespCallback -> (FunPtr LcbCallbackRaw -> IO a) -> IO a
withLcbCallback callback f =
  mkLcbCallbackFunPtr (\ _ cb p -> callback (toEnum (fromIntegral cb)) p) >>= f


type OldRespCallbackPtr = FunPtr LcbCallbackRaw


{# fun lcb_install_callback as ^ {`Lcb', `LcbCallbackType', withLcbCallback* `LcbRespCallback'} -> `OldRespCallbackPtr' id #}

-- {# pointer *lcb_RESPCALLBACK as LcbRespCallback foreign newtype #}
-- {# fun lcb_get_callback as ^ {`Lcb', `LcbCallbackType'} -> `LcbRespCallback' #}  


{# pointer *lcb_RESPGET as LcbRespGetPtr #}

data LcbRespGet = LcbRespGet
  {
    ctx'LcbRespGet :: Ptr ()
  , cookie'LcbRespGet :: Ptr ()
  , rflags'LcbRespGet :: CUShort
  , value'LcbRespGet :: Ptr ()
  , nvalue'LcbRespGet :: CSize
  , bufh'LcbRespGet :: Ptr ()
  , datatype'LcbRespGet :: CUChar
  , itmflags'LcbRespGet :: CUInt
  }

instance Storable LcbRespGet where
  alignment _ = alignment (undefined :: CInt)
  sizeOf _ = {#sizeof lcb_RESPGET#}
  peek p = undefined
  poke p _ = undefined

{-
data LcbCmdGet = LcbCmdGet
  {
    cmdflags'LcbCmdGet :: CUInt
  , exptime'LcbCmdGet :: CUInt
  , cas'LcbCmdGet :: CULong
  , cid'LcbCmdGet :: CUInt
  , scope'LcbCmdGet :: Ptr
  , nscope'LcbCmdGet :: CSize
  , collection'LcbCmdGet :: Ptr
  , ncollection'LcbCmdGet :: CSize
  , key'LcbCmdGet :: Ptr                  lcb_KEYBUF key;                                                                                                    \
  , timeout'LcbCmdGet :: CUInt
  , pspan'LcbCmdGet :: Ptr
  , lock'LcbCmdGet :: CInt
  }

instance Storable LcbCmdGet where
  alignment _ = alignment (undefined :: CInt)
  sizeOf _ = {#sizeof lcb_CMDGET#}
  peek p = undefined
  poke p _ = undefined



-- | Peek from pointer then cast to another integral type.
peekIntegral :: (Integral a, Storable a, Integral b) => Ptr a -> IO b
peekIntegral p = if p == nullPtr
                    then return 0
                    else fromIntegral <$> peek p

{-# SPECIALIZE peekIntegral :: Ptr CInt -> IO Int #-}

-- | Peek string from a two-dimension pointer of CChar.
peekString :: Ptr (Ptr CChar) -> IO String
peekString p = if p == nullPtr
                  then return ""
                  else peek p >>= \p' ->
                      if p' == nullPtr
                         then return ""
                         else peekCString p'

-}

peekCAS :: Ptr (CULong) -> IO CULong
peekCAS ptr =
  (peek ptr) 

peekCULong :: Ptr (CULong) -> IO Int
peekCULong ptr =
  liftM fromIntegral (peek ptr) 

peekCUInt :: Ptr (CUInt) -> IO Int
peekCUInt ptr =
  liftM fromIntegral (peek ptr) 

{# fun lcb_respget_status as ^ {`LcbRespGetPtr'} -> `LcbStatus' #}
{# fun lcb_respget_key    as ^ {`LcbRespGetPtr', alloca- `String' peekString*, alloca- `Int' peekCULong*} -> `LcbStatus' #}
{# fun lcb_respget_value  as ^ {`LcbRespGetPtr', alloca- `String' peekString*, alloca- `Int' peekCULong*} -> `LcbStatus' #}
{# fun lcb_respget_cas    as ^ {`LcbRespGetPtr', alloca- `CULong' peekCAS*} -> `LcbStatus' #}
{# fun lcb_respget_flags  as ^ {`LcbRespGetPtr', alloca- `Int' peekCUInt*} -> `LcbStatus' #}

lcbRespGetGetValue :: LcbRespGetPtr -> IO B.ByteString
lcbRespGetGetValue pv = do
  (s, bytes, nbytes) <- lcbRespgetValue pv
-- putStrLn  ("lcbRespGetGetValue: " ++ show s)
--  putStrLn  ("lcbRespGetGetValue: " ++ show bytes)
--  putStrLn  ("lcbRespGetGetValue: " ++ show nbytes)
--  B.packCStringLen (bytes, fromIntegral nbytes)
  return $ B.pack (encode bytes)


lcbRespGetGetStatus :: LcbRespGetPtr -> IO LcbStatus
lcbRespGetGetStatus = do
  lcbRespgetStatus  


lcbRespGetGetCas :: LcbRespGetPtr -> IO (LcbStatus, LcbCas)
lcbRespGetGetCas = do
  lcbRespgetCas


type LcbGetCallback =
  LcbRespGetPtr -> IO ()


lcbInstallGetCallback :: Lcb -> LcbGetCallback -> IO OldRespCallbackPtr
lcbInstallGetCallback lcb callback =
  lcbInstallCallback lcb LcbCallbackGet $ \ _ ptr -> callback ptr

type LcbCookie = Ptr ()

{# pointer *lcb_CMDGET as LcbCmdGet foreign finalizer lcb_cmdget_destroy newtype #}

newLcbCmdGet :: Ptr LcbCmdGet -> IO LcbCmdGet
newLcbCmdGet ptr =
  LcbCmdGet <$> newForeignPtr lcb_cmdget_destroy ptr

peekLcbCmdGetPtr :: Ptr (Ptr LcbCmdGet) -> IO LcbCmdGet
peekLcbCmdGetPtr ptr =
  peek ptr >>= newLcbCmdGet

{# fun lcb_cmdget_create as ^ {alloca- `LcbCmdGet' peekLcbCmdGetPtr*} -> `LcbStatus' #}
{# fun lcb_cmdget_key as ^ {`LcbCmdGet', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_get as lcbGetRaw {`Lcb', id `LcbCookie', `LcbCmdGet'} -> `LcbStatus' #}


lcbGet :: Lcb -> Maybe LcbCookie -> B.ByteString -> IO LcbStatus
lcbGet lcb Nothing key = do
  (s,o) <- lcbCmdgetCreate
  -- putStrLn ("lcbCmdgetCreate : " ++ show s)
  s' <- lcbCmdgetKey o ks ksl
  -- putStrLn ("lcbCmdgetCreate : " ++ show s')
  lcbGetRaw lcb nullPtr o
  where ks = BU.toString key
        ksl = length ks
lcbGet lcb (Just cookie) key = do
  (s,o) <- lcbCmdgetCreate
  s' <- lcbCmdgetKey o ks ksl
  lcbGetRaw lcb cookie o
  where ks = BU.toString key
        ksl = length ks



{# enum lcb_KVBUFTYPE as LcbKvBufType {underscoreToCase} deriving (Eq, Show) #}


_lcbKreqSimple :: Ptr () -> B.ByteString -> IO a -> IO a
_lcbKreqSimple p bs callback =
  B.useAsCStringLen bs $ \(pv, len) -> do
    {# set lcb_KEYBUF.type #} p $ fromIntegral $ fromEnum LcbKvCopy
    {# set lcb_KEYBUF.contig.bytes #} p $ castPtr pv
    {# set lcb_KEYBUF.contig.nbytes #} p $ fromIntegral len
    callback


_lcbCmdStoreSetKey :: Ptr () -> B.ByteString -> IO a -> IO a
_lcbCmdStoreSetKey p bs callback =
  _lcbKreqSimple (plusPtr p {# offsetof lcb_CMDSTORE.key #}) bs callback


_lcbCmdStoreSetValue :: Ptr () -> B.ByteString -> IO a -> IO a
_lcbCmdStoreSetValue p bs callback =
  B.useAsCStringLen bs $ \(pv, len) -> do
    {# set lcb_CMDSTORE.value.vtype #} p $ fromIntegral $ fromEnum LcbKvCopy
    {# set lcb_CMDSTORE.value.u_buf.contig.bytes #} p $ castPtr pv
    {# set lcb_CMDSTORE.value.u_buf.contig.nbytes #} p $ fromIntegral len
    callback

{# pointer *lcb_RESPSTORE as LcbRespStorePtr #}
{# fun lcb_respstore_status as ^ {`LcbRespStorePtr'} -> `LcbStatus' #}
{# fun lcb_respstore_key    as ^ {`LcbRespStorePtr', alloca- `String' peekString*, alloca- `Int' peekCULong*} -> `LcbStatus' #}
{# fun lcb_respstore_cas    as ^ {`LcbRespStorePtr', alloca- `CULong' peekIntegral*} -> `LcbStatus' #}

{# pointer *lcb_CMDSTORE as LcbCmdStore foreign finalizer lcb_cmdstore_destroy newtype #}
{# enum lcb_STORE_OPERATION as LcbStorage {underscoreToCase} deriving (Eq, Show) #}

{-
data LcbCmdStore =
  LcbCmdStore
  { lcs_operation :: LcbStorage
  , lcs_key :: B.ByteString
  , lcs_value :: B.ByteString
  , lcs_cas :: Maybe LcbCas
  } deriving (Show)
-}

newLcbCmdStoreGet :: Ptr LcbCmdStore -> IO LcbCmdStore
newLcbCmdStoreGet ptr =
  LcbCmdStore <$> newForeignPtr lcb_cmdstore_destroy ptr

peekLcbCmdStorePtr :: Ptr (Ptr LcbCmdStore) -> IO LcbCmdStore
peekLcbCmdStorePtr ptr =
  peek ptr >>= newLcbCmdStoreGet

{# fun lcb_cmdstore_create as ^ {alloca- `LcbCmdStore' peekLcbCmdStorePtr*, `LcbStorage'} -> `LcbStatus' #}
{# fun lcb_cmdstore_cas as ^ {`LcbCmdStore', `CULong'} -> `LcbStatus' #}
{# fun lcb_cmdstore_key as ^ {`LcbCmdStore', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdstore_value as ^ {`LcbCmdStore', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_store as lcbStoreRaw {`Lcb', id `LcbCookie', `LcbCmdStore'} -> `LcbStatus' #}

-- |Convert a C enumeration to Haskell.
--
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

lcbStore :: Lcb -> Maybe LcbCookie -> LcbStorage -> Maybe CULong -> B.ByteString -> B.ByteString -> IO LcbStatus
lcbStore lcb mbc op mbcas key value = do
  (s,o) <- lcbCmdstoreCreate  op
  s''   <- lcbCmdstoreKey   o ks klen
  s'''  <- lcbCmdstoreValue o vs vlen
  -- putStrLn ("lcbCmdstoreCreate : " ++ show s'')
  case mbcas of
    Nothing  -> return LcbSuccess
    (Just cas) -> lcbCmdstoreCas o cas
  case mbc of
    Nothing  -> lcbStoreRaw lcb nullPtr o
    (Just cookie) -> lcbStoreRaw lcb cookie o
  where ks = BU.toString key 
        klen = length ks
        vs = BU.toString value
        vlen = length vs



lcbRespStoreGetOp :: LcbRespStorePtr -> IO LcbStorage
lcbRespStoreGetOp p = (toEnum . fromIntegral) <$> {# get lcb_RESPSTORE.op #} p


_lcbRespBaseGetCookie :: Ptr () -> IO LcbCookie
_lcbRespBaseGetCookie p = {# get lcb_RESPBASE.cookie #} p


lcbRespStoreGetCookie :: LcbRespStorePtr -> IO LcbCookie
lcbRespStoreGetCookie = _lcbRespBaseGetCookie . castPtr


_lcbRespBaseGetKey :: Ptr () -> IO B.ByteString
_lcbRespBaseGetKey p = do
  pv <- {# get lcb_RESPGET.ctx.key #} p
  len <- {# get lcb_RESPGET.ctx.key_len #} p
  B.packCStringLen (castPtr pv, fromIntegral len)


lcbRespStoreGetKey :: LcbRespStorePtr -> IO B.ByteString
lcbRespStoreGetKey = _lcbRespBaseGetKey


lcbRespStoreGetStatus :: LcbRespStorePtr -> IO LcbStatus
lcbRespStoreGetStatus = do
   lcbRespstoreStatus 
    

type LcbCas = CULong



lcbRespStoreGetCas :: LcbRespStorePtr -> IO (LcbStatus, LcbCas)
lcbRespStoreGetCas = do
  lcbRespstoreCas


type LcbStoreCallback =
  LcbRespStorePtr -> IO ()


lcbInstallStoreCallback :: Lcb -> LcbStoreCallback -> IO OldRespCallbackPtr
lcbInstallStoreCallback lcb callback =
  lcbInstallCallback lcb LcbCallbackStore $ \ _ p -> callback p


_lcbCmdRemoveSetKey :: Ptr () -> B.ByteString -> IO a -> IO a
_lcbCmdRemoveSetKey p bs callback =
  _lcbKreqSimple (plusPtr p {# offsetof lcb_CMDREMOVE.key #}) bs callback




lcbRespRemoveGetStatus :: LcbRespRemovePtr -> IO LcbStatus
lcbRespRemoveGetStatus = do
  lcbRespremoveStatus


type LcbRemoveCallback =
  LcbRespRemovePtr -> IO ()


lcbInstallRemoveCallback :: Lcb -> LcbRemoveCallback -> IO OldRespCallbackPtr
lcbInstallRemoveCallback lcb callback =
  lcbInstallCallback lcb LcbCallbackRemove $ \ _ p -> callback p

{# pointer *lcb_RESPREMOVE as LcbRespRemovePtr #}
{# fun lcb_respremove_status as ^ {`LcbRespRemovePtr'} -> `LcbStatus' #}
{# fun lcb_respremove_key    as ^ {`LcbRespRemovePtr', alloca- `String' peekString*, alloca- `Int' peekCULong*} -> `LcbStatus' #}
{# fun lcb_respremove_cas    as ^ {`LcbRespRemovePtr', alloca- `Int' peekIntegral*} -> `LcbStatus' #}

{# pointer *lcb_CMDREMOVE as LcbCmdRemove foreign finalizer lcb_cmdremove_destroy newtype #}
{# fun lcb_cmdremove_create as ^ {alloca- `LcbCmdRemove' peekLcbCmdRemovePtr*} -> `LcbStatus' #}
{# fun lcb_cmdremove_cas as ^ {`LcbCmdRemove', `CULong'} -> `LcbStatus' #}
{# fun lcb_cmdremove_key as ^ {`LcbCmdRemove', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_remove as lcbRemoveRaw {`Lcb', id `LcbCookie', `LcbCmdRemove'} -> `LcbStatus' #}

newLcbCmdRemove :: Ptr LcbCmdRemove -> IO LcbCmdRemove
newLcbCmdRemove ptr =
  LcbCmdRemove <$> newForeignPtr lcb_cmdremove_destroy ptr

peekLcbCmdRemovePtr :: Ptr (Ptr LcbCmdRemove) -> IO LcbCmdRemove
peekLcbCmdRemovePtr ptr =
  peek ptr >>= newLcbCmdRemove



lcbRemove :: Lcb -> Maybe LcbCookie -> B.ByteString -> IO LcbStatus
lcbRemove lcb Nothing key = do
  (s,o) <- lcbCmdremoveCreate
  -- putStrLn ("lcbCmdremoveCreate : " ++ show s)
  s' <- lcbCmdremoveKey o ks ksl
  -- putStrLn ("lcbCmdremoveCreate : " ++ show s')
  lcbRemoveRaw lcb nullPtr o
  where ks  = BU.toString key 
        ksl = length ks
lcbRemove lcb (Just cookie) key = do
  (s,o) <- lcbCmdremoveCreate
  s' <- lcbCmdremoveKey o ks ksl
  lcbRemoveRaw lcb cookie o
  where ks  = BU.toString key 
        ksl = length ks


-- {# fun lcb_wait as ^ {`Lcb'} -> `LcbStatus' #}


{# enum lcb_WAITFLAGS as LcbWaitFlags {underscoreToCase} deriving (Eq, Show) #}

{# fun lcb_wait as ^ {`Lcb', `LcbWaitFlags'} -> `LcbStatus' #}

{-
 N1QL Queries
-}

lcbRespQueryGetStatus :: LcbRespQueryPtr -> IO LcbStatus
lcbRespQueryGetStatus = do
  lcbRespqueryStatus


type LcbQueryCallback =
  LcbRespQueryPtr -> IO ()



-- lcbInstallQueryCallback :: Lcb -> LcbQueryCallback -> IO OldRespCallbackPtr
-- lcbInstallQueryCallback lcb callback =
--   lcbInstallCallback lcb LcbCallbackQuery $ \ _ p -> callback p

{# pointer *lcb_RESPQUERY as LcbRespQueryPtr #}
{# pointer *lcb_CMDQUERY as LcbCmdQuery foreign finalizer lcb_cmdquery_destroy newtype #}
{# pointer *lcb_QUERY_HANDLE as LcbQueryHandle #}

{# fun lcb_respquery_status as ^ {`LcbRespQueryPtr'} -> `LcbStatus' #}
-- IntPtr
-- {# fun lcb_respquery_cookie as ^ {`LcbRespQueryPtr', alloca- `Int' peekCUInt*} -> `LcbStatus' #}
{# fun lcb_respquery_row    as ^ {`LcbRespQueryPtr', alloca- `String' peekString*, alloca- `Int' peekCULong*} -> `LcbStatus' #}
--
-- {# fun lcb_respquery_row    as ^ {`LcbRespQueryPtr', alloca- `String'& peekCStringLen*} -> `LcbStatus' #}
{# fun lcb_respquery_is_final    as ^ {`LcbRespQueryPtr'} -> `Int' #}

{# fun lcb_cmdquery_create  as ^ {alloca- `LcbCmdQuery' peekLcbCmdQueryPtr*} -> `LcbStatus' #}
{# fun lcb_cmdquery_reset   as ^ {`LcbCmdQuery'} -> `LcbStatus' #}
{# fun lcb_cmdquery_payload as ^ {`LcbCmdQuery', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_statement as ^ {`LcbCmdQuery', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_callback as ^ {`LcbCmdQuery', withLcbCallback* `LcbRespCallback'} -> `LcbStatus' #}
{# fun lcb_cmdquery_named_param      as ^ {`LcbCmdQuery', `String', `Int', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_positional_param as ^ {`LcbCmdQuery', `String', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_adhoc   as ^ {`LcbCmdQuery', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_pretty  as ^ {`LcbCmdQuery', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdquery_timeout as ^ {`LcbCmdQuery', `Int'} -> `LcbStatus' #}
{# fun lcb_query            as lcbQueryRaw {`Lcb', id `LcbCookie', `LcbCmdQuery'} -> `LcbStatus' #}
{# fun lcb_query_cancel     as ^ {`Lcb', `LcbQueryHandle'} -> `LcbStatus' #}

newLcbCmdQuery :: Ptr LcbCmdQuery -> IO LcbCmdQuery
newLcbCmdQuery ptr =
  LcbCmdQuery <$> newForeignPtr lcb_cmdquery_destroy ptr

peekLcbCmdQueryPtr :: Ptr (Ptr LcbCmdQuery) -> IO LcbCmdQuery
peekLcbCmdQueryPtr ptr =
  peek ptr >>= newLcbCmdQuery

lcbRespqueryGetRow :: LcbRespQueryPtr -> IO (String)
lcbRespqueryGetRow resp = do
  (s,rs,rlen) <- lcbRespqueryRow resp
  return (take (fromIntegral rlen) rs)

lcbQuery :: Lcb -> Maybe LcbCookie -> B.ByteString -> LcbQueryCallback -> IO LcbStatus
lcbQuery lcb Nothing query callback = do
  (s,o) <- lcbCmdqueryCreate
--  putStrLn ("lcbCmdqueryCreate : " ++ show s)
  s' <- lcbCmdqueryStatement o qs qsl
--  s'' <- lcb_cmdquery_positional_param(cmd, param, strlen(param))
--  s''' <- lcb_cmdquery_option(cmd, "pretty", strlen("pretty"), "false", strlen("false"))
  s'''' <- lcbCmdqueryCallback o $ \ _ p -> callback p 
--  putStrLn ("lcbCmdqueryCreate : " ++ show s')
  lcbQueryRaw lcb nullPtr o
  where qs  = BU.toString query 
        qsl = length qs
lcbQuery lcb (Just cookie) query callback = do
  (s,o) <- lcbCmdqueryCreate
  s' <- lcbCmdqueryStatement o qs qsl
  s'''' <- lcbCmdqueryCallback o $ \ _ p -> callback p 
  lcbQueryRaw lcb cookie o
  where qs  = BU.toString query 
        qsl = length qs


{-
 Ping
-}

{# pointer *lcb_CMDPING as LcbCmdPing foreign finalizer lcb_cmdping_destroy newtype #}

{# fun lcb_cmdping_create  as ^ {alloca- `LcbCmdPing' peekLcbCmdPingPtr*} -> `LcbStatus' #}
{# fun lcb_cmdping_all   as ^ {`LcbCmdPing'} -> `LcbStatus' #}
{# fun lcb_cmdping_kv as ^ {`LcbCmdPing', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdping_query as ^ {`LcbCmdPing', `Int'} -> `LcbStatus' #}
{# fun lcb_cmdping_timeout as ^ {`LcbCmdPing', `Int'} -> `LcbStatus' #}
{# fun lcb_ping            as lcbPingRaw {`Lcb', id `LcbCookie', `LcbCmdPing'} -> `LcbStatus' #}

newLcbCmdPing :: Ptr LcbCmdPing -> IO LcbCmdPing
newLcbCmdPing ptr =
  LcbCmdPing <$> newForeignPtr lcb_cmdping_destroy ptr

peekLcbCmdPingPtr :: Ptr (Ptr LcbCmdPing) -> IO LcbCmdPing
peekLcbCmdPingPtr ptr =
  peek ptr >>= newLcbCmdPing


lcbPing :: Lcb -> Maybe LcbCookie -> IO LcbStatus
lcbPing lcb Nothing = do
  (s,o) <- lcbCmdpingCreate
  -- a command is needed
  s' <- lcbCmdpingAll o
  lcbPingRaw lcb nullPtr o
lcbPing lcb (Just cookie) = do
  (s,o) <- lcbCmdpingCreate
--  putStrLn ("lcbCmdpingCreate : " ++ show s)
  s' <- lcbCmdpingAll o
  lcbPingRaw lcb cookie o


