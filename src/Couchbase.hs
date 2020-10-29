{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Couchbase
  ( CBConnect(..)
  , BucketType(..)
  , Lcb(..)
  , lcbConnect
  , Remove(..)
  , Touch(..)
  , Get(..)
  , Counter(..)
  , Store(..)
  , Query(..)
  , QueryOpts(..)
  , LcbInstance
  , Error(..)
  , ErrorInfo(..)
  , LcbConsistency(..)
  )
where


import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.C.String
import qualified Data.Text                     as T
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           LcbStatus
import           Debug.Trace
import           Data.Foldable
import           Data.Maybe
import           GHC.TypeLits                   ( TypeError(..)
                                                , ErrorMessage(..)
                                                )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import           GHC.Generics                   ( Generic )



data BucketType = LcbTypeBucket
                | LcbTypeCluster
  deriving (Show,Eq)

data LcbConsistency =  LcbConsistencyNone
                    | LcbConsistencyRequest
                    | LcbConsistencyStatement
  deriving (Show,Eq)



instance Enum LcbConsistency where
  succ LcbConsistencyNone = LcbConsistencyRequest
  succ LcbConsistencyRequest = LcbConsistencyStatement
  succ LcbConsistencyStatement = error "LcbConsistency.succ: LcbConsistencyStatement has no successor"

  pred LcbConsistencyNone = error "LcbConsistency.pred: LcbConsistencyNone has no predecessor"
  pred LcbConsistencyRequest = LcbConsistencyNone
  pred LcbConsistencyStatement = LcbConsistencyRequest

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from LcbConsistencyStatement

  fromEnum LcbConsistencyNone = 0
  fromEnum LcbConsistencyRequest = 2
  fromEnum LcbConsistencyStatement = 3

  toEnum 0 = LcbConsistencyNone
  toEnum 2 = LcbConsistencyRequest
  toEnum 3 = LcbConsistencyStatement
  toEnum unmatched = error ("LcbConsistency.toEnum: Cannot match " ++ show unmatched)

data QueryRow = QueryRow CInt CString (Ptr QueryRow)
  deriving (Show, Eq)

data QueryResultInfo = QueryResultInfo CInt CString

instance Storable QueryRow where
  sizeOf _ = 24
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 16
    return (QueryRow a b c)
  pokeElemOff p i (QueryRow a b c) = do
    pokeElemOff (castPtr p) i a
    pokeElemOff (castPtr p) (i+8) b
    pokeElemOff (castPtr p) (i+16) c

data QueryResult = QueryResult CInt CInt (Ptr QueryResultInfo) [(CString,CInt)]
  deriving (Show, Eq)

instance Storable QueryResult where
  sizeOf _ = 40
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 16
    d <- peekByteOff ptr 24 >>= \p -> f (fromIntegral b) (castPtr p)
    return (QueryResult a b c d)
   where
     f :: Int -> Ptr QueryRow -> IO [(CString,CInt)]
     f 0 _ = return []
     f n ptr = do
       (QueryRow size value next) <- peek ptr
       rows <- f (n-1) next
       return $ (value,size):rows
  pokeElemOff p i (QueryResult a b c d) = do
      pokeElemOff (castPtr p) i a
      pokeElemOff (castPtr p) (i+8) b
      pokeElemOff (castPtr p) (i+16) c
      (rootPtr,latest) <- foldrM addRow (nullPtr,Nothing) d
      pokeElemOff (castPtr p) (i+24) rootPtr
      pokeElemOff (castPtr p) (i+32) (fromMaybe nullPtr latest)
    where
        addRow (value,nvalue) (acc,latests) = allocaBytes (sizeOf (undefined :: QueryRow)) $
          \st -> do
              fillBytes st 0 (sizeOf (undefined :: QueryRow))
              pokeElemOff st 0 (QueryRow nvalue value acc)
              let latestElm = case latests of
                                    Nothing -> Just st
                                    _ -> latestElm
              return (st, latestElm)

instance Storable QueryResultInfo where
  sizeOf _ = 16
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    return (QueryResultInfo a b)
  pokeElemOff p i (QueryResultInfo a b) = do
      pokeElemOff (castPtr p) i a
      pokeElemOff (castPtr p) (i+8) b

data LcbResponseRaw = LcbResponseRaw CInt CULong CUInt CString CULong
  deriving (Show, Eq)

instance Storable LcbResponseRaw where
  sizeOf _ = 40
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 16
    d <- peekByteOff ptr 24
    e <- peekByteOff ptr 32
    return (LcbResponseRaw a b c d e)
  pokeElemOff p i (LcbResponseRaw a b c d e) = do
    pokeElemOff (castPtr p) i a
    pokeElemOff (castPtr p) (i+8) b
    pokeElemOff (castPtr p) (i+16) c
    pokeElemOff (castPtr p) (i+24) d
    pokeElemOff (castPtr p) (i+32) e

data LcbWaitFlags = LcbWaitDefault | LcbWaitNoCheck
  deriving (Show,Eq)



data LcbStoreOperation = LcbUpsert
                       | LcbInsert
                       | LcbReplace
                       | LcbAppend
                       | LcbPrepend
  deriving (Show,Eq)

instance Enum LcbWaitFlags where
  succ LcbWaitDefault = LcbWaitNoCheck
  succ LcbWaitNoCheck = error "LcbWaitFlags.succ: LcbWaitNoCheck has no successor"

  pred LcbWaitDefault = error "LcbWaitFlags.pred: LcbWaitDefault has no predecessor"
  pred LcbWaitNoCheck = LcbWaitDefault

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from LcbWaitNoCheck

  fromEnum LcbWaitDefault = 0
  fromEnum LcbWaitNoCheck = 1

  toEnum 0 = LcbWaitDefault
  toEnum 1 = LcbWaitNoCheck
  toEnum unmatched = error ("LcbWaitFlags.toEnum: Cannot match " ++ show unmatched)

instance Enum LcbStoreOperation where
  succ LcbUpsert = LcbInsert
  succ LcbInsert = LcbReplace
  succ LcbReplace = LcbAppend
  succ LcbAppend = LcbPrepend
  succ LcbPrepend = error "LcbStoreOperation.succ: LcbPrepend has no successor"

  pred LcbUpsert = error "LcbStoreOperation.pred: LcbUpsert has no predecessor"
  pred LcbInsert = LcbUpsert
  pred LcbReplace = LcbInsert
  pred LcbAppend = LcbReplace
  pred LcbPrepend = LcbAppend

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from LcbPrepend

  fromEnum LcbUpsert = 0
  fromEnum LcbInsert = 1
  fromEnum LcbReplace = 2
  fromEnum LcbAppend = 4
  fromEnum LcbPrepend = 5

  toEnum 0 = LcbUpsert
  toEnum 1 = LcbInsert
  toEnum 2 = LcbReplace
  toEnum 4 = LcbAppend
  toEnum 5 = LcbPrepend
  toEnum unmatched = error ("LcbStoreOperation.toEnum: Cannot match " ++ show unmatched)

instance Enum BucketType where
  succ LcbTypeBucket = LcbTypeCluster
  succ LcbTypeCluster = error "CB_BucketType.succ: LcbTypeCluster has no successor"

  pred LcbTypeCluster = LcbTypeBucket
  pred LcbTypeBucket = error "CB_BucketType.pred: LcbTypeBucket has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from LcbTypeCluster

  fromEnum LcbTypeBucket = 0
  fromEnum LcbTypeCluster = 1

  toEnum 0 = LcbTypeBucket
  toEnum 1 = LcbTypeCluster
  toEnum unmatched = error ("CB_BucketType.toEnum: Cannot match " ++ show unmatched)

newtype LcbInstance = LcbInstance (ForeignPtr LcbInstance)
  deriving (Show, Eq)

type LcbOps = Ptr ()
type LcbCommand = Ptr ()

peekLcb :: Ptr (Ptr LcbInstance) -> IO LcbInstance
peekLcb ptr = peek ptr >>= \prt -> LcbInstance <$> newForeignPtr lcb_destroy prt

c_lcbCreate :: LcbOps -> IO (CInt, LcbInstance)
c_lcbCreate a2 = alloca $ \a1' ->
  c_lcbCreate'_ a1' a2 >>= \res -> let res' = (toEnum . fromIntegral) res in peekLcb a1' >>= \a1'' -> return (res', a1'')

data CBConnect = CBConnect {
    cb_bucket_type :: BucketType,
    cb_username :: String,
    cb_password :: String,
    cb_connection :: String
}

type Expire = Maybe Int
type Cas = Int
type Key = String
type Delta = Int
type Default = Int
type Value = B.ByteString
type Stmt = String
type AdHoc = Bool
data Options = Pretty Int
             | ClientContextIndex String
  deriving (Show,Eq)



data Store = Upsert Key Value Expire
           | Insert Key Value Expire
           | Replace Key Value Expire (Maybe Cas)
           | Append Key Value Expire (Maybe Cas)
           | Prepend Key Value Expire (Maybe Cas)
  deriving (Show,Eq)

data Get = Get Key
  deriving (Show,Eq)

data Touch = Touch Key Expire
  deriving (Show,Eq)

data Remove = Remove Key (Maybe Cas)
  deriving (Show,Eq)

data Counter = Counter Key Default Delta Expire (Maybe Cas)
  deriving (Show,Eq)

data QueryOpts = QueryOpts {
  namedParams :: [(String,String)],
  posParams :: [String],
  consistency :: Maybe LcbConsistency,
  adhoc :: Maybe AdHoc,
  options :: [Options]
}
  deriving (Show,Eq)

data Query = Query QueryOpts Stmt
  deriving (Show,Eq)

data Error = Error {
  statusCode :: LcbStatus,
  message :: String,
  info :: [ErrorInfo]
}
  deriving (Show)

data ErrorInfo = ErrorInfo {
  code :: Int,
  msg :: String
}
  deriving (Show, Generic)

instance A.FromJSON ErrorInfo
instance A.ToJSON ErrorInfo

class (Storable a) => Status a where
  status :: a -> LcbStatus
  mapError :: a -> IO Error

instance Status CInt where
  status = toEnum . fromIntegral
  mapError st = do
    let errorCode = status st
    msg          <- c_lcbStrerrorLong (fromIntegral $ fromEnum errorCode)
    errorMessage <- peekCString msg
    return $ Error {message = errorMessage, statusCode = errorCode, info = []}

instance Status LcbResponseRaw where
  status (LcbResponseRaw st _ _ _ _) = status st
  mapError response = do
    let errorCode = status response
    msg          <- c_lcbStrerrorLong (fromIntegral $ fromEnum errorCode)
    errorMessage <- peekCString msg
    return $ Error {message = errorMessage, statusCode = errorCode, info = []}

instance Status QueryResult where
  status (QueryResult st _ _ _) = status st
  mapError response@(QueryResult st _ info _) = do
      let errorCode = status response
      msg          <- c_lcbStrerrorLong (fromIntegral $ fromEnum errorCode)
      errorMessage <- peekCString msg
      QueryResultInfo len value <- peek info
      queryInfo <- B.packCStringLen (value, fromEnum len)
      let parsedInfo = A.decode (BL.fromStrict queryInfo) :: Maybe A.Object
          message = maybe Nothing errorInfo parsedInfo
      return $ Error {message = errorMessage, statusCode = errorCode, info = fromMaybe [] message}
    where
      errorInfo :: AT.Object -> Maybe [ErrorInfo]
      errorInfo = AT.parseMaybe (\obj -> obj A..: "errors")

class Lcb a t | a -> t where
  lcb :: LcbInstance -> a -> IO (Either Error t)

responseInfoSize = sizeOf (undefined :: LcbResponseRaw)

lcbConnect :: CBConnect -> IO (Either Error LcbInstance)
lcbConnect params = do
  lcb <- lcbCreate params
  case lcb of
    Right (LcbInstance lcbInstance) -> withForeignPtr lcbInstance $ \prt -> do
      st <- c_lcbConnect'_ prt
      case status st of
        LcbSuccess -> do
          waitStatus <- c_lcbWait prt (fromIntegral $ fromEnum LcbWaitNoCheck)
          case status waitStatus of
            LcbSuccess -> do
              status <$> c_lcbInitWrapper prt
              return lcb
            _ -> Left <$> mapError waitStatus
        _ -> Left <$> mapError st
    Left _ -> return lcb

lcbCreate :: CBConnect -> IO (Either Error LcbInstance)
lcbCreate params = allocaBytes 96 $ \st -> do
  fillBytes st 0 96
  (\ptr val -> pokeByteOff ptr 0 (val :: CInt)) st $ fromIntegral $ fromEnum $ cb_bucket_type params
  withCAStringLen (cb_connection params) $ \(connstr, connstr_len) -> c_lcbCreateoptsConnstr st connstr (toEnum connstr_len)
  withCAStringLen (cb_password params) $ \(password, password_len) ->
    withCAStringLen (cb_username params) $ \(username, username_len) ->
      c_lcbCreateoptsCredentials st username (toEnum username_len) password (toEnum password_len)
  (createStatus, lcbInstance) <- c_lcbCreate st
  case status createStatus of
    LcbSuccess -> return $ Right lcbInstance
    code       -> Left <$> mapError createStatus

instance Lcb Store Cas where
  lcb lcbInstance store = allocaBytes 152 $ \ptrCmd -> do
      let (op, key, value, exptime, cas) = info store
      fillBytes ptrCmd 0 152
      (\ptr val -> pokeByteOff ptr 136 (val :: CInt)) ptrCmd $ fromIntegral $ fromEnum op
      withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdstoreKey ptrCmd _key (toEnum _key_len)
      B.useAsCStringLen value $ \(_value, _value_len) -> c_lcbCmdstoreValue ptrCmd _value (toEnum _value_len)
      lcbAddCas ptrCmd cas c_lcbCmdstoreCas
      lcbAddExpTime ptrCmd exptime c_lcbCmdstoreExpiry
      lcbRun lcbInstance responseInfoSize ptrCmd c_lcbStoreWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return $ fromIntegral cas
    where
      info (Upsert key value exptime     ) = (LcbUpsert, key, value, exptime, Nothing)
      info (Insert key value exptime     ) = (LcbInsert, key, value, exptime, Nothing)
      info (Replace key value exptime cas) = (LcbReplace, key, value, exptime, cas)
      info (Append  key value exptime cas) = (LcbAppend, key, value, exptime, cas)
      info (Prepend key value exptime cas) = (LcbPrepend, key, value, exptime, cas)

instance Lcb Get (Cas,B.ByteString) where
  lcb lcbInstance (Get key) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdgetKey ptrCmd _key (toEnum _key_len)
    lcbRun lcbInstance responseInfoSize ptrCmd c_lcbGetWrapper
      $ \(LcbResponseRaw _ cas length value _) -> (fromIntegral cas, ) <$> B.packCStringLen (value, fromEnum length)

instance Lcb Touch Cas where
  lcb lcbInstance (Touch key exptime) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdgetKey ptrCmd _key (toEnum _key_len)
    lcbAddExpTime ptrCmd exptime c_lcbCmdtouchExpiry
    lcbRun lcbInstance responseInfoSize ptrCmd c_lcbTouchWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return $ fromIntegral cas

instance Lcb Remove Cas where
  lcb lcbInstance (Remove key cas) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdremoveKey ptrCmd _key (toEnum _key_len)
    lcbAddCas ptrCmd cas c_lcbCmdremoveCas
    lcbRun lcbInstance responseInfoSize ptrCmd c_lcbRemoveWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return $ fromIntegral cas

instance Lcb Counter (Cas,Int) where
  lcb lcbInstance (Counter key defaultValue delta exptime cas) = allocaBytes 128 $ \ptrCmd -> do
    fillBytes ptrCmd 0 128
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdcounterKey ptrCmd _key (toEnum _key_len)
    c_lcbCmdcounterInitital ptrCmd (fromIntegral $ fromEnum defaultValue)
    c_lcbCmdcounterDelta ptrCmd (fromIntegral $ fromEnum delta)
    lcbAddExpTime ptrCmd exptime c_lcbCmdtouchExpiry
    lcbAddCas ptrCmd cas c_lcbCmdcounterCas
    lcbRun lcbInstance responseInfoSize ptrCmd c_lcbCounterWrapper
      $ \(LcbResponseRaw _ cas _ _ value) -> return (fromIntegral cas, fromIntegral value)

instance Lcb Query (Cas,[B.ByteString]) where
  lcb lcbInstance (Query opts stmt) = allocaBytes 8 $ \ptrCmd -> do
    fillBytes ptrCmd 0 8
    c_lcbCmdqueryCreate ptrCmd
    cmd <- peek ptrCmd
    withCAStringLen stmt $ \(_stmt, _stmt_len) -> c_lcbCmdqueryStatement cmd _stmt (toEnum _stmt_len)
    mapM_ (nameParameters cmd) (namedParams opts)
    mapM_ (positionalParameters cmd) (posParams opts)
    addConsistency cmd (consistency opts)
    c_lcbCmdqueryMetrics cmd 0
    response <- lcbRun lcbInstance (sizeOf (undefined :: QueryResult)) cmd c_lcbQueryWrapper
      $ \(QueryResult _ result_no _ value) -> do
        rows <- mapM (\(row,rowLength) -> B.packCStringLen (row, fromIntegral rowLength)) value
        return (fromIntegral result_no, rows)
    c_lcbCmdqueryDestroy cmd
    return response
   where

    addConsistency _ Nothing = return (fromIntegral $ fromEnum LcbSuccess)
    addConsistency cmd (Just consistency) = c_lcbCmdqueryConsistency cmd (fromIntegral $ fromEnum consistency)

    nameParameters cmd (name,value) =
      withCAStringLen name $ \(_name, _name_len) ->
        withCAStringLen value $ \(_value, _value_len) ->
          c_lcbCmdqueryNamedParam cmd _name (toEnum _name_len) _value (toEnum _value_len)

    positionalParameters cmd value =
        withCAStringLen value $ \(_value, _value_len) ->
          c_lcbCmdqueryPositionalParam cmd _value (toEnum _value_len)

lcbRun
  :: (Storable b, Status b)
  => LcbInstance
  -> Int
  -> Ptr ()
  -> (Ptr LcbInstance -> Ptr () -> Ptr b -> IO ())
  -> (b -> IO a)
  -> IO (Either Error a)
lcbRun (LcbInstance lcbInstance) size ptrCmd cmd success = allocaBytes size $ \responseInfo -> do
  fillBytes responseInfo 0 size
  withForeignPtr lcbInstance $ \prt -> cmd prt ptrCmd responseInfo
  response <- peek responseInfo
  case status response of
    LcbSuccess -> Right <$> success response
    _          -> Left <$> mapError response

lcbAddCas :: Ptr () -> Maybe Int -> (Ptr () -> CULong -> IO CInt) -> IO LcbStatus
lcbAddCas _      Nothing  _  = return LcbSuccess
lcbAddCas ptrCmd (Just x) fn = status <$> fn ptrCmd (fromIntegral $ fromEnum x)

lcbAddExpTime :: Ptr () -> Maybe Int -> (Ptr () -> CULong -> IO CInt) -> IO LcbStatus
lcbAddExpTime _      Nothing  _  = return LcbSuccess
lcbAddExpTime ptrCmd (Just x) fn = status <$> fn ptrCmd (fromIntegral $ fromEnum x)

lbcStatusToError :: LcbStatus -> IO String
lbcStatusToError status = c_lcbStrerrorLong (toEnum $ fromEnum status) >>= peekCString

foreign import ccall "couchbase.h &lcb_destroy"
  lcb_destroy :: FinalizerPtr LcbInstance

foreign import ccall safe "couchbase.h lcb_create"
  c_lcbCreate'_ :: Ptr (Ptr LcbInstance) -> Ptr () -> IO CInt

foreign import ccall safe "couchbase.h lcb_connect"
  c_lcbConnect'_ :: Ptr LcbInstance -> IO CInt

foreign import ccall safe "couchbase.h lcb_wait"
  c_lcbWait :: Ptr LcbInstance -> CInt -> IO CInt

foreign import ccall safe "couchbase.h lcb_createopts_connstr"
  c_lcbCreateoptsConnstr :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_createopts_credentials"
  c_lcbCreateoptsCredentials :: Ptr () -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdstore_key"
  c_lcbCmdstoreKey :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdget_key"
  c_lcbCmdgetKey :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdcounter_key"
  c_lcbCmdcounterKey :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdremove_key"
  c_lcbCmdremoveKey :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdstore_value"
  c_lcbCmdstoreValue :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdcounter_initial"
  c_lcbCmdcounterInitital :: Ptr () -> CULong -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdcounter_delta"
  c_lcbCmdcounterDelta :: Ptr () -> CULong -> IO ()

foreign import ccall safe "couchbase.h lcb_strerror_long"
  c_lcbStrerrorLong :: CInt -> IO (Ptr CChar)

foreign import ccall safe "couchbase.h lcb_cmdstore_cas"
  c_lcbCmdstoreCas :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdremove_cas"
  c_lcbCmdremoveCas :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdcounter_cas"
  c_lcbCmdcounterCas :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdstore_expiry"
  c_lcbCmdstoreExpiry :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdtouch_expiry"
  c_lcbCmdtouchExpiry :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdcounter_expiry"
  c_lcbCmdcounterExpiry :: Ptr () -> CULong -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_init_wrapper"
  c_lcbInitWrapper :: Ptr LcbInstance -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_store_wrapper"
  c_lcbStoreWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_get_wrapper"
  c_lcbGetWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_remove_wrapper"
  c_lcbRemoveWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_touch_wrapper"
  c_lcbTouchWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_counter_wrapper"
  c_lcbCounterWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_create"
  c_lcbCmdqueryCreate :: Ptr (Ptr ()) -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_destroy"
  c_lcbCmdqueryDestroy :: Ptr () -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_statement"
  c_lcbCmdqueryStatement :: Ptr () -> Ptr CChar -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_named_param"
  c_lcbCmdqueryNamedParam :: Ptr () -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_positional_param"
  c_lcbCmdqueryPositionalParam :: Ptr () -> Ptr CChar -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_consistency"
  c_lcbCmdqueryConsistency :: Ptr () -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_option"
  c_lcbCmdqueryOption :: Ptr () -> Ptr CChar -> CInt -> Ptr CChar -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_cmdquery_metrics"
  c_lcbCmdqueryMetrics :: Ptr () -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_query_wrapper"
  c_lcbQueryWrapper :: Ptr LcbInstance -> Ptr () -> Ptr QueryResult -> IO ()


