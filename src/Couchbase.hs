{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Couchbase
  ( CBConnect(..)
  , BucketType(..)
  , LcbValueOf(..)
  , Lcb(..)
  , lcbConnect
  , Remove(..)
  , Touch(..)
  , Get(..)
  , Counter(..)
  , Store(..)
  , LcbInstance
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
import           Data.Text.Encoding             ( encodeUtf8 )
import qualified Data.ByteString               as B
import           LcbStatus
import           Debug.Trace
import           GHC.TypeLits                   ( TypeError(..)
                                                , ErrorMessage(..)
                                                )

data BucketType = LcbTypeBucket
                   | LcbTypeCluster
  deriving (Show,Eq)



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

class LcbValueOf a where
  valueOf :: a -> B.ByteString
  toValue :: B.ByteString -> a

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

c_lcbCreate :: LcbOps -> IO (LcbStatus, LcbInstance)
c_lcbCreate a2 = alloca $ \a1' ->
  c_lcbCreate'_ a1' a2 >>= \res -> let res' = (toEnum . fromIntegral) res in peekLcb a1' >>= \a1'' -> return (res', a1'')

data CBConnect = CBConnect {
    cb_bucket_type :: BucketType,
    cb_username :: String,
    cb_password :: String,
    cb_connection :: String
}

type Expire = Maybe Int
type Cas = Maybe Int
type Key = String
type Delta = Int
type Default = Int

data Store a = Upsert Key a Expire
             | Insert Key a Expire
             | Replace Key a Expire Cas
             | Append Key a Expire Cas
             | Prepend Key a Expire Cas
  deriving (Show,Eq)


data Get = Get Key

data Touch = Touch Key Expire

data Remove = Remove Key Cas

data Counter = Counter Key Default Delta Expire Cas

class Lcb a where
  lcb :: LcbInstance -> a -> IO (Either LcbStatus (Int,(Maybe B.ByteString)))


responseInfoSize = sizeOf (undefined :: LcbResponseRaw)

lcbToStatus = toEnum . fromIntegral

lcbConnect :: CBConnect -> IO (Either LcbStatus LcbInstance)
lcbConnect params = do
  lcb <- lcbCreate params
  case lcb of
    Right (LcbInstance lcbInstance) -> withForeignPtr lcbInstance $ \prt -> do
      status <- (toEnum . fromIntegral) <$> c_lcbConnect'_ prt
      case status of
        LcbSuccess -> do
          waitStatus <- c_lcbWait prt (fromIntegral $ fromEnum LcbWaitNoCheck)
          case lcbToStatus waitStatus of
            LcbSuccess -> do
              lcbToStatus <$> c_lcbInitWrapper prt
              return lcb
            any -> return $ Left any
        any -> return $ Left any
    Left _ -> return lcb

lcbCreate :: CBConnect -> IO (Either LcbStatus LcbInstance)
lcbCreate params = allocaBytes 96 $ \st -> do
  fillBytes st 0 96
  (\ptr val -> pokeByteOff ptr 0 (val :: CInt)) st $ fromIntegral $ fromEnum $ cb_bucket_type params
  withCAStringLen (cb_connection params) $ \(connstr, connstr_len) -> c_lcbCreateoptsConnstr st connstr (toEnum connstr_len)
  withCAStringLen (cb_password params) $ \(password, password_len) ->
    withCAStringLen (cb_username params) $ \(username, username_len) ->
      c_lcbCreateoptsCredentials st username (toEnum username_len) password (toEnum password_len)
  (status, lcbInstance) <- c_lcbCreate st
  return $ case status of
    LcbSuccess -> Right lcbInstance
    any        -> Left any

instance (LcbValueOf a) => Lcb (Store a) where
  lcb lcbInstance store = allocaBytes 152 $ \ptrCmd -> do
      let (op, key, value, exptime, cas) = info store
      fillBytes ptrCmd 0 152
      (\ptr val -> pokeByteOff ptr 136 (val :: CInt)) ptrCmd $ fromIntegral $ fromEnum $ op
      withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdstoreKey ptrCmd _key (toEnum _key_len)
      B.useAsCStringLen (valueOf value) $ \(_value, _value_len) -> c_lcbCmdstoreValue ptrCmd _value (toEnum _value_len)
      casResponse <- lcbAddCas ptrCmd cas c_lcbCmdstoreCas
      lcbAddExpTime ptrCmd exptime c_lcbCmdstoreExpiry
      case casResponse of
        LcbSuccess ->
          lcbRun lcbInstance ptrCmd c_lcbStoreWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return (fromIntegral cas, Nothing)
        any -> return $ Left any
    where
      info (Upsert key a exptime     ) = (LcbUpsert, key, a, exptime, Nothing)
      info (Insert key a exptime     ) = (LcbInsert, key, a, exptime, Nothing)
      info (Replace key a exptime cas) = (LcbReplace, key, a, exptime, cas)
      info (Append  key a exptime cas) = (LcbAppend, key, a, exptime, cas)
      info (Prepend key a exptime cas) = (LcbPrepend, key, a, exptime, cas)

instance Lcb Get where
  lcb lcbInstance (Get key) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdgetKey ptrCmd _key (toEnum _key_len)
    lcbRun lcbInstance ptrCmd c_lcbGetWrapper
      $ \(LcbResponseRaw _ cas length value _) -> (fromIntegral cas, ) . Just <$> B.packCStringLen (value, fromEnum length)

instance Lcb Touch where
  lcb lcbInstance (Touch key exptime) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdgetKey ptrCmd _key (toEnum _key_len)
    lcbAddExpTime ptrCmd exptime c_lcbCmdtouchExpiry
    lcbRun lcbInstance ptrCmd c_lcbTouchWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return (fromIntegral cas, Nothing)

instance Lcb Remove where
  lcb lcbInstance (Remove key cas) = allocaBytes 112 $ \ptrCmd -> do
    fillBytes ptrCmd 0 112
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdremoveKey ptrCmd _key (toEnum _key_len)
    lcbAddCas ptrCmd cas c_lcbCmdremoveCas
    lcbRun lcbInstance ptrCmd c_lcbRemoveWrapper $ \(LcbResponseRaw _ cas _ _ _) -> return (fromIntegral cas, Nothing)

instance Lcb Counter where
  lcb lcbInstance (Counter key defaultValue delta exptime cas) = allocaBytes 128 $ \ptrCmd -> do
    fillBytes ptrCmd 0 128
    withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdcounterKey ptrCmd _key (toEnum _key_len)
    c_lcbCmdcounterInitital ptrCmd (fromIntegral $ fromEnum defaultValue)
    c_lcbCmdcounterDelta ptrCmd (fromIntegral $ fromEnum delta)
    lcbAddExpTime ptrCmd exptime c_lcbCmdtouchExpiry
    lcbAddCas ptrCmd cas c_lcbCmdcounterCas
    lcbRun lcbInstance ptrCmd c_lcbCounterWrapper
      $ \(LcbResponseRaw _ cas _ _ value) -> return (fromIntegral cas, Just $ encodeUtf8 $ T.pack $ show value)

lcbRun
  :: LcbInstance
  -> Ptr ()
  -> (Ptr LcbInstance -> Ptr () -> Ptr LcbResponseRaw -> IO ())
  -> (LcbResponseRaw -> IO a)
  -> IO (Either LcbStatus a)
lcbRun (LcbInstance lcbInstance) ptrCmd cmd success = allocaBytes responseInfoSize $ \responseInfo -> do
  fillBytes responseInfo 0 responseInfoSize
  withForeignPtr lcbInstance $ \prt -> cmd prt ptrCmd responseInfo
  response@(LcbResponseRaw status _ _ _ _) <- peek responseInfo
  case lcbToStatus status of
    LcbSuccess -> Right <$> success response
    any        -> return $ Left any

lcbAddCas :: Ptr () -> Maybe Int -> (Ptr () -> CULong -> IO CInt) -> IO LcbStatus
lcbAddCas _      Nothing  _  = return LcbSuccess
lcbAddCas ptrCmd (Just x) fn = lcbToStatus <$> fn ptrCmd (fromIntegral $ fromEnum x)

lcbAddExpTime :: Ptr () -> Maybe Int -> (Ptr () -> CULong -> IO CInt) -> IO LcbStatus
lcbAddExpTime _      Nothing  _  = return LcbSuccess
lcbAddExpTime ptrCmd (Just x) fn = lcbToStatus <$> fn ptrCmd (fromIntegral $ fromEnum x)

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
