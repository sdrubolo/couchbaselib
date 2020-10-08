{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies#-}


module Couchbase
  ( CBConnect(..)
  , CB_BucketType(..)
  , LcbStoreOperation(..)
  , LcbStore(..)
  , StoreOpts(..)
  , LcbValueOf(..)
  , lcbCreate
  , lcbConnect
  , lcbStore
  , lcbGet
  )
where


import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.C.String

import qualified Data.ByteString               as B
import           LcbStatus
import           Debug.Trace
import           GHC.TypeLits                   ( TypeError(..)
                                                , ErrorMessage(..)
                                                )



data CB_BucketType = LcbTypeBucket
                   | LcbTypeCluster
  deriving (Show,Eq)

data LcbResponse = LcbResponse CInt CUInt CUInt CString
  deriving (Show, Eq)

instance Storable LcbResponse where
  sizeOf _ = 32
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 8
    c <- peekByteOff ptr 16
    d <- peekByteOff ptr 24
    return (LcbResponse a b c d)
  pokeElemOff p i (LcbResponse a b c d) = do
    pokeElemOff (castPtr p) i a
    pokeElemOff (castPtr p) (i+8) b
    pokeElemOff (castPtr p) (i+16) c
    pokeElemOff (castPtr p) (i+24) d

data LcbWaitFlags = LcbWaitDefault | LcbWaitNoCheck
  deriving (Show,Eq)

class LcbValueOf a where
  valueOf :: a -> B.ByteString

data LcbStoreOperation = LcbUpsert
                       | LcbInsert
                       | LcbReplace
                       | LcbAppend
                       | LcbPrepend
  deriving (Show,Eq)

type Key = String

data StoreOpts = StoreOpts {
  cas :: Maybe Int,
  exptime :: Maybe Int
}

data LcbStore a = LcbStore LcbStoreOperation StoreOpts Key a


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

instance Enum CB_BucketType where
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
type LcbOps = Ptr ()
type LcbCommand = Ptr ()

peekLcb :: Ptr (Ptr LcbInstance) -> IO LcbInstance
peekLcb ptr = peek ptr >>= \prt -> LcbInstance <$> newForeignPtr lcb_destroy prt

c_lcbCreate :: LcbOps -> IO (LcbStatus, LcbInstance)
c_lcbCreate a2 = alloca $ \a1' ->
  c_lcbCreate'_ a1' a2 >>= \res -> let res' = (toEnum . fromIntegral) res in peekLcb a1' >>= \a1'' -> return (res', a1'')

data CBConnect = CBConnect {
    cb_bucket_type :: CB_BucketType,
    cb_username :: String,
    cb_password :: String,
    cb_connection :: String
}

lcbToStatus = toEnum . fromIntegral

lcbCreate :: CBConnect -> IO (LcbStatus, LcbInstance)
lcbCreate params = allocaBytes 96 $ \st -> do
  fillBytes st 0 96
  (\ptr val -> pokeByteOff ptr 0 (val :: CInt)) st $ fromIntegral $ fromEnum $ cb_bucket_type params
  withCAStringLen (cb_connection params) $ \(connstr, connstr_len) -> c_lcbCreateoptsConnstr st connstr (toEnum connstr_len)
  withCAStringLen (cb_password params) $ \(password, password_len) ->
    withCAStringLen (cb_username params) $ \(username, username_len) ->
      c_lcbCreateoptsCredentials st username (toEnum username_len) password (toEnum password_len)
  c_lcbCreate st

lcbConnect :: LcbInstance -> IO LcbStatus
lcbConnect (LcbInstance lcbInstance) = withForeignPtr lcbInstance $ \prt -> do
  status <- (toEnum . fromIntegral) <$> c_lcbConnect'_ prt
  case status of
    LcbSuccess -> do
      waitStatus <- c_lcbWait prt (fromIntegral $ fromEnum LcbWaitNoCheck)
      case lcbToStatus waitStatus of
        LcbSuccess -> lcbToStatus <$> c_lcbInitWrapper prt
        any        -> return any
    _ -> return status

lcbAddCas :: Ptr () -> Maybe Int -> IO LcbStatus
lcbAddCas _      Nothing  = return LcbSuccess
lcbAddCas ptrCmd (Just x) = lcbToStatus <$> c_lcbCmdstoreCas ptrCmd (fromIntegral $ fromEnum x)

lcbAddExpTime :: Ptr () -> Maybe Int -> IO LcbStatus
lcbAddExpTime _      Nothing  = return LcbSuccess
lcbAddExpTime ptrCmd (Just x) = lcbToStatus <$> c_lcbCmdstoreExpiry ptrCmd (fromIntegral $ fromEnum x)

lcbStore :: (LcbValueOf a) => LcbInstance -> LcbStore a -> IO LcbStatus
lcbStore (LcbInstance lcbInstance) (LcbStore op opts key value) = allocaBytes 152 $ \ptrCmd -> do
  fillBytes ptrCmd 0 152
  (\ptr val -> pokeByteOff ptr 136 (val :: CInt)) ptrCmd $ fromIntegral $ fromEnum $ op
  withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdstoreKey ptrCmd _key (toEnum _key_len)
  B.useAsCStringLen (valueOf value) $ \(_value, _value_len) -> c_lcbCmdstoreValue ptrCmd _value (toEnum _value_len)
  casResponse <- lcbAddCas ptrCmd (cas opts)
  lcbAddExpTime ptrCmd (exptime opts)
  case casResponse of
    LcbSuccess -> allocaBytes 24 $ \responseInfo -> do
      fillBytes responseInfo 0 24
      withForeignPtr lcbInstance $ \prt -> c_lcbStoreWrapper prt ptrCmd responseInfo
      LcbResponse status _ _ _ <- peek responseInfo
      return $ lcbToStatus status
    any -> return any

lcbGet :: LcbInstance -> String -> IO (Either LcbStatus B.ByteString)
lcbGet (LcbInstance lcbInstance) key = allocaBytes 112 $ \ptrCmd -> do
  fillBytes ptrCmd 0 112
  withCAStringLen key $ \(_key, _key_len) -> c_lcbCmdgetKey ptrCmd _key (toEnum _key_len)
  allocaBytes 24 $ \responseInfo -> do
    fillBytes responseInfo 0 24
    withForeignPtr lcbInstance $ \prt -> c_lcbGetWrapper prt ptrCmd responseInfo
    LcbResponse status _ length value <- peek responseInfo
    case (lcbToStatus status) of
      LcbSuccess -> Right <$> B.packCStringLen (value, fromEnum length)
      any        -> return $ Left any


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

foreign import ccall safe "couchbase.h lcb_cmdstore_value"
  c_lcbCmdstoreValue :: Ptr () -> Ptr CChar -> CInt -> IO ()

foreign import ccall safe "couchbase.h lcb_cmdstore_cas"
  c_lcbCmdstoreCas :: Ptr () -> CLong -> IO CInt

foreign import ccall safe "couchbase.h lcb_cmdstore_expiry"
  c_lcbCmdstoreExpiry :: Ptr () -> CInt -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_init_wrapper"
  c_lcbInitWrapper :: Ptr LcbInstance -> IO CInt

foreign import ccall safe "couchbaseWrapper.h lcb_store_wrapper"
  c_lcbStoreWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponse -> IO ()

foreign import ccall safe "couchbaseWrapper.h lcb_get_wrapper"
  c_lcbGetWrapper :: Ptr LcbInstance -> Ptr () -> Ptr LcbResponse -> IO ()




