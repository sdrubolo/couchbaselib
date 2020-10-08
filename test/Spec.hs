{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Redundant do" #-}
module Main where


import           Control.Monad
import           Couchbase
import           LcbStatus
import qualified Data.ByteString               as B
import           Foreign.Ptr
import           Test.Hspec
import           Test.RandomStrings
import           Debug.Trace
import           Control.Concurrent
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )

toString :: String -> B.ByteString
toString = encodeUtf8 . T.pack



defaultParams :: CBConnect
defaultParams = cbConnect "gianluca" "qwerty12345" "couchbase://localhost/test"

cbConnect username password connection =
  CBConnect {cb_bucket_type = LcbTypeBucket, cb_username = username, cb_password = password, cb_connection = connection}

withConnection fn = do
  (err, lcb) <- lcbCreate defaultParams
  err `shouldBe` LcbSuccess
  lcbConnect lcb `shouldReturn` LcbSuccess
  fn lcb

emptyStoreOpts = StoreOpts {cas = Nothing, exptime = Nothing}

main :: IO ()
main = hspec $ do
  describe "connect" $ do

    context "when valid credentials are given" $ do
      it "should connect" $ do
        (err, lcb) <- lcbCreate defaultParams
        err `shouldBe` LcbSuccess
        lcbConnect lcb `shouldReturn` LcbSuccess

    context "when invalid credentials are given" $ do
      it "should not connect" $ do
        (err, lcb) <- lcbCreate $ cbConnect "test" "qwerty12345" "couchbase://localhost/test"
        err `shouldBe` LcbSuccess
        lcbConnect lcb `shouldReturn` LcbErrAuthenticationFailure

    context "when invalid bucket name is given" $ do
      it "should not connect" $ do
        (err, lcb) <- lcbCreate $ cbConnect "gianluca" "qwerty12345" "couchbase://localhost/not_found"
        err `shouldBe` LcbSuccess
        lcbConnect lcb `shouldReturn` LcbErrGeneric

  describe "upsert" $ do
    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection ->
          lcbStore connection
                   (LcbStore LcbUpsert (emptyStoreOpts { cas = Nothing }) "test" "{ 'value' : 'test', 'type' : 'test4' }")
            `shouldReturn` LcbSuccess

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection ->
          lcbStore connection
                   (LcbStore LcbUpsert (emptyStoreOpts { cas = Just 1 }) "test" "{ 'value' : 'test', 'type' : 'test4' }")
            `shouldReturn` LcbErrInvalidArgument

  describe "insert" $ do

    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          lcbStore
              connection
              (LcbStore LcbInsert (emptyStoreOpts { cas = Nothing }) randomKey "{ 'value' : 'test', 'type' : 'test2' }")
            `shouldReturn` LcbSuccess

    context "when adding twice value with same key" $ do
      it "should fail" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let op = LcbStore LcbInsert (emptyStoreOpts { cas = Nothing }) randomKey "{ 'value' : 'test', 'type' : 'test3' }"
          lcbStore connection op `shouldReturn` LcbSuccess
          --threadDelay 6000000
          lcbStore connection op `shouldReturn` LcbErrDocumentExists

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection ->
          lcbStore connection
                   (LcbStore LcbInsert (emptyStoreOpts { cas = Just 1 }) "test" "{ 'value' : 'test', 'type' : 'test4' }")
            `shouldReturn` LcbErrInvalidArgument

  describe "get" $ do

    context "when an existing key is provided" $ do
      it "should retrun the element" $ do
        withConnection $ \connection -> do
          let value = "{ 'value' : 'test', 'type' : 'test2' }"
          randomKey <- randomWord randomASCII 20
          lcbStore connection (LcbStore LcbInsert (emptyStoreOpts { cas = Nothing }) randomKey value)
            `shouldReturn` LcbSuccess
          lcbGet connection randomKey `shouldReturn` (Right $ toString value)
