{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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


instance LcbValueOf [Char] where
  valueOf = encodeUtf8 . T.pack

instance LcbValueOf Int where
  valueOf = valueOf . show

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
        withConnection $ \connection -> do
          let op =
                LcbStore LcbUpsert (emptyStoreOpts { cas = Nothing }) "test" "{ \"value\" : \"test\", \"type\" : \"test4\" }"
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op =
                LcbStore LcbUpsert (emptyStoreOpts { cas = Just 1 }) "test" "{ \"value\" : \"test\", \"type\" : \"test4\" }"
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbErrInvalidArgument

  describe "insert" $ do

    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let
            op = LcbStore LcbInsert
                          (emptyStoreOpts { cas = Nothing })
                          randomKey
                          "{ \"value\" : \"test\", \"type\" : \"test2\" }"
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess

    context "when adding twice value with same key" $ do
      it "should fail" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let
            op = LcbStore LcbInsert
                          (emptyStoreOpts { cas = Nothing })
                          randomKey
                          "{ \"value\" : \"test\", \"type\" : \"test3\" }"
            op2 = LcbStore LcbInsert (emptyStoreOpts { cas = Nothing }) randomKey 1
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess
          lcbStore connection (op2 :: LcbStore Int) `shouldReturn` LcbErrDocumentExists

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op =
                LcbStore LcbInsert (emptyStoreOpts { cas = Just 1 }) "test" "{ \"value\" : \"test\", \"type\" : \"test4\" }"
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbErrInvalidArgument

  describe "get" $ do

    context "when an existing key is provided" $ do
      it "should retrun the element" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let value = "{ \"value\" : \"test\", \"type\" : \"test2\" }"
              op    = LcbStore LcbInsert (emptyStoreOpts { cas = Nothing }) randomKey value
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess
          lcbGet connection randomKey `shouldReturn` (Right $ valueOf value)

  describe "expire" $ do

    let value = "{ \"value\" : \"test\", \"type\" : \"test2\" }"

    context "when a document with expire time is create" $ do
      it "should not be found if the time exceed the exptime" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let op = LcbStore LcbInsert (emptyStoreOpts { cas = Nothing, exptime = Just 5 }) randomKey value
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess
          threadDelay 6000000
          lcbGet connection randomKey `shouldReturn` (Left LcbErrDocumentNotFound)

      it "should be found if the time does not exceed the exptime" $ do
        withConnection $ \connection -> do
          randomKey <- randomWord randomASCII 20
          let op = LcbStore LcbInsert (emptyStoreOpts { cas = Nothing, exptime = Just 5 }) randomKey value
          lcbStore connection (op :: LcbStore String) `shouldReturn` LcbSuccess
          threadDelay 3000000
          lcbGet connection randomKey `shouldReturn` (Right $ valueOf value)
