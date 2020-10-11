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
  Right lcb <- lcbCreate defaultParams
  lcbConnect lcb `shouldReturn` (Right lcb)
  fn lcb

withKey fn = do
  randomKey <- randomWord randomASCII 20
  fn randomKey

emptyStoreOpts = StoreOpts {cas = Nothing, exptime = Nothing}

document = "{ \"value\" : \"test\", \"type\" : \"test4\" }"

main :: IO ()
main = hspec $ do

  describe "Connect" $ do

    context "when valid credentials are given" $ do
      it "should connect" $ do
        Right lcb <- lcbCreate defaultParams
        lcbConnect lcb `shouldReturn` (Right lcb)

    context "when invalid credentials are given" $ do
      it "should not connect" $ do
        Right lcb <- lcbCreate $ cbConnect "test" "qwerty12345" "couchbase://localhost/test"
        lcbConnect lcb `shouldReturn` Left LcbErrAuthenticationFailure

    context "when invalid bucket name is given" $ do
      it "should not connect" $ do
        Right lcb <- lcbCreate $ cbConnect "gianluca" "qwerty12345" "couchbase://localhost/not_found"
        lcbConnect lcb `shouldReturn` Left LcbErrGeneric

  describe "Upsert" $ do
    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op = LcbStore LcbUpsert emptyStoreOpts "test" document
          fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op = LcbStore LcbUpsert (emptyStoreOpts { cas = Just 1 }) "test" document
          lcbStore connection (op :: LcbStore String) `shouldReturn` Left LcbErrInvalidArgument

  describe "Replace" $ do
    context "when key and value is provided with no cas" $ do
      it "should replaced the element" $ do
        withConnection $ \connection -> do
          let op = LcbStore LcbReplace emptyStoreOpts "test" document
          fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing

    context "when cas is valued" $ do
      it "should replace the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let insertOp = LcbStore LcbInsert emptyStoreOpts key document
            fmap snd <$> lcbStore connection (insertOp :: LcbStore String) `shouldReturn` Right Nothing
            Right (cas, _) <- lcbGet connection key
            let replaceOp = LcbStore LcbReplace (emptyStoreOpts { cas = Just cas }) key document
            fmap snd <$> lcbStore connection (replaceOp :: LcbStore String) `shouldReturn` Right Nothing

  describe "Insert" $ do

    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert emptyStoreOpts key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing

    context "when adding twice value with same key" $ do
      it "should fail" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op  = LcbStore LcbInsert emptyStoreOpts key document
                op2 = LcbStore LcbInsert emptyStoreOpts key 1
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            lcbStore connection (op2 :: LcbStore Int) `shouldReturn` Left LcbErrDocumentExists

    context "when cas is valued" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op = LcbStore LcbInsert (emptyStoreOpts { cas = Just 1 }) "test" document
          lcbStore connection (op :: LcbStore String) `shouldReturn` Left LcbErrInvalidArgument

  describe "Get" $ do

    context "when an existing key is provided" $ do
      it "should retrun the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert emptyStoreOpts key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            Right (_, Just response) <- lcbGet connection key
            response `shouldBe` valueOf document

  describe "Touch" $ do

    context "when touch an existing document" $ do

      it "should return document not found error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert emptyStoreOpts key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            fmap snd <$> lcbTouch connection key 4 `shouldReturn` Right Nothing
            threadDelay 5000000
            lcbGet connection key `shouldReturn` Left LcbErrDocumentNotFound

  describe "Remove" $ do

    context "when retrive an existing document is deleted" $ do

      context "and the document is retrieved" $ do

        it "should return document not found error" $ do
          withConnection $ \connection -> do
            withKey $ \key -> do
              let op = LcbStore LcbInsert emptyStoreOpts key document
              fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
              Right (_, Just response) <- lcbGet connection key
              response `shouldBe` valueOf document
              Right _ <- lcbRemove connection key Nothing
              lcbGet connection key `shouldReturn` Left LcbErrDocumentNotFound

    context "when deleting a document with wrong cas" $ do

      it "should return document alraedy exists error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert emptyStoreOpts key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            Right (_, Just response) <- lcbGet connection key
            response `shouldBe` valueOf document
            lcbRemove connection key (Just 2) `shouldReturn` Left LcbErrDocumentExists

    context "when a non exisiting document is deleted" $ do
      it "should return document not found error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            lcbRemove connection key Nothing `shouldReturn` Left LcbErrDocumentNotFound

  describe "Counter" $ do

    context "when is created for the fitst time" $ do

      it "should return the default value" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap snd <$> lcbCounter connection key defaultValue 1 Nothing Nothing `shouldReturn` Right defaultValue

    context "when is increment is performed" $ do

      it "should return the default value increased by one" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap snd <$> lcbCounter connection key defaultValue 1 Nothing Nothing `shouldReturn` Right defaultValue
            fmap snd <$> lcbCounter connection key defaultValue 1 Nothing Nothing `shouldReturn` Right (defaultValue + 1)

    context "when is decrement is performed" $ do

      it "should return the default value decreased by one" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap snd <$> lcbCounter connection key defaultValue 1 Nothing Nothing `shouldReturn` Right defaultValue
            fmap snd <$> lcbCounter connection key defaultValue (-1) Nothing Nothing `shouldReturn` Right (defaultValue - 1)

  describe "Expire" $ do

    context "when a document with expire time is create" $ do

      it "should not be found if the time exceed the exptime" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert (emptyStoreOpts { exptime = Just 5 }) key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            threadDelay 6000000
            lcbGet connection key `shouldReturn` Left LcbErrDocumentNotFound

      it "should be found if the time does not exceed the exptime" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = LcbStore LcbInsert (emptyStoreOpts { exptime = Just 5 }) key document
            fmap snd <$> lcbStore connection (op :: LcbStore String) `shouldReturn` Right Nothing
            threadDelay 3000000
            Right (_, Just response) <- lcbGet connection key
            response `shouldBe` valueOf document
