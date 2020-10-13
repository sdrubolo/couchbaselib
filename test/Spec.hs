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
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import qualified Data.Binary.Get               as G


instance LcbValueOf [Char] where
  valueOf = encodeUtf8 . T.pack
  toValue = T.unpack . decodeUtf8

instance LcbValueOf Int where
  valueOf = valueOf . show
  toValue = read . toValue

defaultParams :: CBConnect
defaultParams = cbConnect "gianluca" "qwerty12345" "couchbase://localhost/test"

cbConnect username password connection =
  CBConnect {cb_bucket_type = LcbTypeBucket, cb_username = username, cb_password = password, cb_connection = connection}

withConnection fn = do
  Right lcb <- lcbConnect defaultParams
  fn lcb

withKey fn = do
  randomKey <- randomWord randomASCII 20
  fn randomKey

document = "{ \"value\" : \"test\", \"type\" : \"test4\" }"

toInt = (fmap toValue) . snd

main :: IO ()
main = hspec $ do


  describe "Connect" $ do

    context "when invalid credentials are given" $ do
      it "should not connect" $ do
        lcbConnect (cbConnect "test" "qwerty12345" "couchbase://localhost/test")
          `shouldReturn` Left LcbErrAuthenticationFailure

    context "when invalid bucket name is given" $ do
      it "should not connect" $ do
        lcbConnect (cbConnect "gianluca" "qwerty12345" "couchbase://localhost/not_found") `shouldReturn` Left LcbErrGeneric

  describe "Upsert" $ do
    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          let op = Upsert "test" document Nothing
          fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing

  describe "Replace" $ do
    context "when key and value is provided with no cas" $ do
      it "should replaced the element" $ do
        withConnection $ \connection -> do
          let op = Replace "test" document Nothing Nothing
          fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing

    context "when cas is valued" $ do
      it "should replace the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let insertOp = Insert key document Nothing
            fmap snd <$> lcb connection (insertOp :: Store String) `shouldReturn` Right Nothing
            Right (cas, _) <- lcb connection (Get key)
            let replaceOp = Replace key document Nothing (Just cas)
            fmap snd <$> lcb connection (replaceOp :: Store String) `shouldReturn` Right Nothing

  describe "Insert" $ do

    context "when key and value is provided" $ do
      it "should add the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document Nothing
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing

    context "when adding twice value with same key" $ do
      it "should fail" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op  = Insert key document Nothing
                op2 = Insert key 1 Nothing
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            lcb connection (op2 :: Store Int) `shouldReturn` Left LcbErrDocumentExists

  describe "Get" $ do

    context "when an existing key is provided" $ do
      it "should retrun the element" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document Nothing
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            Right (_, Just response) <- lcb connection (Get key)
            response `shouldBe` valueOf document

  describe "Touch" $ do

    context "when touch an existing document" $ do

      it "should return document not found error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document Nothing
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            fmap snd <$> lcb connection (Touch key (Just 4)) `shouldReturn` Right Nothing
            threadDelay 5000000
            lcb connection (Get key) `shouldReturn` Left LcbErrDocumentNotFound

  describe "Remove" $ do

    context "when retrive an existing document is deleted" $ do

      context "and the document is retrieved" $ do

        it "should return document not found error" $ do
          withConnection $ \connection -> do
            withKey $ \key -> do
              let op = Insert key document Nothing
              fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
              Right (_, Just response) <- lcb connection (Get key)
              response `shouldBe` valueOf document
              Right _ <- lcb connection (Remove key Nothing)
              lcb connection (Get key) `shouldReturn` Left LcbErrDocumentNotFound

    context "when deleting a document with wrong cas" $ do

      it "should return document alraedy exists error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document Nothing
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            Right (_, Just response) <- lcb connection (Get key)
            response `shouldBe` valueOf document
            lcb connection (Remove key (Just 2)) `shouldReturn` Left LcbErrDocumentExists

    context "when a non exisiting document is deleted" $ do
      it "should return document not found error" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            lcb connection (Remove key Nothing) `shouldReturn` Left LcbErrDocumentNotFound

  describe "Counter" $ do

    context "when is created for the fitst time" $ do

      it "should return the default value" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap toInt <$> lcb connection (Counter key defaultValue 1 Nothing Nothing) `shouldReturn` Right
              (Just defaultValue)

    context "when is increment is performed" $ do

      it "should return the default value increased by one" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap toInt <$> lcb connection (Counter key defaultValue 1 Nothing Nothing) `shouldReturn` Right
              (Just defaultValue)
            fmap toInt <$> lcb connection (Counter key defaultValue 1 Nothing Nothing) `shouldReturn` Right
              (Just $ defaultValue + 1)

    context "when is decrement is performed" $ do

      it "should return the default value decreased by one" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            fmap toInt <$> lcb connection (Counter key defaultValue 1 Nothing Nothing) `shouldReturn` Right
              (Just defaultValue)
            fmap toInt <$> lcb connection (Counter key defaultValue (-1) Nothing Nothing) `shouldReturn` Right
              (Just $ defaultValue - 1)

  describe "Expire" $ do

    context "when a document with expire time is create" $ do

      it "should not be found if the time exceed the exptime" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document (Just 5)
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            threadDelay 6000000
            lcb connection (Get key) `shouldReturn` Left LcbErrDocumentNotFound

      it "should be found if the time does not exceed the exptime" $ do
        withConnection $ \connection -> do
          withKey $ \key -> do
            let op = Insert key document (Just 5)
            fmap snd <$> lcb connection (op :: Store String) `shouldReturn` Right Nothing
            threadDelay 3000000
            Right (_, Just response) <- lcb connection (Get key)
            response `shouldBe` valueOf document
