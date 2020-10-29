{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.Set                      as S
import           Control.Parallel.Strategies

defaultParams :: CBConnect
defaultParams = cbConnect "gianluca" "qwerty12345" "couchbase://localhost/test"

cbConnect username password connection =
  CBConnect {cb_bucket_type = LcbTypeBucket, cb_username = username, cb_password = password, cb_connection = connection}

withConnection = do
  Right lcb <- lcbConnect defaultParams
  return lcb

withKey fn = do
  randomKey <- randomWord randomASCII 20
  fn randomKey

document = do
  randomKey <- randomWord randomASCII 40
  return $ toByteString $ "{ \"name\" : \"" <> randomKey <> "\", \"type\" : \"test4\" }"

toByteString = encodeUtf8 . T.pack

mkDocument connection = withKey $ \key -> do
  doc <- document
  let op = Insert key doc Nothing
  lcb connection op
  Right (cas, response) <- lcb connection (Get key)
  response `shouldBe` doc
  return (key, doc, cas)

insert = f
 where
  f _          0 = return []
  f connection n = withKey $ \key -> do
    let stringDocument = "{ \"name\" : " <> show n <> ", \"type\" : \"query\" }"
        doc            = toByteString $ stringDocument
        insertOp       = Insert key doc Nothing
    lcb connection insertOp
    threadDelay 500000
    ((key, toByteString $ "{\"test\":" <> stringDocument <> "}") :) <$> f connection (n - 1)


remove connection = mapM_ g
 where
  g key = do
    threadDelay 500000
    lcb connection (Remove key Nothing)

errorCode (Left Error {..}) = statusCode
errorCode _                 = error $ "errorCode: no error match found"

infoQuery (Left Error {..}) = msg $ head info
infoQuery _                 = error $ "infoQuery: no error match found"

result (Right queryResult) = queryResult
result noMatch             = error $ "result: no error match found" <> show noMatch


main :: IO ()
main = hspec $ do

  describe "Connect" $ do

    beforeAll withConnection $ do

      context "when invalid credentials are given" $ do

        it "should not connect" $ \_ -> do
          errorCode
            <$>            lcbConnect (cbConnect "test" "qwerty12345" "couchbase://localhost/test")
            `shouldReturn` LcbErrAuthenticationFailure

      context "when invalid bucket name is given" $ do

        it "should not connect" $ \_ -> do
          errorCode
            <$>            lcbConnect (cbConnect "gianluca" "qwerty12345" "couchbase://localhost/not_found")
            `shouldReturn` LcbErrGeneric

  describe "Insert" $ do

    beforeAll withConnection $ do

      context "when key and value is provided" $ do

        it "should add the element" $ \connection -> do
          withKey $ \key -> do
            doc                 <- document
            Right _             <- lcb connection (Insert key doc Nothing)
            Right (_, response) <- lcb connection (Get key)
            response `shouldBe` doc

      context "when adding twice value with same key" $ do

        it "should fail" $ \connection -> do
          withKey $ \key -> do
            doc <- document
            let op = Insert key (toByteString $ show 1) Nothing
            Right _ <- lcb connection (Insert key doc Nothing)
            errorCode <$> lcb connection op `shouldReturn` LcbErrDocumentExists

  describe "Get" $ do

    beforeAll withConnection $ do

      context "when an existing key is provided" $ do

        it "should retrun the element" $ \connection -> do
          withKey $ \key -> do
            doc <- document
            let op = Insert key doc Nothing
            Right _             <- lcb connection op
            Right (_, response) <- lcb connection (Get key)
            response `shouldBe` doc

      context "when a non existing key is provided" $ do

        it "should retrun the element" $ \connection -> do
          withKey $ \key -> do
            errorCode <$> lcb connection (Get key) `shouldReturn` LcbErrDocumentNotFound

  describe "Upsert" $ do

    beforeAll withConnection $ do

      context "when key and value is provided" $ do

        it "should upsert the element" $ \connection -> do
          withKey $ \key -> do
            doc <- document
            let op = Upsert key doc Nothing
            Right _             <- lcb connection op
            Right (_, response) <- lcb connection (Get key)
            response `shouldBe` doc

  describe "Replace" $ do

    beforeAll withConnection $ do

      context "when key and value is provided with no cas" $ do

        it "should replaced the element" $ \connection -> do
          (key, _, _) <- mkDocument connection
          doc         <- document
          let op = Replace key doc Nothing Nothing
          lcb connection op
          Right (_, response) <- lcb connection (Get key)
          response `shouldBe` doc

      context "when cas is valued" $ do

        it "should replace the element" $ \connection -> do
          (key, _, cas) <- mkDocument connection
          doc           <- document
          let replaceOp = Replace key doc Nothing (Just cas)
          Right _             <- lcb connection replaceOp
          Right (_, response) <- lcb connection (Get key)
          response `shouldBe` doc

  describe "Touch" $ do

    beforeAll withConnection $ do

      context "when touch an existing document" $ do

        it "should return document not found error" $ \connection -> do
          (key, _, _) <- mkDocument connection
          Right _     <- lcb connection (Touch key (Just 4))
          threadDelay 5000000
          errorCode <$> lcb connection (Get key) `shouldReturn` LcbErrDocumentNotFound

  describe "Remove" $ do

    beforeAll withConnection $ do

      context "when retrive an existing document is deleted" $ do

        context "and the document is retrieved" $ do

          it "should return document not found error" $ \connection -> do
            (key, _, _) <- mkDocument connection
            Right _     <- lcb connection (Remove key Nothing)
            errorCode <$> lcb connection (Get key) `shouldReturn` LcbErrDocumentNotFound

      context "when deleting a document with wrong cas" $ do

        it "should return document alraedy exists error" $ \connection -> do
          (key, _, _) <- mkDocument connection
          errorCode <$> lcb connection (Remove key (Just 2)) `shouldReturn` LcbErrDocumentExists

      context "when a non exisiting document is deleted" $ do

        it "should return document not found error" $ \connection -> do
          withKey $ \key -> do
            errorCode <$> lcb connection (Remove key Nothing) `shouldReturn` LcbErrDocumentNotFound


  describe "Counter" $ do

    beforeAll withConnection $ do

      context "when is created for the fitst time" $ do

        it "should return the default value" $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            Right (_, value) <- lcb connection (Counter key defaultValue 1 Nothing Nothing)
            value `shouldBe` defaultValue

      context "when is increment is performed" $ do

        it "should return the default value increased by one" $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            Right (_, value) <- lcb connection (Counter key defaultValue 1 Nothing Nothing)
            value `shouldBe` defaultValue
            Right (_, increased) <- lcb connection (Counter key defaultValue 1 Nothing Nothing)
            increased `shouldBe` (defaultValue + 1)

      context "when is decrement is performed" $ do

        it "should return the default value decreased by one" $ \connection -> do
          withKey $ \key -> do
            let defaultValue = 10
            Right (_, value) <- lcb connection (Counter key defaultValue 1 Nothing Nothing)
            value `shouldBe` defaultValue
            Right (_, increased) <- lcb connection (Counter key defaultValue (-1) Nothing Nothing)
            increased `shouldBe` (defaultValue - 1)


  describe "Expire" $ do

    beforeAll withConnection $ do

      context "when a document with expire time is create" $ do

        it "should not be found if the time exceed the exptime" $ \connection -> do
          withKey $ \key -> do
            doc <- document
            lcb connection (Insert key doc (Just 5))
            Right (cas, response) <- lcb connection (Get key)
            response `shouldBe` doc
            threadDelay 6000000
            errorCode <$> lcb connection (Get key) `shouldReturn` LcbErrDocumentNotFound

        it "should be found if the time does not exceed the exptime" $ \connection -> do
          (key, doc, _) <- mkDocument connection
          threadDelay 3000000
          Right (_, response) <- lcb connection (Get key)
          response `shouldBe` doc

  describe "Query" $ do

    beforeAll
        (withConnection >>= \connection -> insert connection 10 >>= \keys -> threadDelay 7000000 >> return (connection, keys)
        )
      . afterAll (\(connection, keys) -> remove connection (map fst keys))
      $ do

          let emptyOpts = QueryOpts {namedParams = [], posParams = [], consistency = Nothing, adhoc = Nothing, options = []}

          context "when query is executed with strong consistency" $ do

            it "should return all the items found" $ \(connection, keys) -> do
              let op = Query (emptyOpts { consistency = Just LcbConsistencyStatement })
                             "SELECT * from `test` where name is VALUED AND type = \"query\""
              (count, result) <- result <$> lcb connection op
              count `shouldBe` length keys
              result `shouldBe` (reverse $ map snd keys)

          context "when query is executed" $ do

            it "should return all the items found" $ \(connection, keys) -> do
              let op = Query emptyOpts "SELECT * from `test` where name is VALUED AND type = \"query\""
              (count, result) <- result <$> lcb connection op
              count `shouldBe` length keys
              S.fromList result `shouldBe` S.fromList (map snd keys)

          context "when query with name paramter is executed" $ do

            it "should return all the items found" $ \(connection, keys) -> do
              let
                op = Query (emptyOpts { namedParams = [("name", "1")] })
                           "SELECT * from `test` where name = $name AND type = \"query\""
              (count, result) <- result <$> lcb connection op
              count `shouldBe` 1
              head result `shouldBe` (last $ map snd keys)

          context "when query with positional paramter is executed" $ do

            it "should return all the items found" $ \(connection, keys) -> do
              let op = Query (emptyOpts { posParams = ["1"] }) "SELECT * from `test` where name = $1 AND type = \"query\""
              (count, result) <- result <$> lcb connection op
              count `shouldBe` 1
              head result `shouldBe` (last $ map snd keys)

          context "when query index is missing" $ do

            it "should return an error" $ \(connection, keys) -> do
              let op = Query (emptyOpts { namedParams = [("query", "query")], posParams = ["1"] })
                             "SELECT * from `test` where name = $1 AND type = $query"
              queryResult <- lcb connection op
              errorCode queryResult `shouldBe` LcbErrPlanningFailure
              infoQuery queryResult
                `shouldBe` "No index available on keyspace test that matches your query. Use CREATE INDEX or CREATE PRIMARY INDEX to create an index, or check that your expected index is online."

          context "when missing named parameter is given" $ do

            it "should return an error" $ \(connection, keys) -> do
              let op = Query (emptyOpts { namedParams = [("name", "1")], posParams = [] })
                             "SELECT * from `test` where name = $missing AND type = \"query\""
              queryResult <- lcb connection op
              errorCode queryResult `shouldBe` LcbErrInternalServerFailure
              infoQuery queryResult `shouldBe` "Error evaluating span. - cause: No value for named parameter $missing."

          context "when run two queries in parallel" $ do

            it "should return an error" $ \(connection, keys) -> do
              let query value = Query (emptyOpts { namedParams = [("name", value)], posParams = [] })
                                      "SELECT * from `test` where name = $name AND type = \"query\""
              let (queryResult1, queryResult2) = runEval $ do
                    queryResult1 <- rpar $ lcb connection (query "1")
                    queryResult2 <- rpar $ lcb connection (query "10")
                    rseq queryResult1
                    rseq queryResult2
                    return (queryResult1, queryResult2)

              (count1, result1) <- result <$> queryResult1
              (count2, result2) <- result <$> queryResult2

              count1 `shouldBe` 1
              head result1 `shouldBe` (last $ map snd keys)
              count2 `shouldBe` 1
              head result2 `shouldBe` (head $ map snd keys)
