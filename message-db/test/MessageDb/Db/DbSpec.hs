{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MessageDb.Db.DbSpec (dbTests) where

import Hasql.Pool (Pool, use)
import MessageDb.Db.Sessions
import MessageDb.Db.TastySetup (withPool)
import MessageDb.Stream qualified as S
import MessageDb.Message ( NewMessage(..), StreamPosition(..) )
import Test.Tasty
import Test.Tasty.HUnit
import Generator (newMessageGen, genStream)
import Hedgehog.Gen qualified as Gen
import MessageDb.Db.Statements
    ( GetStreamMessagesQuery(GetStreamMessagesQuery),
      GetCategoryMessagesQuery(GetCategoryMessagesQuery) )
import Data.Vector ((!))
import MessageDb.Message (represents)
import Data.Functor ((<&>))

getStreamMessagesTests :: Pool -> IO ()
getStreamMessagesTests pool = do
  msg <- Gen.sample newMessageGen
  let stream = msg.stream
  msg2 <- Gen.sample newMessageGen
  let msg2' = msg2 { stream }
  _ <- use pool (writeStreamMessage msg) >>= either (assertFailure . show) pure
  _ <- use pool (writeStreamMessage msg2') >>= either (assertFailure . show) pure
  let query = GetStreamMessagesQuery stream Nothing Nothing Nothing
  messages <- use pool (getStreamMessages query) >>= either (assertFailure . show) pure
  assertEqual "Got two messages back" (length messages) 2
  assertEqual "First msg is persisted correctly" (represents (messages ! 0) msg) True
  assertEqual "Second msg is persisted correctly" (represents (messages ! 1) msg2') True

getCategoryMessagesTests :: Pool -> IO ()
getCategoryMessagesTests pool = do
  msg <- Gen.sample newMessageGen
  let stream = msg.stream
  msg2 <- Gen.sample newMessageGen
  let msg2' = msg2 { stream }
  _ <- use pool (writeStreamMessage msg) >>= either (assertFailure . show) pure
  _ <- use pool (writeStreamMessage msg2') >>= either (assertFailure . show) pure
  let query = GetCategoryMessagesQuery stream.category Nothing Nothing Nothing Nothing Nothing
  messages <- use pool (getCategoryMessages query) >>= either (assertFailure . show) pure
  assertEqual "Got two category messages back" (length messages) 2
  assertEqual "First returned msg is correct" (represents (messages ! 0) msg) True
  assertEqual "Second returned msg is correct" (represents (messages ! 1) msg2') True

getLastStreamMessageTests :: Pool -> IO ()
getLastStreamMessageTests pool = do
  msg <- Gen.sample newMessageGen
  let stream = msg.stream
  msg2 <- Gen.sample newMessageGen
  let msg2' = msg2 { stream }
  _ <- use pool (writeStreamMessage msg) >>= either (assertFailure . show) pure
  _ <- use pool (writeStreamMessage msg2') >>= either (assertFailure . show) pure
  lastMsg <- use pool (getLastStreamMessage stream) >>= either (assertFailure . show) pure
  assertEqual "Properly get the last stream message" (lastMsg <&> \m -> represents m msg2') (Just True)

writeStreamMessageTests :: Pool -> IO ()
writeStreamMessageTests pool = do
  msg <- Gen.sample newMessageGen
  p <- use pool (writeStreamMessage msg) >>= either (assertFailure . show) pure
  assertEqual "stream position of new message should be 0" p (StreamPosition 0)
  msg2 <- Gen.sample newMessageGen
  p2 <- use pool (writeStreamMessage (msg2 { stream = msg.stream })) >>= either (assertFailure . show) pure
  assertEqual "stream position of new message should be 1" p2 (StreamPosition 1)

getStreamVersionTests :: Pool -> IO ()
getStreamVersionTests pool = do
  msg <- Gen.sample newMessageGen
  let stream = msg.stream
  msg2 <- Gen.sample newMessageGen
  _ <- use pool (writeStreamMessage msg) >>= either (assertFailure . show) pure
  _ <- use pool (writeStreamMessage msg2 { stream}) >>= either (assertFailure . show) pure
  v <- use pool (getStreamVersion stream) >>= either (assertFailure . show) pure
  assertEqual "stream version should be 1" v (Just $ StreamPosition 1)
  emptyStream <- Gen.sample genStream
  v2 <- use pool (getStreamVersion emptyStream) >>= either (assertFailure . show) pure
  assertEqual "stream version should be Nothing for non persisted streams" Nothing v2

dbTests :: TestTree
dbTests = withPool $ \pool ->
  testGroup
    "MessageDb"
    [testCase "getStreamMessages" (pool >>= getStreamMessagesTests),
     testCase "writeStreamMessage" (pool >>= writeStreamMessageTests),
     testCase "getLastStreamMessage" (pool >>= getLastStreamMessageTests),
     testCase "getStreamVersion" (pool >>= getStreamVersionTests),
     testCase "getCategoryMessages" (pool >>= getCategoryMessagesTests)
    ]
