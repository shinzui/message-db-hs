{-# LANGUAGE RecordWildCards #-}

module Generator (newMessageGen, genValidStreamName, genStream) where

import Control.Lens
import Data.Text (Text)
import Data.UUID qualified as UUID
import Generators.Aeson (genValue, sensibleRanges)
import Generators.UUID (genUUID)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MessageDb.Message
import MessageDb.Stream

genCategoryName :: Gen Text
genCategoryName =
  Gen.text (Range.linear 5 30) validChars
  where
    validChars =
      Gen.frequency
        [ (30, Gen.alphaNum),
          (1, pure ':')
        ]

genValidStreamName :: Gen Text
genValidStreamName = do
  category <- genCategoryName
  identifier <- genUUID
  pure $ category <> "-" <> UUID.toText identifier

genMessageId :: Gen MessageId
genMessageId = MessageId <$> genUUID

genMessageType :: Gen MessageType
genMessageType = MessageType <$> Gen.text (Range.linear 10 100) Gen.alphaNum

genMessageData :: Gen MessageData
genMessageData = MessageData <$> genValue sensibleRanges

genMessageMetadata :: Gen MessageMetadata
genMessageMetadata = MessageMetadata <$> genValue sensibleRanges

genStream :: Gen Stream
genStream = do
  streamName <- genValidStreamName
  pure $ parse streamName ^?! _Right

newMessageGen :: Gen NewMessage
newMessageGen = do
  stream <- genStream
  messageId <- genMessageId
  messageType <- genMessageType
  messageData <- genMessageData
  messageMetadata <- genMessageMetadata

  let expectedPosition = Nothing
  pure NewMessage {..}
