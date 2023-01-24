{-# LANGUAGE RecordWildCards #-}

module Generator
  ( newMessageGen,
    genValidStreamName,
    genStream,
    genStreamIdentifier,
    genValidStreamIdentifier,
    genStreamCategory,
    genCategoryName,
  )
where

import Control.Lens
import Data.Text (Text, pack)
import Data.UUID qualified as UUID
import Generators.Aeson (genValue, sensibleRanges)
import Generators.UUID (genUUID)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MessageDb.Message
import MessageDb.Stream
import MessageDb.StreamCategory (StreamCategory)
import MessageDb.StreamCategory qualified as C
import MessageDb.StreamIdentifier (StreamIdentifier)
import MessageDb.StreamIdentifier qualified as I

genCategoryName :: Gen Text
genCategoryName =
  Gen.text (Range.linear 5 30) validChars
  where
    validChars =
      Gen.frequency
        [ (30, Gen.alphaNum),
          (1, pure ':')
        ]

genValidStreamIdentifier :: Gen Text
genValidStreamIdentifier = Gen.choice [genUUIDText, genNaturalText]
  where
    genUUIDText = genUUID <&> UUID.toText
    genNaturalText = Gen.word32 Range.linearBounded <&> pack . show

genValidStreamName :: Gen Text
genValidStreamName = do
  category <- genCategoryName
  identifier <- genValidStreamIdentifier
  pure $ category <> "-" <> identifier

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
  pure $ fromText streamName ^?! _Right

genStreamIdentifier :: Gen StreamIdentifier
genStreamIdentifier = do
  i <- genValidStreamIdentifier
  pure $ I.fromText i ^?! _Right

genStreamCategory :: Gen StreamCategory
genStreamCategory = do
  c <- genCategoryName
  pure $ C.fromText c ^?! _Right

newMessageGen :: Gen NewMessage
newMessageGen = do
  stream <- genStream
  messageId <- genMessageId
  messageType <- genMessageType
  messageData <- genMessageData
  messageMetadata <- genMessageMetadata

  let expectedPosition = Nothing
  pure NewMessage {..}
