module MessageDb.Db.Decoders
  ( messageDecoder,
    streamDecoder,
  )
where

import Hasql.Decoders qualified as D
import MessageDb.Message
import MessageDb.Stream

messageDecoder :: D.Row Message
messageDecoder =
  Message
    <$> (MessageId <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable streamDecoder)
    <*> (MessageType <$> D.column (D.nonNullable D.text))
    <*> (StreamPosition <$> D.column (D.nonNullable D.int8))
    <*> (GlobalPosition <$> D.column (D.nonNullable D.int8))
    <*> (MessageData <$> D.column (D.nonNullable D.json))
    <*> (MessageMetadata <$> D.column (D.nonNullable D.json))
    <*> D.column (D.nonNullable D.timestamptz)

streamDecoder :: D.Value Stream
streamDecoder = D.refine parse D.text
