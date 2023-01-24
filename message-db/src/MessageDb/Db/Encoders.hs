module MessageDb.Db.Encoders
  ( newMessageEncoder,
    streamValue,
    streamCategoryValue,
    streamPositionValue,
    streamEncoder,
  )
where

import Control.Lens
import Data.Coerce (coerce)
import Data.Functor.Contravariant ((>$<))
import Hasql.Encoders qualified as E
import MessageDb.Message
import MessageDb.Stream
import MessageDb.StreamCategory (StreamCategory)
import MessageDb.StreamCategory qualified as S

newMessageEncoder :: E.Params NewMessage
newMessageEncoder =
  ((\e -> unMessageId $ e ^. #messageId) >$< E.param (E.nonNullable E.uuid))
    <> ((\e -> e ^. #stream) >$< E.param (E.nonNullable streamValue))
    <> ((\e -> unMessageType $ e ^. #messageType) >$< E.param (E.nonNullable E.text))
    <> ((\e -> unMessageData $ e ^. #messageData) >$< E.param (E.nonNullable E.jsonb))
    <> ((\e -> unMessageMetadata $ e ^. #messageMetadata) >$< E.param (E.nonNullable E.jsonb))
    <> ((coerce <$> expectedPosition) >$< E.param (E.nullable E.int8))

streamValue :: E.Value Stream
streamValue = toText >$< E.text

streamCategoryValue :: E.Value StreamCategory
streamCategoryValue = S.toText >$< E.text

streamPositionValue :: E.Value StreamPosition
streamPositionValue = (coerce <$> unStreamPosition) >$< E.int8

streamEncoder :: E.Params Stream
streamEncoder = E.param (E.nonNullable streamValue)
