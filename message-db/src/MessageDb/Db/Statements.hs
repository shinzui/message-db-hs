module MessageDb.Db.Statements
  ( getLastStreamMessage,
    getStreamMessages,
    getCategoryMessages,
    writeStreamMessage,
  )
where

import Control.Lens
import Data.Coerce (coerce)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement (Statement (..))
import MessageDb.Db.Decoders qualified as D
import MessageDb.Db.Encoders qualified as E
import MessageDb.Message
import MessageDb.Stream
import Numeric.Natural (Natural)

data BatchSize = Unlimited | Limit Word64
  deriving stock (Eq, Show)

batchSizeValue :: E.Value BatchSize
batchSizeValue = batchSizeToInt64 >$< E.int8
  where
    batchSizeToInt64 Unlimited = -1
    batchSizeToInt64 (Limit n) = fromIntegral n

data GetStreamMessagesQuery = GetStreamMessagesQuery
  { stream :: !Stream,
    startingPosition :: !(Maybe StreamPosition),
    batchSize :: !(Maybe BatchSize),
    condition :: !(Maybe QueryCondition)
  }
  deriving stock (Eq, Generic, Show)

getStreamMessagesEncoder :: E.Params GetStreamMessagesQuery
getStreamMessagesEncoder =
  ((\e -> e ^. #stream) >$< E.param (E.nonNullable E.streamValue))
    <> ((fmap coerce . startingPosition) >$< E.param (E.nullable E.int8))
    <> (view #batchSize >$< E.param (E.nullable batchSizeValue))
    <> (view #condition >$< E.param (E.nullable E.text))

-- [https://github.com/message-db/message-db#get-messages-from-a-stream]
getStreamMessages :: Statement GetStreamMessagesQuery (Vector Message)
getStreamMessages = Statement sql encoder decoder True
  where
    sql = "select id::uuid, stream_name, type, position, global_position, data, metadata, time from get_stream_messages($1,$2,$3,$4)"
    encoder = getStreamMessagesEncoder
    decoder = D.rowVector D.messageDecoder

-- [http://docs.eventide-project.org/user-guide/message-db/server-functions.html#get-last-message-from-a-stream]
getLastStreamMessage :: Statement Stream (Maybe Message)
getLastStreamMessage = Statement sql encoder decoder True
  where
    sql = "select id::uuid, stream_name, type, position, global_position, data, metadata, time from get_last_stream_message($1)"
    encoder = E.param (E.nonNullable E.streamValue)
    decoder = D.rowMaybe D.messageDecoder

type QueryCondition = Text

data ConsumerGroup = ConsumerGroup
  { groupMember :: !Natural,
    groupSize :: !Natural
  }
  deriving stock (Eq, Generic, Show)

data GetCategoryMessagesQuery = GetCategoryMessagesQuery
  { streamCategory :: !StreamCategory,
    globalPositionStart :: !(Maybe GlobalPosition),
    batchSize :: !(Maybe BatchSize),
    correlation :: !(Maybe StreamCategory),
    consumerGroup :: !(Maybe ConsumerGroup),
    condition :: !(Maybe QueryCondition)
  }
  deriving stock (Eq, Generic, Show)

getCategoryMessagesQueryEncoder :: E.Params GetCategoryMessagesQuery
getCategoryMessagesQueryEncoder =
  ((\e -> e ^. #streamCategory) >$< E.param (E.nonNullable E.streamCategoryValue))
    <> ((fmap coerce . globalPositionStart) >$< E.param (E.nullable E.int8))
    <> (view #batchSize >$< E.param (E.nullable batchSizeValue))
    <> (view #correlation >$< E.param (E.nullable E.streamCategoryValue))
    <> ((fmap (fromIntegral . groupMember) . view #consumerGroup) >$< E.param (E.nullable E.int4))
    <> ((fmap (fromIntegral . groupMember) . view #consumerGroup) >$< E.param (E.nullable E.int4))
    <> (view #condition >$< E.param (E.nullable E.text))

-- [http://docs.eventide-project.org/user-guide/message-db/server-functions.html#get-messages-from-a-category]
getCategoryMessages :: Statement GetCategoryMessagesQuery (Vector Message)
getCategoryMessages = Statement sql encoder decoder True
  where
    sql = "select id::uuid, stream_name, type, position, global_position, data, metadata, time from get_category_messages($1,$2,$3,$4,$5,$6,$7)"
    encoder = getCategoryMessagesQueryEncoder
    decoder = D.rowVector D.messageDecoder

-- [http://docs.eventide-project.org/user-guide/message-db/server-functions.html#write-a-message]
writeStreamMessage :: Statement NewMessage StreamPosition
writeStreamMessage = Statement sql encoder decoder True
  where
    sql = "select write_message($1::varchar,$2,$3,$4,$5,$6)"
    encoder = E.newMessageEncoder
    decoder = D.singleRow (D.column (D.nonNullable D.streamPositionDecoder))
