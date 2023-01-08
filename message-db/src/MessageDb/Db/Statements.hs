module MessageDb.Db.Statements
  ( getLastStreamMessage,
    getStreamMessages,
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

data BatchSize = Unlimited | Limit Word64
  deriving stock (Eq, Show)

batchSizeValue :: E.Value BatchSize
batchSizeValue = batchSizeToInt64 >$< E.int8
  where
    batchSizeToInt64 Unlimited = -1
    batchSizeToInt64 (Limit n) = fromIntegral n

type QueryCondition = Text

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