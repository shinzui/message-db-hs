module MessageDb.Message
  ( Message (..),
    NewMessage (..),
    MessageId (..),
    MessageType (..),
    MessageData (..),
    MessageMetadata (..),
    StreamPosition (..),
    GlobalPosition (..),
    represents,
    toMessageType,
  )
where

import Control.Lens
import Data.Aeson (Value)
import Data.Data (Data (toConstr))
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import MessageDb.Stream (Stream)

newtype MessageId = MessageId {unMessageId :: UUID}
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic)

newtype MessageType = MessageType {unMessageType :: Text}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

newtype MessageData = MessageData {unMessageData :: Value}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

newtype MessageMetadata = MessageMetadata {unMessageMetadata :: Value}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

newtype StreamPosition = StreamPosition {unStreamPosition :: Int64}
  deriving newtype (Eq, Num, Ord)
  deriving stock (Show, Generic)

newtype GlobalPosition = GlobalPosition {unGlobalPosition :: Int64}
  deriving newtype (Eq, Num, Ord)
  deriving stock (Show, Generic)

-- | Represent a message in the message store
-- [http://docs.eventide-project.org/user-guide/message-db/anatomy.html#messages-table]
data Message = Message
  { -- | Identifier for a message record
    messageId :: !MessageId,
    -- | The stream to which the message belong
    stream :: !Stream,
    -- | The type of the message
    messageType :: !MessageType,
    -- | The ordinal position of the message in its stream. Position is gapless.
    position :: !StreamPosition,
    -- | The ordinal position of the message in the entire message store. Global position may have gaps.
    globalPosition :: !GlobalPosition,
    -- | Message payload
    messageData :: !MessageData,
    -- | Message metadata
    messageMetadata :: !MessageMetadata,
    -- | Timestamp when the message was written. The timestamp does not include a time zone.
    time :: !UTCTime
  }
  deriving stock (Eq, Generic, Show)

data NewMessage = NewMessage
  { -- | Identifier for a message record
    messageId :: !MessageId,
    -- | The stream to which the message belong
    stream :: !Stream,
    -- | The type of the message
    messageType :: !MessageType,
    -- | Message payload
    messageData :: !MessageData,
    -- | Message metadata
    messageMetadata :: !MessageMetadata,
    -- | The version that the stream is expected to be when the message is written.
    expectedPosition :: !(Maybe StreamPosition)
  }
  deriving stock (Eq, Generic, Show)

represents :: Message -> NewMessage -> Bool
represents msg newMsg =
  newMsg ^. #stream == msg ^. #stream
    && newMsg ^. #messageId == msg ^. #messageId
    && newMsg ^. #messageType == msg ^. #messageType
    && newMsg ^. #messageData == msg ^. #messageData
    && newMsg ^. #messageMetadata == msg ^. #messageMetadata
    && (isNothing (newMsg ^. #expectedPosition) || (newMsg ^. #expectedPosition ^? _Just == Just (msg ^. #position)))

toMessageType :: forall a. Data a => a -> MessageType
toMessageType a = MessageType $ pack . show $ toConstr a
