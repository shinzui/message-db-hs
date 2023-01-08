module MessageDb.Message
  ( Message (..),
    NewMessage (..),
    MessageId (..),
    MessageType (..),
    MessageData (..),
    MessageMetadata (..),
    StreamPosition (..),
    GlobalPosition (..),
  )
where

import Data.Aeson (Value)
import Data.Generics.Labels ()
import Data.Int (Int64)
import Data.Text (Text)
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
    -- | The ordinal position of the message in the entire message store. Global position may have gaps.
    globalPosition :: !GlobalPosition,
    -- | Message payload
    messageData :: !MessageData,
    -- | Message metadata
    messageMetadata :: !MessageMetadata,
    -- | The version that the stream is expected to be when the message is written.
    expectedPosition :: !(Maybe StreamPosition)
  }
  deriving stock (Eq, Generic, Show)
