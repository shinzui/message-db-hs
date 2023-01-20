module MessageDb.Stream
  ( Stream (..),
    StreamCategory,
    Identifier,
    parse,
    toText,
    streamCategoryToText,
  )
where

import Data.Generics.Labels ()
import Data.Text (Text, intercalate, split)
import GHC.Generics (Generic)

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

newtype StreamCategory = StreamCategory {unStreamCategory :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

data Stream = Stream
  { category :: !StreamCategory,
    identifier :: !Identifier
  }
  deriving stock (Eq, Ord, Show, Generic)

mkStream :: StreamCategory -> Identifier -> Stream
mkStream c i = Stream {category = c, identifier = i}

streamCategoryToText :: StreamCategory -> Text
streamCategoryToText = unStreamCategory

toText :: Stream -> Text
toText (Stream c i) = unStreamCategory c <> "-" <> unIdentifier i

parse :: Text -> Either Text Stream
parse t = case split (== '-') t of
  [] -> Left $ "Invalid stream name: " <> t
  [_c] -> Left "Invalid stream. Streams must have a '-' to separate the stream category from the stream identifier"
  (c : i) -> Right $ Stream (StreamCategory c) (Identifier $ intercalate "-" i)
