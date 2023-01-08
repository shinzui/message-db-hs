module MessageDb.Stream (Stream (..), parse, toText) where

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

toText :: Stream -> Text
toText (Stream c i) = unStreamCategory c <> "-" <> unIdentifier i

parse :: Text -> Either Text Stream
parse t = case split (== '-') t of
  [] -> Left $ "Invalid stream name: " <> t
  [_c] -> Left "Invalid stream. Streams must have a '-' to separate the stream category from the stream identifier"
  (c : i) -> Right $ Stream (StreamCategory c) (Identifier $ intercalate "-" i)
