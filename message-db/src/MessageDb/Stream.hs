module MessageDb.Stream
  ( Stream (..),
    Identifier,
    parse,
    toText,
  )
where

import Data.Generics.Labels ()
import Data.Text (Text, intercalate, split)
import GHC.Generics (Generic)
import MessageDb.StreamCategory qualified as C

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

data Stream = Stream
  { category :: !C.StreamCategory,
    identifier :: !Identifier
  }
  deriving stock (Eq, Ord, Show, Generic)

mkStream :: C.StreamCategory -> Identifier -> Stream
mkStream c i = Stream {category = c, identifier = i}

toText :: Stream -> Text
toText (Stream c i) = C.toText c <> "-" <> unIdentifier i

parse :: Text -> Either Text Stream
parse t = case split (== '-') t of
  [] -> Left $ "Invalid stream name: " <> t
  [_c] -> Left "Invalid stream. Streams must have a '-' to separate the stream category from the stream identifier"
  (c : i) -> C.fromText c >>= \c' -> Right $ Stream c' (Identifier $ intercalate "-" i)
