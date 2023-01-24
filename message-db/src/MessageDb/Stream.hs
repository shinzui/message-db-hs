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
import MessageDb.StreamIdentifier qualified as I

newtype Identifier = Identifier {unIdentifier :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

data Stream = Stream
  { category :: !C.StreamCategory,
    identifier :: !I.StreamIdentifier
  }
  deriving stock (Eq, Ord, Show, Generic)

toText :: Stream -> Text
toText (Stream c i) = C.toText c <> "-" <> I.toText i

parse :: Text -> Either Text Stream
parse t = case split (== '-') t of
  [] -> Left $ "Invalid stream name: " <> t
  [_c] -> Left "Invalid stream. Streams must have a '-' to separate the stream category from the stream identifier"
  (c : i) ->
    C.fromText c >>= \c' ->
      I.fromText (intercalate "-" i) >>= \i' ->
        Right $ Stream c' i'
