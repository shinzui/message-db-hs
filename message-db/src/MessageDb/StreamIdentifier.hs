module MessageDb.StreamIdentifier (StreamIdentifier, fromText, toText) where

import Data.Text (Text, null)
import GHC.Generics (Generic)
import Prelude hiding (null)

newtype StreamIdentifier = StreamIdentifier {unIdentifier :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

toText :: StreamIdentifier -> Text
toText = unIdentifier

fromText :: Text -> Either Text StreamIdentifier
fromText t
  | null t = Left "Identifier name must at least be 1 character long"
  | otherwise = Right $ StreamIdentifier t
