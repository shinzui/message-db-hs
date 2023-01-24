module MessageDb.StreamIdentifier
  ( StreamIdentifier,
    fromText,
    toText,
    fromNatural,
    fromUUID,
  )
where

import Data.Text (Text, null, pack)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (null)

newtype StreamIdentifier = StreamIdentifier {unIdentifier :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

fromNatural :: Natural -> StreamIdentifier
fromNatural n = StreamIdentifier $ pack (show n)

fromUUID :: UUID -> StreamIdentifier
fromUUID u = StreamIdentifier $ UUID.toText u

toText :: StreamIdentifier -> Text
toText = unIdentifier

fromText :: Text -> Either Text StreamIdentifier
fromText t
  | null t = Left "Identifier name must at least be 1 character long"
  | otherwise = Right $ StreamIdentifier t
