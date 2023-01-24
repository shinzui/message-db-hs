module MessageDb.StreamCategory (StreamCategory, toText, fromText) where

import Data.Text
import GHC.Generics (Generic)
import Prelude hiding (null)

newtype StreamCategory = StreamCategory {unStreamCategory :: Text}
  deriving newtype (Eq)
  deriving stock (Ord, Show, Generic)

toText :: StreamCategory -> Text
toText = unStreamCategory

fromText :: Text -> Either Text StreamCategory
fromText t
  | null t = Left "Category name must at least be 1 character long"
  | otherwise = Right $ StreamCategory t
