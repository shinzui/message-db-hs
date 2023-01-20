module Generators.UUID (genUUID) where

import Data.UUID (UUID, fromWords)
import Hedgehog
import Hedgehog.Gen (word32)
import Hedgehog.Range (linearBounded)

genUUID :: Gen UUID
genUUID = fromWords <$> genWord <*> genWord <*> genWord <*> genWord
  where
    genWord = word32 linearBounded
