{-# LANGUAGE OverloadedRecordDot #-}

{-
This module is taken from [haskell-hedgehog-gen-json](https://github.com/amrhassan/haskell-hedgehog-gen-json/blob/master/src/Hedgehog/Gen/JSON/Unconstrained.hs) project.
-}
module Generators.Aeson(genValue, genObj, genArray) where

import Data.Aeson qualified as A
import Generators.Ranges
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Data.Scientific qualified as Scientific
import Data.Vector qualified as Vector
import qualified Data.Aeson.Key as A

genNull :: Gen A.Value
genNull = pure A.Null

genStringValue :: StringRange -> Gen A.Value
genStringValue (StringRange sr) = A.String <$> Gen.text sr Gen.unicode

genBool :: Gen A.Value
genBool = A.Bool <$> Gen.bool

genNumber :: NumberRange -> Gen A.Value
genNumber (NumberRange nr) = A.Number . Scientific.fromFloatDigits <$> Gen.double nr

genObj :: Ranges -> Gen A.Value
genObj ranges = A.object <$> kv
  where
    kv =  Gen.list ar ((,) <$>  (A.fromText <$> Gen.text sr Gen.unicode) <*> genValue ranges)
    (StringRange sr) = ranges.stringRange
    (ArrayRange ar) = ranges.arrayRange

genArray :: Ranges -> Gen A.Value
genArray ranges = do
  let gen = Gen.recursive Gen.choice [genBool, genNumber nr, genStringValue sr] [genArray ranges, genObj ranges]
  A.Array . Vector.fromList <$> Gen.list ar gen
  where
    nr = ranges.numberRange
    sr = ranges.stringRange
    ArrayRange ar = ranges.arrayRange

genValue :: Ranges -> Gen A.Value
genValue ranges = Gen.choice [genNull, genStringValue (ranges.stringRange), genBool, genNumber (ranges.numberRange), genArray ranges, genObj ranges]

