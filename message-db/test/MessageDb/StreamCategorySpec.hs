module MessageDb.StreamCategorySpec (streamCategoryProps) where

import Generator (genStreamCategory)
import Hedgehog
import MessageDb.StreamCategory
import Test.Tasty
import Test.Tasty.Hedgehog

prop_fromToText :: Property
prop_fromToText = property $ do
  i <- forAll genStreamCategory
  tripping i toText fromText

streamCategoryProps :: TestTree
streamCategoryProps =
  testGroup
    "StreamCategory property tests"
    [testProperty "MessageDb.StreamCategory tripping toText/fromText" prop_fromToText]
