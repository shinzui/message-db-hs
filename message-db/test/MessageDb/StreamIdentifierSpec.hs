module MessageDb.StreamIdentifierSpec (streamIdentifierProps) where

import Generator (genStreamIdentifier)
import Hedgehog
import MessageDb.StreamIdentifier
import Test.Tasty
import Test.Tasty.Hedgehog

prop_fromToText :: Property
prop_fromToText = property $ do
  i <- forAll genStreamIdentifier
  tripping i toText fromText

streamIdentifierProps :: TestTree
streamIdentifierProps =
  testGroup
    "StreamIdentifier property tests"
    [testProperty "MessageDb.StreamIdentifier tripping toText/fromText" prop_fromToText]
