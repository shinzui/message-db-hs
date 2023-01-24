module Internal.GeneratorSpec (internalGeneratorProps) where

import Data.Either (isRight)
import Generator (genCategoryName, genValidStreamIdentifier, genValidStreamName)
import Hedgehog
import MessageDb.Stream (fromText)
import MessageDb.StreamCategory qualified as C
import MessageDb.StreamIdentifier qualified as I
import Test.Tasty
import Test.Tasty.Hedgehog

prop_genValidStreamName :: Property
prop_genValidStreamName = property $ do
  streamName <- forAll genValidStreamName
  assert (isRight $ fromText streamName)

prop_genValidStreamIdentifier :: Property
prop_genValidStreamIdentifier = property $ do
  identifier <- forAll genValidStreamIdentifier
  assert (isRight $ I.fromText identifier)

prop_genValidStreamCategory :: Property
prop_genValidStreamCategory = property $ do
  category <- forAll genCategoryName
  assert (isRight $ C.fromText category)

internalGeneratorProps :: TestTree
internalGeneratorProps =
  testGroup
    "Internal generator tests"
    [ testProperty "Internal generate valid stream names" prop_genValidStreamName,
      testProperty "Internal generate valid stream identifier" prop_genValidStreamIdentifier,
      testProperty "Internal generate valid stream category" prop_genValidStreamCategory
    ]
