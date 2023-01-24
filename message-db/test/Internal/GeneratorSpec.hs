module Internal.GeneratorSpec (internalGeneratorProps) where

import Data.Either (isRight)
import Generator (genValidStreamName)
import Hedgehog
import MessageDb.Stream (fromText)
import Test.Tasty
import Test.Tasty.Hedgehog

prop_genValidStreamName :: Property
prop_genValidStreamName = property $ do
  streamName <- forAll genValidStreamName
  assert (isRight $ fromText streamName)

internalGeneratorProps :: TestTree
internalGeneratorProps = testGroup "Internal generator tests" [testProperty "Internal generate valid stream names" prop_genValidStreamName]
