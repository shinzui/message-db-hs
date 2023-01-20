module MessageDb.StreamSpec (streamProps) where

import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text, findIndex, length)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MessageDb.Stream (parse)
import Test.Tasty
import Test.Tasty.Hedgehog
import Prelude hiding (length)

genStreamName :: Gen Text
genStreamName = Gen.frequency [(5, genStreamNameWithDash), (1, genStreamNameWithoutDash)]
  where
    genStreamNameWithDash = do
      c <- Gen.text (Range.linear 1 100) Gen.latin1
      i <- Gen.text (Range.linear 1 100) Gen.unicode
      pure $ c <> "-" <> i
    genStreamNameWithoutDash = Gen.text (Range.linear 1 100) Gen.unicode

prop_parseStream :: Property
prop_parseStream = property $ do
  streamName <- forAll genStreamName
  let hasDash = findIndex (== '-') streamName <&> (< length streamName - 1)
      validStreamName = fromMaybe False hasDash
      invalidStreamName = maybe True not hasDash
  cover 10 "valid stream name" validStreamName
  cover 3 "invalid stream name" invalidStreamName
  case parse streamName of
    Left _ | invalidStreamName -> pure ()
    Right _ | validStreamName -> pure ()
    other -> annotateShow other >> fail "Could not parse stream name into a stream"

streamProps :: TestTree
streamProps = testGroup "Stream property tests" [testProperty "MessageDb.Stream.parse parses stream names properly" prop_parseStream]
