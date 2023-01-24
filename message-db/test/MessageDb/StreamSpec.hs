module MessageDb.StreamSpec (streamProps) where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text (Text, findIndex, length, split)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import MessageDb.Stream (fromText)
import Test.Tasty
import Test.Tasty.Hedgehog
import Prelude hiding (length)

genStreamName :: Gen Text
genStreamName = Gen.frequency [(5, genStreamNameWithDash), (1, genStreamNameWithoutDash)]
  where
    genStreamNameWithDash = do
      c <- Gen.text (Range.linear 0 100) Gen.latin1
      i <- Gen.text (Range.linear 1 100) Gen.unicode
      pure $ c <> "-" <> i
    genStreamNameWithoutDash = Gen.text (Range.linear 1 100) Gen.unicode

prop_parseStream :: Property
prop_parseStream = property $ do
  streamName <- forAll genStreamName
  let hasDash = findIndex (== '-') streamName <&> (< length streamName - 1)
      validCategory = fromMaybe False $ split (== '-') streamName ^? _head <&> (> 0) . length
      validStreamName = fromMaybe False hasDash && validCategory
      invalidStreamName = maybe True not hasDash
      invalidCategory = not validCategory
  cover 10 "valid stream name" validStreamName
  cover 3 "invalid stream name" invalidStreamName
  cover 1 "invalid category name in stream name" invalidCategory
  case fromText streamName of
    Left _ | invalidStreamName -> pure ()
    Right _ | validStreamName -> pure ()
    Left _ | invalidCategory -> pure ()
    other -> annotateShow other >> fail "Could not parse stream name into a stream"

streamProps :: TestTree
streamProps = testGroup "Stream property tests" [testProperty "MessageDb.Stream.parse parses stream names properly" prop_parseStream]
