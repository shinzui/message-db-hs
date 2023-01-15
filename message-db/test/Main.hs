module Main (main) where

import MessageDb.StreamSpec
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ testGroup "Property tests" props
      ]

props :: [TestTree]
props = [testProperty "MessageDb.Stream.parse parses stream names properly" prop_parseStream]
