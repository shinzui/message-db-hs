module Main (main) where

import Internal.GeneratorSpec
import MessageDb.Db.DbSpec
import MessageDb.StreamIdentifierSpec
import MessageDb.StreamSpec
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests"
      [ internalGeneratorProps,
        streamProps,
        streamIdentifierProps,
        dbTests
      ]
