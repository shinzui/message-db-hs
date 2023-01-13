module Generators.Ranges
  ( Ranges (..),
    StringRange (..),
    NumberRange (..),
    IntegerRange (..),
    ArrayRange (..),
    ObjectRange (..),
  )
where

import Hedgehog

newtype StringRange = StringRange
  { unStringRange :: Range Int
  }

newtype NumberRange = NumberRange
  { unNumberRange :: Range Double
  }

newtype IntegerRange = IntegerRange
  { unIntegerRange :: Range Integer
  }

newtype ArrayRange = ArrayRange
  { unArrayRange :: Range Int
  }

newtype ObjectRange = ObjectRange
  { unObjectRange :: Range Int
  }

data Ranges = Ranges
  { numberRange :: NumberRange,
    integerRange :: IntegerRange,
    stringRange :: StringRange,
    arrayRange :: ArrayRange,
    objectRange :: ObjectRange
  }
