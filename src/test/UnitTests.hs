module UnitTests
  ( unitTests
  ) where

import Test.Tasty (TestTree, testGroup)
import UnitTests.Conversions (conversionsTests)
import UnitTests.DeriveTrace (deriveTraceTests)
import UnitTests.Validator (validatorTests)

unitTests :: TestTree
unitTests = testGroup "TOA v1 Unit Tests" [validatorTests, conversionsTests, deriveTraceTests]
