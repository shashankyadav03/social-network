import Test.HUnit
import qualified InteractionHistoryTest
import qualified MessageTest
import qualified UserTest

-- Combine all test cases from different modules into one test list
allTests :: Test
allTests = TestList [ InteractionHistoryTest.tests
                    , MessageTest.tests
                    , UserTest.tests
                    ]

-- Main function to run all tests
main :: IO Counts
main = runTestTT allTests
