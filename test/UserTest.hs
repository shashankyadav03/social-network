{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Types
import User

-- Test for createUser function
testCreateUser :: Test
testCreateUser = TestCase $ do
    let id = 100
    let name = "TestUser"
    let user = createUser id name

    assertEqual "User ID should be 100" (userId user) 100
    assertEqual "Username should be TestUser" (username user) "TestUser"

-- Test for randomUsername function
-- Testing random functions can be challenging; focus on testing the format or type
testRandomUsername :: Test
testRandomUsername = TestCase $ do
    username <- randomUsername
    assertBool "Username should not be empty" (not (null username))
    -- More assertions can be added based on how randomUsername is implemented

-- Test for createRandomUser function
-- As the result is random, you might want to check the properties of the user created
testCreateRandomUser :: Test
testCreateRandomUser = TestCase $ do
    user <- createRandomUser
    assertBool "User ID should be positive" ((userId user) > 0)
    assertBool "Username should not be empty" (not (null (username user)))
    -- More assertions can be added based on your implementation

-- Test list
tests :: Test
tests = TestList [TestLabel "testCreateUser" testCreateUser,
                  TestLabel "testRandomUsername" testRandomUsername,
                  TestLabel "testCreateRandomUser" testCreateRandomUser]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
