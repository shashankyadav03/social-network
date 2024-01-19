{-# LANGUAGE OverloadedStrings #-}

module MessageTest where

import Test.HUnit
import Types
import Message 

-- Test data setup
user1 :: User
user1 = User 1 "Alice"

user2 :: User
user2 = User 2 "Bob"

-- Test for createMessage function
testCreateMessage :: Test
testCreateMessage = TestCase $ do
    let content = "Hello, Bob!"
    let message = createMessage user1 user2 content

    assertEqual "Sender should be Alice" (userId (sender message)) 1
    assertEqual "Receiver should be Bob" (userId (receiver message)) 2
    
-- Test for createRandomMessage function
-- This test can be tricky as the result is random.
-- You may test certain properties of the message instead of exact content.
testCreateRandomMessage :: Test
testCreateRandomMessage = TestCase $ do
    message <- createRandomMessage user1 user2

    assertEqual "Sender should be Alice" (userId (sender message)) 1
    assertEqual "Receiver should be Bob" (userId (receiver message)) 2
    -- Add more assertions if necessary. 

-- Test list
tests :: Test
tests = TestList [TestLabel "testCreateMessage" testCreateMessage,
                  TestLabel "testCreateRandomMessage" testCreateRandomMessage]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
