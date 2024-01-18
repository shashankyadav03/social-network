{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Database.SQLite.Simple
import Types
import InteractionHistory
import User
import Message

-- Sample data for testing
testUser1 :: User
testUser1 = User 1 "Alice"

testUser2 :: User
testUser2 = User 2 "Bob"

testMessage :: Message
testMessage = Message testUser1 testUser2 "Hello, Bob!"

-- Test Cases
testInitDb :: Test
testInitDb = TestCase $ do
    conn <- open ":memory:"
    initDb conn
    -- Add assertions to check if the table was created successfully.
    close conn

testLogInteraction :: Test
testLogInteraction = TestCase $ do
    conn <- open ":memory:"
    initDb conn
    logInteraction conn testMessage
    -- Add assertions to check if the message was logged correctly.
    close conn

testQueryFullHistory :: Test
testQueryFullHistory = TestCase $ do
    conn <- open ":memory:"
    initDb conn
    logInteraction conn testMessage
    history <- queryFullHistory conn
    -- Add assertions to check if the history query returns the correct data.
    close conn

-- Test List
tests :: Test
tests = TestList [TestLabel "testInitDb" testInitDb,
                  TestLabel "testLogInteraction" testLogInteraction,
                  TestLabel "testQueryFullHistory" testQueryFullHistory]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
