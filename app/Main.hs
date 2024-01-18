{-# LANGUAGE OverloadedStrings #-}

-- | Main module for the Haskell-Based Social Network Simulation.
-- This module initializes the simulation, manages user threads,
-- and handles the interaction history.
module Main where

import Control.Concurrent
import Control.Monad (forever, forM_, replicateM)
import System.Random (randomRIO)
import Database.SQLite.Simple (Connection, close)

import Types (User, Message)  -- Import User and Message from Types
import User (createRandomUser, getUsername)
import Message (createRandomMessage, content, receiver, sender)
import ConcurrentUtils (randomDelayAction)
import InteractionHistory (connectDb, initDb, logInteraction, queryFullHistory)

-- | Simulates user behavior in a separate thread.
-- Each user sends messages to randomly selected recipients at random intervals.
userThread :: Connection -> [User] -> User -> IO ()
userThread conn users currentUser = forever $ do
    recipientIndex <- randomRIO (0, length users - 1)
    let recipient = users !! recipientIndex

    message <- createRandomMessage currentUser recipient

    print $ "Message from " ++ getUsername (sender message) ++ " to " ++
            getUsername (receiver message) ++ ": " ++ content message

    logInteraction conn message
    randomDelayAction 1 5 (return ())

-- | The entry point for the simulation.
-- Initializes the database, spawns user threads, and manages the simulation lifecycle.
main :: IO ()
main = do
    putStrLn "Starting Social Network Simulation"
    conn <- connectDb
    initDb conn
    threadDelay (3 * 1000000)  -- Initial delay before starting the simulation

    users <- replicateM 10 createRandomUser
    mapM_ (forkIO . userThread conn users) users

    -- terminate after 100 messages are sent
    forM_ [1..100] $ \_ -> do
        threadDelay (1 * 1000000)
    

    putStrLn "Terminating Simulation..."
    -- Additional steps can be added here to gracefully terminate all threads if needed
    putStrLn "Terminated"

    -- Count number of messages send by each user
    forM_ users $ \user -> do
        history <- queryFullHistory conn
        let userHistory = filter (\msg -> sender msg == user) history
        putStrLn $ "User " ++ getUsername user ++ " sent " ++ show (length userHistory) ++ " messages"


    -- Count number of messages receive by each user
    forM_ users $ \user -> do
        history <- queryFullHistory conn
        let userHistory = filter (\msg -> sender msg == user || receiver msg == user) history
        putStrLn $ "User " ++ getUsername user ++ " received " ++ show (length userHistory) ++ " messages"

    history <- queryFullHistory conn
    putStrLn "Final Interaction History:"
    mapM_ print history

    putStrLn "Simulation completed"
    close conn

-- {-# LANGUAGE OverloadedStrings #-}

-- import Control.Concurrent
-- import Control.Monad (forever,forM_,replicateM)
-- import System.Random (randomRIO)
-- import Database.SQLite.Simple (Connection, close)

-- import User (createRandomUser, getUsername, userId, User, username)
-- import Message (createRandomMessage, content, Message, receiver, sender)
-- import ConcurrentUtils (randomDelayAction)
-- import InteractionHistory (connectDb, initDb, logInteraction, queryUserHistory, queryFullHistory)
-- import Control.Concurrent (threadDelay)

-- -- User behavior simulation in a separate thread
-- userThread :: Connection -> [User] -> User -> IO ()
-- userThread conn users currentUser = forever $ do
--     -- Randomly select a user to send a message to
--     recipientIndex <- randomRIO (0, length users - 1)
--     let recipient = users !! recipientIndex

--     -- Create a random message
--     message <- createRandomMessage currentUser recipient

--     let msgContent = content message
--     let msgSender = sender message
--     let msgReceiver = receiver message

--     print $ "Message from " ++ getUsername msgSender ++ " to " ++ getUsername msgReceiver ++ ": " ++ msgContent

--     -- Log the interaction in the database
--     logInteraction conn message

--     -- Random delay before next action
--     randomDelayAction 1 5 (return ())

-- main :: IO ()
-- main = do
--     putStrLn "Starting Social Network Simulation"
--     -- Connect to the SQLite database
--     conn <- connectDb

--     -- -- Initialize the database
--     initDb conn

--     putStrLn "Press any key and enter to terminate..."
--     -- Wait for 3 sec
--     threadDelay (3 * 1000000)

--     -- Create 10 random users
--     users <- replicateM 10 createRandomUser

--     -- Spawn user threads
--     mapM_ (forkIO . userThread conn users) users

--     -- Wait for a certain condition or user input to terminate the simulation
--     _ <- getLine

--     putStrLn "Terminating..."
--     -- terminate all threads
--     putStrLn "Terminated"

--     -- Query and print the interaction history
--     history <- queryFullHistory conn
--     putStrLn "Final Interaction History:"
--     mapM_ print history

--     putStrLn "Simulation completed"
--     -- Close the database connection
--     close conn
    
    

