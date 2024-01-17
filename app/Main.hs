{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad (forever, forM_, replicateM)
import System.Random (randomRIO)
import Database.SQLite.Simple (Connection, close)

import User (createRandomUser, getUsername, userId, User, username)
import Message (createRandomMessage)
import ConcurrentUtils (randomDelayAction)
import InteractionHistory (connectDb, initDb, logInteraction, queryUserHistory, queryFullHistory)

-- User behavior simulation in a separate thread
userThread :: Connection -> [User] -> User -> IO ()
userThread conn users currentUser = forever $ do
    -- Randomly select a user to send a message to
    recipientIndex <- randomRIO (0, length users - 1)
    let recipient = users !! recipientIndex

    -- Create a random message
    message <- createRandomMessage currentUser recipient

    -- Log the interaction in the database
    logInteraction conn message

    -- Random delay before next action
    randomDelayAction 1 5 (return ())

main :: IO ()
main = do
    putStrLn "Inside main..."
    -- Connect to the SQLite database
    conn <- connectDb


    -- -- Initialize the database
    initDb conn

    -- Create 10 random users
    users <- replicateM 10 createRandomUser

    -- Spawn user threads
    mapM_ (forkIO . userThread conn users) users

    -- Wait for a certain condition or user input to terminate
    putStrLn "Press any key to terminate..."
    _ <- getLine

    -- Query and print the interaction history
    history <- queryFullHistory conn
    putStrLn "Final Interaction History:"
    mapM_ print history

    -- Close the database connection
    close conn

-- module Main where

-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Monad (forever, forM_)
-- import System.Random (randomRIO)
-- import qualified Data.Map as Map
-- import User (User, createUser, username)
-- import Message (Message, createRandomMessage)
-- import ConcurrentUtils (sendMessage, messageCount)

-- -- | Main entry point of the application.
-- main :: IO ()
-- main = do
--     -- Initialize application
--     putStrLn "Initializing Social Network Simulation"

--     -- Create ten user instances
--     let users = map createUser ["user1", "user2", "user3", "user4", "user5",
--                                 "user6", "user7", "user8", "user9", "user10"]

--     -- Spawn ten threads, one for each user
--     forM_ users $ \user -> forkIO $ userThread user users

--     -- Wait for the simulation to complete
--     -- This can be a condition to check if 100 messages have been sent
--     -- Not implemented here for simplicity
--     waitForCompletion

-- -- | Represents the behavior of each user in a separate thread.
-- userThread :: User -> [User] -> IO ()
-- userThread user users = forever $ do
--     -- Random delay to simulate random intervals
--     delay <- randomRIO (1, 10) -- Random delay between 1 to 10 seconds
--     threadDelay (delay * 1000000)

--     -- Select a random user
--     recipient <- selectRandomUser user users

--     -- Send a random message to the selected user
--     message <- createRandomMessage user recipient
--     sendMessage message

--     -- Optionally: Check if 100 messages have been sent and then terminate
--     -- This part of logic needs to be implemented

-- -- | Selects a random user from the list, excluding the sender.
-- selectRandomUser :: User -> [User] -> IO User
-- selectRandomUser sender users = do
--     let possibleRecipients = filter (\u -> username u /= username sender) users
--     randomIndex <- randomRIO (0, length possibleRecipients - 1)
--     return (possibleRecipients !! randomIndex)

-- -- | Waits for the simulation to complete. This function should implement
-- -- | the logic to check if 100 messages have been sent.
-- waitForCompletion :: IO ()
-- waitForCompletion = do
--     -- Implement completion logic here
--     putStrLn "Simulation completed"

-- -- | A utility function to log messages or actions. Extendable for more complex logging.
-- logInfo :: String -> IO ()
-- logInfo msg = putStrLn $ "Log: " ++ msg

-- -- Necessary imports

-- -- Assuming the existence of createUser, createRandomMessage, logInteraction, queryHistory functions
-- -- in their respective modules.

-- -- userThread :: [User] -> User -> MVar [Message] -> IO ()
-- -- userThread users currentUser historyVar = forever $ do
--     -- Pseudocode for userThread function:
--     -- 1. Randomly select a recipient user.
--     -- 2. Generate a random message.
--     -- 3. Log the interaction.
--     -- 4. Sleep for a random interval.

-- -- main :: IO ()
-- -- main = do
--     -- Pseudocode for the main function:
--     -- 1. Initialize users.
--     -- 2. Create a shared MVar for interaction history.
--     -- 3. Spawn user threads.
--     -- 4. Wait for a termination condition.
--     -- 5. Output interaction history.
