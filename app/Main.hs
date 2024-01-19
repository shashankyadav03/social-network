{-# LANGUAGE OverloadedStrings #-}
-- | Main module for the Haskell-Based Social Network Simulation.
-- Manages the simulation process, including database interactions, user thread management, and message handling.
module Main where

import Control.Concurrent
import Control.Monad (forever, forM_, replicateM, when)
import System.Random (randomRIO)
import Database.SQLite.Simple (Connection, close)

import Types (User, Message)
import User (createRandomUser, getUsername)
import Message (createRandomMessage, sender, receiver, content)
import ConcurrentUtils (randomDelayAction)
import InteractionHistory (connectDb, initDb, logInteraction, queryFullHistory)

-- | Simulates the behavior of a user in a separate thread.
-- Users randomly select other users and send messages at random intervals.
-- The simulation for a user stops when a total of 100 messages have been sent.
--
-- @param conn The database connection.
-- @param msgCount A shared variable tracking the total number of messages sent.
-- @param users A list of all users in the simulation.
-- @param currentUser The current user running in this thread.
userThread :: Connection -> MVar Int -> [User] -> User -> IO ()
userThread conn msgCount users currentUser = forever $ do
    currentCount <- readMVar msgCount
    when (currentCount >= 100) $ return ()

    recipientIndex <- randomRIO (0, length users - 1)
    let recipient = users !! recipientIndex

    message <- createRandomMessage currentUser recipient
    print $ "Message from " ++ getUsername (sender message) ++ " to " ++
            getUsername (receiver message) ++ ": " ++ content message

    logInteraction conn message
    modifyMVar_ msgCount $ \count -> return (count + 1)

    randomDelayAction 1 5 (return ())

-- | The entry point for the simulation.
-- Initializes the simulation environment and starts user threads.
main :: IO ()
main = do
    putStrLn "Starting Social Network Simulation\n"
    conn <- connectDb
    initDb conn
    threadDelay (2 * 1000000) 

    users <- replicateM 10 createRandomUser
    msgCount <- newMVar 0

    mapM_ (forkIO . userThread conn msgCount users) users

    -- Wait until 100 messages are sent
    waitForMessages msgCount

    putStrLn "Terminating Simulation...\n"
    
    putStrLn "Terminated\n"

    history <- queryFullHistory conn
    putStrLn "\nFinal Interaction History:\n"
    mapM_ print history

    -- Combined send and receive report
    putStrLn "\nMessage Report \n"
    forM_ users $ \user -> do
        let sentMessages = filter (\msg -> sender msg == user) history
        let receivedMessages = filter (\msg -> receiver msg == user) history
        putStrLn $ "User " ++ getUsername user ++ " sent " ++ show (length sentMessages) ++ " messages"
        putStrLn $ "User " ++ getUsername user ++ " received " ++ show (length receivedMessages) ++ " messages"
        putStrLn ""

    putStrLn "\nSimulation completed\n"
    close conn

-- | Waits for the simulation to reach a total of 100 sent messages before continuing.
--
-- @param msgCount A shared variable tracking the total number of messages sent.
waitForMessages :: MVar Int -> IO ()
waitForMessages msgCount = do
    currentCount <- readMVar msgCount
    when (currentCount < 100) $ do
        threadDelay (1 * 1000000)
        waitForMessages msgCount
