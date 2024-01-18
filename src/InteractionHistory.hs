{-# LANGUAGE OverloadedStrings #-}

-- | This module handles the interaction history of the Haskell-Based Social Network Simulation.
-- It includes functionalities to connect to a SQLite database, log interactions, and query the interaction history.
module InteractionHistory
    ( connectDb
    , initDb
    , logInteraction
    , queryUserHistory
    , queryFullHistory
    ) where

import Database.SQLite.Simple (Connection, open, execute, query, Query(..))
import Types (User(..), Message(..))
import Message (createMessage, sender, receiver, content)
import Control.Exception (bracket)
import Data.List (intercalate)
import System.Random (randomRIO)

-- | Connects to the SQLite database used for storing interaction history.
connectDb :: IO Connection
connectDb = open "interaction_history.db"

-- | Initializes the database by creating a table for messages if it does not exist.
initDb :: Connection -> IO ()
initDb conn = execute conn
    (Query "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, sender_id INTEGER, sender_username TEXT, receiver_id INTEGER, receiver_username TEXT, content TEXT)") ()

-- | Logs a new interaction (message) in the database.
--
-- @param conn The database connection.
-- @param message The message to log.
logInteraction :: Connection -> Message -> IO ()
logInteraction conn message = do
    let senderId = userId $ sender message
        receiverId = userId $ receiver message
        msgContent = content message
        senderUsername = username $ sender message
        receiverUsername = username $ receiver message
    execute conn
        (Query "INSERT INTO messages (sender_id, sender_username, receiver_id, receiver_username, content) VALUES (?, ?, ?, ?, ?)")
        (senderId, senderUsername, receiverId, receiverUsername, msgContent)

-- | Queries the interaction history of a specific user.
--
-- @param conn The database connection.
-- @param senderUserId The user ID of the sender.
-- @param receiverUserId The user ID of the receiver.
-- @return A list of messages related to the specified user.
queryUserHistory :: Connection -> Int -> Int -> IO [Message]
queryUserHistory conn senderUserId receiverUserId = do
    rows <- query conn
        (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages WHERE sender_id = ? OR receiver_id = ?")
        (senderUserId, receiverUserId)
    return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows

-- | Queries the entire interaction history.
--
-- @param conn The database connection.
-- @return A list of all messages in the interaction history.
queryFullHistory :: Connection -> IO [Message]
queryFullHistory conn = do
    rows <- query conn (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages") ()
    return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows

-- {-# LANGUAGE OverloadedStrings #-}

-- module InteractionHistory where

-- import Database.SQLite.Simple (Connection, open, execute, query, Query(..))
-- import Control.Exception (bracket)
-- import Message (Message, createMessage, sender, receiver, content)
-- import User (User(..), userId, username)
-- import Data.List (intercalate)
-- import System.Random (randomRIO)

-- -- A function to connect to the SQLite database
-- connectDb :: IO Connection
-- connectDb = open "interaction_history.db"

-- -- Function to initialize the database 
-- initDb :: Connection -> IO ()
-- initDb conn = execute conn
--     (Query "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, sender_id INTEGER, sender_username TEXT , receiver_id INTEGER, receiver_username TEXT, content TEXT)") ()

-- -- Function to log a new interaction
-- logInteraction :: Connection -> Message -> IO ()
-- logInteraction conn message = do
--     let senderId = userId $ sender message
--         receiverId = userId $ receiver message
--         msgContent = content message
--         senderUsername = username $ sender message
--         receiverUsername = username $ receiver message
--     execute conn
--         (Query "INSERT INTO messages (sender_id, sender_username, receiver_id, receiver_username, content) VALUES (?, ?, ?, ?, ?)")
--         (senderId, senderUsername, receiverId, receiverUsername, msgContent)

-- -- Function to query the interaction history of a specific user
-- queryUserHistory :: Connection -> Int -> Int -> IO [Message]
-- queryUserHistory conn sender_userId receiver_userId = do
--     rows <- query conn
--         (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages WHERE sender_id = ? OR receiver_id = ?")
--         (sender_userId, receiver_userId)
--     return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows

-- -- Function to query the entire interaction history
-- queryFullHistory :: Connection -> IO [Message]
-- queryFullHistory conn = do
--     rows <- query conn (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages") ()
--     return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows
