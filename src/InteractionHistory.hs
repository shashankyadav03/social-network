{-# LANGUAGE OverloadedStrings #-}

module InteractionHistory where

import Database.SQLite.Simple (Connection, open, execute, query, Query(..))
import Control.Exception (bracket)
import Message (Message, createMessage, sender, receiver, content)
import User (User(..), userId, username)
import Data.List (intercalate)
import System.Random (randomRIO)

-- A function to connect to the SQLite database
connectDb :: IO Connection
connectDb = open "interaction_history.db"

-- Function to initialize the database
initDb :: Connection -> IO ()
initDb conn = execute conn
    (Query "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, sender_id INTEGER, sender_username TEXT , receiver_id INTEGER, receiver_username TEXT, content TEXT)") ()

-- Function to log a new interaction
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

-- Function to query the interaction history of a specific user
queryUserHistory :: Connection -> Int -> IO [Message]
queryUserHistory conn userId = do
    rows <- query conn
        (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages WHERE sender_id = ? OR receiver_id = ?")
        (userId, userId)
    return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows

-- Function to query the entire interaction history
queryFullHistory :: Connection -> IO [Message]
queryFullHistory conn = do
    rows <- query conn (Query "SELECT sender_id, sender_username, receiver_id, receiver_username, content FROM messages") ()
    return $ map (\(senderId, senderUsername, receiverId, receiverUsername, content) -> createMessage (User senderId senderUsername) (User receiverId receiverUsername) content) rows
