{-# LANGUAGE OverloadedStrings #-}

module InteractionHistory where

import Database.SQLite.Simple (Connection, open, execute, query, Query(..))
import Control.Exception (bracket)
import Message (Message, createMessage, sender, receiver, content)
import User (User(..), userId)
import Data.List (intercalate)
import System.Random (randomRIO)

-- A function to connect to the SQLite database
connectDb :: IO Connection
connectDb = open "interaction_history.db"

-- Function to initialize the database
initDb :: Connection -> IO ()
initDb conn = execute conn
    (Query "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, sender_id INTEGER, receiver_id INTEGER, content TEXT)") ()

-- Function to log a new interaction
logInteraction :: Connection -> Message -> IO ()
logInteraction conn message = do
    let senderId = userId $ sender message
        receiverId = userId $ receiver message
        msgContent = content message
    execute conn
        (Query "INSERT INTO messages (sender_id, receiver_id, content) VALUES (?, ?, ?)")
        (senderId, receiverId, msgContent)

-- Function to query the interaction history of a specific user
queryUserHistory :: Connection -> Int -> IO [Message]
queryUserHistory conn userId = do
    rows <- query conn
        (Query "SELECT sender_id, receiver_id, content FROM messages WHERE sender_id = ? OR receiver_id = ?")
        (userId, userId)
    return $ map (\(senderId, receiverId, content) -> createMessage (User senderId "") (User receiverId "") content) rows

-- Function to query the entire interaction history
queryFullHistory :: Connection -> IO [Message]
queryFullHistory conn = do
    rows <- query conn (Query "SELECT sender_id, receiver_id, content FROM messages") ()
    return $ map (\(senderId, receiverId, content) -> createMessage (User senderId "") (User receiverId "") content) rows
