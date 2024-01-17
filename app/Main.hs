{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad (forever, forM_, replicateM)
import System.Random (randomRIO)

import User (createRandomUser, getUsername, userId, User, username)
import Message (createRandomMessage, Message, content, receiver, sender)
import ConcurrentUtils (randomDelayAction)

-- User behavior simulation in a separate thread
userThread ::  [User] -> User -> IO ()
userThread users currentUser = forever $ do
    -- Randomly select a user to send a message to
    recipientIndex <- randomRIO (0, length users - 1)
    let recipient = users !! recipientIndex

    -- Create a random message
    message <- createRandomMessage currentUser recipient

    let msgContent = content message
    let msgSender = sender message
    let msgReceiver = receiver message

    print $ "Message from " ++ getUsername msgSender ++ " to " ++ getUsername msgReceiver ++ ": " ++ msgContent
    -- Random delay before next action
    randomDelayAction 1 5 (return ())

main :: IO ()
main = do
    putStrLn "Inside main..."

    -- Create 10 random users
    users <- replicateM 10 createRandomUser

    -- Spawn user threads
    mapM_ (forkIO . userThread users) users

    -- Wait for a certain condition or user input to terminate
    putStrLn "Press any key to terminate..."
    _ <- getLine

    putStrLn "Demostration done"
   