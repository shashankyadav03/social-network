module Message where

import User (User)
import System.Random (randomRIO)

-- Define the Message data type
data Message = Message {
    sender :: User,
    receiver :: User,
    content :: String
} deriving (Eq, Show)

-- Function to create a new message
createMessage :: User -> User -> String -> Message
createMessage from to msgContent = Message from to msgContent

-- Function to generate a random message content
randomMessageContent :: IO String
randomMessageContent = do
    -- Generate a random message content
    -- This is a placeholder for simplicity; you can replace it with more complex logic
    let messages = ["Hello", "How are you?", "Good day", "Hi there!", "Greetings"]
    index <- randomRIO (0, length messages - 1)
    let message = messages !! index
    -- Print the message for logging or debugging
    putStrLn $ "Generated message: " ++ message
    return $ messages !! index

-- Function to create a random message between two users
createRandomMessage :: User -> User -> IO Message
createRandomMessage from to = do
    msgContent <- randomMessageContent
    return $ createMessage from to msgContent
