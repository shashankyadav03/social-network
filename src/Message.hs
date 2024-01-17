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
    let messages = ["Hello", "Hi", "How are you?", "Good morning", "Good evening", "Good night", "Good afternoon", "Good day", "Goodbye", "Bye", "See you", "See you later", "See you soon", "Lets go out?","Lets go to the movies?", "Lets go to the park?", "Lets go to the beach?", "Lets go to the mall?", "Lets go to the restaurant?", "Lets go to the gym?", "Lets go to the party?", "Lets go to the club?", "Lets go to the bar?", "Lets go to the concert?", "Lets go to the theater?", "Lets go to the museum?", "Lets go to the zoo?", "Lets go to the aquarium?", "Lets go to the stadium?", "Lets go to the game?", "Lets go to the match?", "Lets go to the show?", "Lets go to the festival?", "Lets go to the carnival?", "Lets go to the fair?", "Lets go to the circus?", "Lets go to the opera?", "Lets go to the ballet?","Thank you","How are you?", "I am fine", "I am good", "I am great", "I am awesome", "I am amazing", "I am fantastic", "I am wonderful", "I am excellent", "I am happy", "I am sad", "I am tired", "I am sleepy", "I am hungry", "I am thirsty", "I am bored", "I am sick", "I am ill", "I am injured", "I am hurt", "I am busy", "I am free", "I am available", "I am here", "I am there", "I am near", "I am far", "I am close", "I am ready", "I am excited", "I am nervous", "I am scared", "I am afraid", "I am worried", "I am anxious", "I am calm", "I am relaxed", "I am comfortable", "I am cold", "I am hot", "I am warm", "I am cool", "I am freezing", "I am freezing cold"]
    index <- randomRIO (0, length messages - 1)
    
    return $ messages !! index

-- Function to create a random message between two users
createRandomMessage :: User -> User -> IO Message
createRandomMessage from to = do
    msgContent <- randomMessageContent
    return $ createMessage from to msgContent
