-- | Module for handling messages in the Haskell-Based Social Network Simulation.
-- This module defines the Message data type and provides functionalities
-- for creating and managing messages.
module Message
    ( Message(..)
    , createMessage
    , randomMessageContent
    , createRandomMessage
    ) where

import Types (User, Message(..))  -- Import User and Message from Types
import System.Random (randomRIO)

-- | Creates a new message with specified sender, receiver, and content.
-- 
-- @param from The sender of the message.
-- @param to The receiver of the message.
-- @param msgContent The content of the message.
-- @return The constructed Message.
createMessage :: User -> User -> String -> Message
createMessage from to msgContent = Message from to msgContent

-- | Generates a random message content from a predefined list of phrases.
--
-- @return A randomly selected message content.
randomMessageContent :: IO String
randomMessageContent = do
    let messages = ["Hello, how's your day going?", "Hi! Long time no see.", "How are you feeling today?", "Good morning, hope you slept well!", "Good evening, did you have a good day?", "Wishing you a peaceful night.", "Enjoying this sunny afternoon!", "What a lovely day, isn't it?", "Take care, talk to you soon.", "Bye! Catch up later?", "See you tomorrow at our regular spot.", "Can't wait to see you this weekend.", "How about a coffee catch-up soon?", "Do you want to join me for a movie tonight?", "Let's have a picnic in the park this Saturday.", "Fancy a trip to the beach this Sunday?", "Shall we go shopping at the mall this evening?", "I found a new restaurant downtown. Let's try it out!", "Are you up for a workout at the gym later?", "There's a cool party happening this Friday. Interested?", "Let's go clubbing this weekend!", "How about a drink at the bar after work?", "I have tickets for a concert next week. Wanna come?", "I'm planning to go to the theater this month. Join me?", "There's an interesting exhibition at the museum. Let's visit!", "Have you ever been to the zoo here? It's quite nice.", "Let's check out the new aquarium in town.", "Are you interested in going to the game next week?", "Thank you for being there for me.", "I've been feeling really upbeat lately!", "Honestly, I'm feeling a bit down. Could use some cheering up.", "I'm quite tired today, had a long week.", "I could really go for a nap right now.", "I'm starving, let's grab some food!", "Feeling dehydrated after that workout!", "Do you ever get that feeling of just wanting to do nothing?", "Haven't been feeling the best, might take a day off.", "I've got so much work to do, feeling a bit overwhelmed.", "I'm totally free this weekend, let's plan something fun!", "Just here chilling at home. What about you?", "I'm really looking forward to our trip next month.", "Got a bit nervous about the presentation tomorrow.", "I've been anxious about the results, hope it goes well.", "Trying to stay calm before the big day.", "Let's get cozy and watch a movie, it's freezing outside!", "It's so warm today, perfect for a swim."]
    index <- randomRIO (0, length messages - 1)
    return $ messages !! index

-- | Creates a random message between two users.
--
-- This function selects a random content and constructs a message from
-- one user to another.
-- 
-- @param from The sender of the message.
-- @param to The receiver of the message.
-- @return A randomly generated Message in IO context.
createRandomMessage :: User -> User -> IO Message
createRandomMessage from to = do
    msgContent <- randomMessageContent
    return $ createMessage from to msgContent

-- module Message where

-- import User (User)
-- import System.Random (randomRIO)

-- -- Define the Message data type
-- data Message = Message {
--     sender :: User,
--     receiver :: User,
--     content :: String
-- } deriving (Eq, Show)

-- -- Function to create a new message
-- createMessage :: User -> User -> String -> Message
-- createMessage from to msgContent = Message from to msgContent

-- -- Function to generate a random message content
-- randomMessageContent :: IO String
-- randomMessageContent = do
--     -- Generate a random message content
--     -- This is a placeholder for simplicity; you can replace it with more complex logic
--     let messages = ["Hello", "Hi", "How are you?", "Good morning", "Good evening", "Good night", "Good afternoon", "Good day", "Goodbye", "Bye", "See you", "See you later", "See you soon", "Lets go out?","Lets go to the movies?", "Lets go to the park?", "Lets go to the beach?", "Lets go to the mall?", "Lets go to the restaurant?", "Lets go to the gym?", "Lets go to the party?", "Lets go to the club?", "Lets go to the bar?", "Lets go to the concert?", "Lets go to the theater?", "Lets go to the museum?", "Lets go to the zoo?", "Lets go to the aquarium?", "Lets go to the stadium?", "Lets go to the game?", "Lets go to the match?", "Lets go to the show?", "Lets go to the festival?", "Lets go to the carnival?", "Lets go to the fair?", "Lets go to the circus?", "Lets go to the opera?", "Lets go to the ballet?","Thank you","How are you?", "I am fine", "I am good", "I am great", "I am awesome", "I am amazing", "I am fantastic", "I am wonderful", "I am excellent", "I am happy", "I am sad", "I am tired", "I am sleepy", "I am hungry", "I am thirsty", "I am bored", "I am sick", "I am ill", "I am injured", "I am hurt", "I am busy", "I am free", "I am available", "I am here", "I am there", "I am near", "I am far", "I am close", "I am ready", "I am excited", "I am nervous", "I am scared", "I am afraid", "I am worried", "I am anxious", "I am calm", "I am relaxed", "I am comfortable", "I am cold", "I am hot", "I am warm", "I am cool", "I am freezing", "I am freezing cold"]
--     index <- randomRIO (0, length messages - 1)
    
--     return $ messages !! index

-- -- Function to create a random message between two users
-- createRandomMessage :: User -> User -> IO Message
-- createRandomMessage from to = do
--     msgContent <- randomMessageContent
--     return $ createMessage from to msgContent
