{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
-- | Module for handling user-related functionalities in the Haskell-Based Social Network Simulation.
-- This module provides functionalities for creating and managing users,
-- including generating random users for the simulation.
module User
    ( createUser
    , randomUsername
    , createRandomUser
    , getUsername
    ) where

import System.Random (randomRIO)
import Types (User(..))  -- Import User from Types

-- | Creates a new user with a specified ID and username.
--
-- @param id The unique identifier for the new user.
-- @param name The name for the new user.
-- @return A new User object.
createUser :: Int -> String -> User
createUser id name = User id name

-- | Generates a random username from a predefined list of names.
--
-- @return A randomly selected username.
randomUsername :: IO String
randomUsername = do
    let names = ["John", "Jane", "Alice", "Bob", "Eve", "Oscar", "Charlie", "Carol", "Dave", "Erin", "Frank", "Grace", "Heidi", "Ivan", "Judy", "Mallory", "Peggy", "Sybil", "Trudy", "Victor", "Walter", "Wendy"]
    index <- randomRIO (0, length names - 1)
    num <- randomRIO (1, 10000 :: Int)
    return $ names !! index ++ show num

-- | Creates a random user with a unique ID and a randomly generated username.
--
-- @return A randomly generated User in IO context.
createRandomUser :: IO User
createRandomUser = do
    randomId <- randomRIO (1, 10000) -- Generate a random user ID
    name <- randomUsername
    return $ createUser randomId name

-- | Retrieves the username of a given User.
--
-- @param user The User object.
-- @return The username of the User.
getUsername :: User -> String
getUsername = username

-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
-- module User where

-- import System.Random (randomRIO)
-- import Control.Monad (replicateM)

-- -- Define the User data type
-- data User = User {
--     userId :: Int,
--     username :: String
-- } deriving (Eq, Show)

-- -- Function to create a new user with a unique ID and a username
-- createUser :: Int -> String -> User
-- createUser id name = User id name

-- -- Function to generate a random username
-- randomUsername :: IO String
-- randomUsername = do
--     -- Generate a random sequence of characters or choose from a list of names
--     -- This is a placeholder for simplicity; you can replace it with more complex logic
--     let names = ["John", "Jane", "Alice", "Bob", "Eve", "Oscar", "Charlie", "Carol", "Dave", "Erin", "Frank", "Grace", "Heidi", "Ivan", "Judy", "Mallory", "Peggy", "Sybil", "Trudy", "Victor", "Walter", "Wendy"]
--     index <- randomRIO (0, length names - 1)
--     -- Example: Generate a username like "User1234"
--     num <- randomRIO (1, 10000 :: Int) -- Generate a random user ID
--     return $ names !! index 

-- -- Function to create a random user
-- createRandomUser :: IO User
-- createRandomUser = do
--     randomId <- randomRIO (1, 10000) -- Generate a random user ID
--     name <- randomUsername
--     return $ createUser randomId name

-- -- Example function to get a username from a User
-- getUsername :: User -> String
-- getUsername = username
