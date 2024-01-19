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