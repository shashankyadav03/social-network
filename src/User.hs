{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module User where

import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Define the User data type
data User = User {
    userId :: Int,
    username :: String
} deriving (Eq, Show)

-- Function to create a new user with a unique ID and a username
createUser :: Int -> String -> User
createUser id name = User id name

-- Function to generate a random username
randomUsername :: IO String
randomUsername = do
    -- Generate a random sequence of characters or choose from a list of names
    -- This is a placeholder for simplicity; you can replace it with more complex logic
    let names = ["John", "Jane", "Alice", "Bob", "Eve", "Oscar", "Charlie"]
    index <- randomRIO (0, length names - 1)
    -- Example: Generate a username like "User1234"
    num <- randomRIO (1, 10000 :: Int) -- Generate a random user ID
    return $ names !! index ++ show num

-- Function to create a random user
createRandomUser :: IO User
createRandomUser = do
    randomId <- randomRIO (1, 10000) -- Generate a random user ID
    name <- randomUsername
    return $ createUser randomId name

-- Example function to get a username from a User
getUsername :: User -> String
getUsername = username
