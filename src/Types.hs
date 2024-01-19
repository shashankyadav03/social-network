{-# LANGUAGE OverloadedStrings #-}

-- | Module for defining core data types used in the Haskell-Based Social Network Simulation.
-- This includes types for representing users and messages in the network.
module Types
    ( User(..)
    , Message(..)
    ) where

-- | Represents a user in the social network simulation.
-- Each user is characterized by a unique identifier and a username.
data User = User
    { userId :: Int      -- ^ The unique identifier of the user.
    , username :: String -- ^ The username of the user.
    } deriving (Eq, Show)

-- | Represents a message in the social network simulation.
-- A message consists of a sender, a receiver, and the message content.
data Message = Message
    { sender   :: User   -- ^ The user who sends the message.
    , receiver :: User   -- ^ The user who receives the message.
    , content  :: String -- ^ The content of the message.
    } deriving (Eq, Show)
