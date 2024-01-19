-- | This module provides utility functions for handling concurrency and random delays
-- in the Haskell-Based Social Network Simulation.
module ConcurrentUtils
    ( randomDelayAction
    , modifySharedVar
    , readSharedVar
    ) where

import Control.Concurrent
import System.Random (randomRIO)

-- | Performs an action with a random delay.
--
-- The delay is chosen randomly within the specified range of seconds. This function
-- is particularly useful for simulating random user activities in a concurrent environment.
--
-- @param minSec Minimum delay in seconds.
-- @param maxSec Maximum delay in seconds.
-- @param action The IO action to be performed after the delay.
randomDelayAction :: Int -> Int -> IO () -> IO ()
randomDelayAction minSec maxSec action = do
    delaySec <- randomRIO (minSec, maxSec)
    threadDelay (delaySec * 1000000) -- Convert seconds to microseconds
    action

-- | Safely modifies a shared variable (MVar).
--
-- This function ensures thread-safe modification of an MVar. It takes an MVar and a function,
-- applies the function to the MVar, and then safely writes the result back to the MVar.
--
-- @param var The MVar to modify.
-- @param action The modification function.
modifySharedVar :: MVar a -> (a -> IO a) -> IO ()
modifySharedVar var action = modifyMVar_ var action

-- | Reads a shared variable (MVar) safely.
--
-- This function allows for a thread-safe way to read the value of an MVar without
-- the risk of deadlocks or race conditions.
--
-- @param var The MVar to be read.
-- @return The value contained in the MVar.
readSharedVar :: MVar a -> IO a
readSharedVar = readMVar
