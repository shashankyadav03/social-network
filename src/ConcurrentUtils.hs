module ConcurrentUtils where

import Control.Concurrent
import System.Random (randomRIO)

-- Function to perform an action with a random delay
-- The delay is in seconds, and the range is specified by the arguments
randomDelayAction :: Int -> Int -> IO () -> IO ()
randomDelayAction minSec maxSec action = do
    delaySec <- randomRIO (minSec, maxSec)
    threadDelay (delaySec * 1000000) -- Convert seconds to microseconds
    action

-- Function to safely modify a shared variable (MVar)
modifySharedVar :: MVar a -> (a -> IO a) -> IO ()
modifySharedVar var action = modifyMVar_ var action

-- Function to read a shared variable (MVar)
readSharedVar :: MVar a -> IO a
readSharedVar = readMVar
