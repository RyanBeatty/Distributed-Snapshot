{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

-- Interface for the 'Color' abstraction that Spezialleti and Kearns
-- describes in their paper. Here the 'White' process id acts as a sentinal
-- process id.
class ProcessId a where
        -- Is the process id the 'White' process id.
        isWhiteId :: a -> Bool

-- Wrapper around Integer type.
newtype Count = Count { getCount :: Integer }
  deriving (Show, Eq)

class Channel a

-- The state of a Process.
data ProcessState a b where
        ProcessState  :: (ProcessId a, Channel b) =>
                         { selfId        :: a     -- The proccess id of the process currently running.
                         , localColor    :: a     -- The current color of the process.
                         , opCount       :: Count -- The number of operations that have been executed on this process.
                         , snapshotCount :: Count -- The number of snapshots that this process has been involved in.
                         , inChannels    :: [b]   -- All incoming channels to this process.
                         , outchannels   :: [b]   -- All outgoing channels from this process.
                         } -> ProcessState a b

someFunc :: IO ()
someFunc = putStrLn "someFunc"
