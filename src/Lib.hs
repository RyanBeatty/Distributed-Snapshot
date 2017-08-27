{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

class ProcessId a

-- Wrapper around Integer type.
newtype Count = Count { getCount :: Integer }
  deriving (Show, Eq)

data Color = White | Red
  deriving (Show, Eq)

class Channel a

-- The state of a Process.
data ProcessState a b where
        ProcessState  :: (ProcessId a, Channel b) =>
                         { selfId        :: a     -- The proccess id of the process currently running.
                         , selfColor     :: Color -- The current color of the process.
                         , opCount       :: Count -- The number of operations that have been executed on this process.
                         , snapshotCount :: Count -- The number of snapshots that this process has been involved in.
                         , inChannels    :: [b]   -- All incoming channels to this process.
                         , outchannels   :: [b]   -- All outgoing channels from this process.
                         } -> ProcessState a b

someFunc :: IO ()
someFunc = putStrLn "someFunc"
