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

type Channel = ()

-- The state of a Process.
data ProcessState a where
        ProcessState  :: (ProcessId a) =>
                         { selfId        :: a         -- The proccess id of the process currently running.
                         , selfColor     :: Color     -- The current color of the process.
                         , opCount       :: Count     -- The number of operations that have been executed on this process.
                         , snapshotCount :: Count     -- The number of snapshots that this process has been involved in.
                         , inChannels    :: [Channel] -- All incoming channels to this process.
                         , outchannels   :: [Channel] -- All outgoing channels from this process.
                         } -> ProcessState a

someFunc :: IO ()
someFunc = putStrLn "someFunc"
