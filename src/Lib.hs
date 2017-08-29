{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where

import qualified Data.Set as S
import Control.Monad.RWS.Lazy (RWS)

-- Interface for the 'Color' abstraction that Spezialleti and Kearns
-- describes in their paper. Here the 'White' process id acts as a sentinal
-- process id.
class ProcessId a where
        -- Is the process id the 'White' process id.
        isWhiteId :: a -> Bool

-- Wrapper around Integer type.
newtype Count = Count { getCount :: Integer }
  deriving (Show, Eq)

class Channel a where
        makeChan :: a
        sendTo :: a -> ()
        receiveFrom :: a -> ()

data Message = Message
  deriving (Show, Eq)

data ProcessConfig = ProcessConfig
  deriving (Show, Eq)

-- A Letter contains a Message to send and the necessary info to be able to
-- actually send the message to a process.
data Letter a where
        Letter :: (Channel a) =>
                { recipientOf :: a -- The channel to send the message across.
                , msg :: Message   -- The message payload.
                } -> Letter a
deriving instance (Show a) => Show (Letter a)
deriving instance (Eq a) => Eq (Letter a)

-- The state of a Process.
data ProcessState a b where
        ProcessState  :: (ProcessId a, Channel b) =>
                { idColor       :: a     -- The color that uniquely identifies the process.
                , localColor    :: a     -- The current color of the process.
                , opCount       :: Count -- The number of operations that have been executed on this process.
                , snapshotCount :: Count -- The number of snapshots that this process has been involved in.
                , inChannels    :: [b]   -- All incoming channels to this process.
                , outchannels   :: [b]   -- All outgoing channels from this process.
                , warningRecSet :: S.Set b -- Set of channels that have sent a warning to this process.
                , idBorderSet   :: S.Set a -- Set of process ids that belong to neighboring master initiator processes.
                } -> ProcessState a b
deriving instance (Show a, Show b) => Show (ProcessState a b)
deriving instance (Eq a, Eq b) => Eq (ProcessState a b)

newtype ProcessAction a b c = ProcessAction { runAction :: RWS ProcessConfig [Letter b] (ProcessState a b) c }

msgHandler :: Letter b -> ProcessAction a b ()
msgHandler = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
