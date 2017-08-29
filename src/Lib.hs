{-# LANGUAGE GADTs #-}
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

-- Type Parameters:
-- ProcessId p
data Message p =
  -- A warning message is sent to alert processes that a snapshot has begun.
  WarningMsg { warningColor :: p -- The color of the initiator of the current snapshot.
             , senderColor  :: p -- The color of the process that sent this warning message.
             }
  deriving (Show, Eq)

data ProcessConfig = ProcessConfig
  deriving (Show, Eq)

-- A Letter contains a Message to send and the necessary info to be able to
-- actually send the message to a process.
-- Type Parameters:
-- (Channel c, ProcessId p)
data Letter c p = Letter { senderOf    :: c -- The channel to send the response accross.
                         , recipientOf :: c -- The channel to send the message across.
                         , msg         :: Message p  -- The message payload.
                         } 
  deriving (Show, Eq)

-- The state of a Process.
-- Type Parameters:
-- (ProcessId p, Channel c)
data ProcessState p c = ProcessState { idColor       :: p     -- The color that uniquely identifies the process.
                                     , localColor    :: p     -- The current color of the process.
                                     , opCount       :: Count -- The number of operations that have been executed on this process.
                                     , snapshotCount :: Count -- The number of snapshots that this process has been involved in.
                                     , inChannels    :: [c]   -- All incoming channels to this process.
                                     , outchannels   :: [c]   -- All outgoing channels from this process.
                                     , warningRecSet :: S.Set c -- Set of channels that have sent a warning to this process.
                                     , idBorderSet   :: S.Set p -- Set of process ids that belong to neighboring master initiator processes.
                                     }
  deriving (Show, Eq)

-- Type Parameters:
-- (ProcessId a, Channel b, x)
newtype ProcessAction p c x = ProcessAction { runAction :: RWS ProcessConfig [Letter c p] (ProcessState p c) x }

msgHandler :: (Channel c, ProcessId p) => Letter c p -> ProcessAction p c ()
msgHandler = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
