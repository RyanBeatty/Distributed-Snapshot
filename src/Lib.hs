{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Lens (makeLenses, (^.), (.~), set)
import Control.Monad.RWS.Lazy (RWS, get, gets, put, modify, tell, ask)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Traversable (mapM)
import qualified Data.Set as S

-- Interface for the 'Color' abstraction that Spezialleti and Kearns
-- describes in their paper. Here the 'White' process id acts as a sentinal
-- process id.
class ProcessId a where
        -- Is the process id the 'White' process id.
        isWhiteId :: a -> Bool

-- Wrapper around Integer type.
newtype Count = Count { _getCount :: Integer }
  deriving (Show, Eq)
makeLenses ''Count

class Channel a where
        makeChan :: a
        sendTo :: a -> ()
        receiveFrom :: a -> ()

-- Type Parameters:
-- ProcessId p
data Message p =
  -- A warning message is sent to alert processes that a snapshot has begun.
  WarningMsg { _warningColor :: p -- The color of the initiator of the current snapshot.
             , _senderColor  :: p -- The color of the process that sent this warning message.
             }
  deriving (Show, Eq)
makeLenses ''Message

data ProcessConfig = ProcessConfig
  deriving (Show, Eq)

-- A Letter contains a Message to send and the necessary info to be able to
-- actually send the message to a process.
-- Type Parameters:
-- (Channel c, ProcessId p)
data Letter c p = Letter { _senderOf    :: c -- The channel to send the response accross.
                         , _recipientOf :: c -- The channel to send the message across.
                         , _msg         :: Message p  -- The message payload.
                         } 
  deriving (Show, Eq)
makeLenses ''Letter

-- The state of a Process.
-- Type Parameters:
-- (ProcessId p, Channel c)
data ProcessState p c = ProcessState { _idColor       :: p     -- The color that uniquely identifies the process.
                                     , _localColor    :: p     -- The current color of the process.
                                     , _parentColor   :: p     -- The color of the parent process of this process. Inititally set to WHITE.
                                     , _opCount       :: Count -- The number of operations that have been executed on this process.
                                     , _snapshotCount :: Count -- The number of snapshots that this process has been involved in.
                                     , _inChannels    :: [c]   -- All incoming channels to this process.
                                     , _outChannels   :: [c]   -- All outgoing channels from this process.
                                     , _warningRecSet :: S.Set c -- Set of channels that have sent a warning to this process.
                                     , _idBorderSet   :: S.Set p -- Set of process ids that belong to neighboring master initiator processes.
                                     }
  deriving (Show, Eq)
makeLenses ''ProcessState

-- Type Parameters:
-- (ProcessId a, Channel b, x)
newtype ProcessAction p c x = ProcessAction { runAction :: RWS ProcessConfig [Letter c p] (ProcessState p c) x }
        deriving (Functor, Applicative, Monad, MonadReader ProcessConfig, MonadWriter [Letter c p], MonadState (ProcessState p c))
--type ProcessAction p c x = RWS ProcessConfig [Letter c p] (ProcessState p c) x

msgHandler :: (Channel c, ProcessId p) => Letter c p -> ProcessAction p c ()
msgHandler letter =
        case letter^.msg of
    WarningMsg { _warningColor=warningColor, _senderColor=senderColor } -> undefined

changeColor :: (ProcessId p, Channel c) => p -> p -> ProcessAction p c ()
changeColor warning_color sender_color = do
        -- set the local color and parent color of the process to be the warning color and sender color respectively.
        -- The warning color tells the process what process is its master and the parent color tells the process which
        -- process is its parent for this snapshot. 
        modify (set parentColor sender_color . set localColor warning_color)
        ps <- get
        if isWhiteId (ps^.parentColor)
           then sendChildMsg (ps^.idColor) (ps^.parentColor)
           else return ()
        saveCurrentState
        gets (^. inChannels) >>= (tell . map (\x -> undefined))
        gets (^. outChannels) >>= (tell . map (\x -> undefined))

sendChildMsg id_color parent_color = undefined

saveCurrentState = undefined

handleWarningMsg :: (ProcessId p, Channel c) => c -> c -> p -> p -> ProcessAction p c ()
handleWarningMsg senderOf recipientOf warningColor senderColor = undefined
  



someFunc :: IO ()
someFunc = putStrLn "someFunc"
