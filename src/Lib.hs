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
class (Eq p) => ProcessId p where
        -- Is the process id the 'White' process id.
        isWhiteId :: p -> Bool

-- Wrapper around Integer type.
newtype Count = Count { _getCount :: Integer }
  deriving (Show, Eq)
makeLenses ''Count

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
-- (ProcessId p)
data Letter p = Letter { _senderOf    :: p -- The sender process of this message.
                       , _recipientOf :: p -- The the receiver process of this message.
                       , _msg         :: Message p  -- The message payload.
                       } 
  deriving (Show, Eq)
makeLenses ''Letter

-- The state of a Process.
-- Type Parameters:
-- (ProcessId p)
data ProcessState p = ProcessState { _idColor       :: p       -- The color that uniquely identifies the process.
                                   , _localColor    :: p       -- The current color of the process.
                                   , _parentColor   :: p       -- The color of the parent process of this process. Inititally set to WHITE.
                                   , _opCount       :: Count   -- The number of operations that have been executed on this process.
                                   , _snapshotCount :: Count   -- The number of snapshots that this process has been involved in.
                                   , _inChannels    :: [p]     -- All processes with an incoming connection to this process.
                                   , _outChannels   :: [p]     -- All processes that this process has an outgoing connection to.
                                   , _warningRecSet :: S.Set p -- Set of processes that have sent a warning to this process.
                                   , _idBorderSet   :: S.Set p -- Set of process ids that belong to neighboring master initiator processes.
                                   }
  deriving (Show, Eq)
makeLenses ''ProcessState

-- Type Parameters:
-- (ProcessId p, x)
newtype ProcessAction p x = ProcessAction { runAction :: RWS ProcessConfig [Letter p] (ProcessState p) x }
        deriving (Functor, Applicative, Monad, MonadReader ProcessConfig, MonadWriter [Letter p], MonadState (ProcessState p))

msgHandler :: (ProcessId p) => Letter p -> ProcessAction p ()
msgHandler letter =
        case letter^.msg of
    WarningMsg { _warningColor=warningColor, _senderColor=senderColor } -> undefined

changeColor :: (ProcessId p) => p -> p -> ProcessAction p ()
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
        id_color <- gets (^. idColor)
        modify (set warningRecSet S.empty)
        gets (^. outChannels) >>= (tell . map (\channel -> makeWarningMsg id_color channel warning_color id_color))

sendChildMsg id_color parent_color = undefined

saveCurrentState = undefined

handleWarningMsg :: (ProcessId p) => p -> p -> p -> p -> ProcessAction p ()
handleWarningMsg senderOf recipientOf warningColor senderColor = undefined
  
makeWarningMsg :: (ProcessId p) => p -> p -> p -> p -> Letter p
makeWarningMsg sender recipient warning_color sender_color = Letter { _senderOf=sender, _recipientOf=recipient, _msg=WarningMsg { _warningColor=warning_color, _senderColor=sender_color } }


someFunc :: IO ()
someFunc = putStrLn "someFunc"
