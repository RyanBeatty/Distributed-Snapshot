{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}  -- Needed to use makeFields.
{-# LANGUAGE FunctionalDependencies #-} -- Needed to use makeFields.
{-# LANGUAGE FlexibleInstances #-}      -- Needed to use makeFields.

module Lib where

import Control.Lens (makeLenses, makeFields, (^.), (.~), set, over, view)
import Control.Monad.RWS.Lazy (RWS, get, gets, put, modify, tell, ask)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Monoid (mappend)
import Data.Traversable (mapM)
import qualified Data.Set as S

-- Interface for the 'Color' abstraction that Spezialleti and Kearns
-- describes in their paper. Here the 'White' process id acts as a sentinal
-- process id.
class (Eq p, Ord p) => ProcessId p where
        -- Is the process id the 'White' process id.
        isWhiteId :: p -> Bool

-- Wrapper around Integer type.
newtype Count = Count { _countGetCount :: Integer }
        deriving (Show, Eq, Ord, Num, Enum)
makeFields ''Count

-- A StateInfo contains all of the local state information of a process that is needed
-- to build the snapshot by master process of a snapshot.
data StateInfo p = StateInfo { _stateInfoIdColor :: p            -- The color of the process this StateInfo is for.
                             , _stateInfoOpCount :: Count        -- The operation count of the process.
                             , _stateInfoSnapshotCount :: Count  -- The number of snapshots this process has been in.
                             , _stateInfoWarningColor :: p       -- The color of the initiator of the snapshot this process is in.
                             , _stateInfoParentColor :: p        -- The parent color of this process.
                             }
  deriving (Show, Eq)
makeFields ''StateInfo

data InfoBundle p = InfoBundle { _infoBundleStateInfo :: StateInfo p
                               , _infoBundleIdBorderSet :: S.Set p
                               , _infoBundleIdColor :: p
                               }
  deriving (Show, Eq)
makeFields ''InfoBundle

-- A StateBundle is a collection of StateInfos from various processes. Child processes accumulate and send their
-- bundles to their master processes which build the snapshots.
newtype StateBundle p = StateBundle { _stateBundleStateInfos :: [InfoBundle p] }
        deriving (Show, Eq, Monoid)
makeFields ''StateBundle

-- Type Parameters:
-- ProcessId p
data Message p = 
        -- A warning message is sent to alert processes that a snapshot has begun.
    WarningMsg { _messageMasterColor :: p -- The color of the initiator of the current snapshot.
               , _messageSenderColor  :: p -- The color of the process that sent this warning message.
               }
  | ChildMsg { _messageChildColor :: p -- The color of the child process.
             }
  | DataMsg { _messageStateBundle :: StateBundle p
            }
  | DoneMsg { _messageMyColor :: p
            } 
  deriving (Show, Eq)
makeFields ''Message

-- A Letter contains a Message to send and the necessary info to be able to
-- actually send the message to a process.
-- Type Parameters:
-- (ProcessId p)
data Letter p = Letter { _letterSenderOf    :: p -- The sender process of this message.
                       , _letterRecipientOf :: p -- The the receiver process of this message.
                       , _letterMsg         :: Message p  -- The message payload.
                       } 
  deriving (Show, Eq)
makeFields ''Letter

data ProcessConfig = ProcessConfig
  deriving (Show, Eq)

-- The state of a Process.
-- Type Parameters:
-- (ProcessId p)
data ProcessState p = ProcessState { _processStateIdColor       :: p       -- The color that uniquely identifies the process.
                                   , _processStateLocalColor    :: p       -- The current color of the process.
                                   , _processStateParentColor   :: p       -- The color of the parent process of this process. Inititally set to WHITE.
                                   , _processStateOpCount       :: Count   -- The number of operations that have been executed on this process.
                                   , _processStateSnapshotCount :: Count   -- The number of snapshots that this process has been involved in.
                                   , _processStateInChannels    :: [p]     -- All processes with an incoming connection to this process.
                                   , _processStateOutChannels   :: [p]     -- All processes that this process has an outgoing connection to.
                                   , _processStateWarningRecSet :: S.Set p -- Set of processes that have sent a warning to this process.
                                   , _processStateIdBorderSet   :: S.Set p -- Set of process ids that belong to neighboring master initiator processes.
                                   , _processStateChildSet      :: S.Set p -- Set of all child process of this process.
                                   , _processStateRecStateInfo  :: StateInfo p
                                   , _processStateStateBundle   :: StateBundle p
                                   }
  deriving (Show, Eq)
makeFields ''ProcessState

-- Type Parameters:
-- (ProcessId p, x)
newtype ProcessAction p x = ProcessAction { runAction :: RWS ProcessConfig [Letter p] (ProcessState p) x }
        deriving (Functor, Applicative, Monad, MonadReader ProcessConfig, MonadWriter [Letter p], MonadState (ProcessState p))

msgHandler :: (ProcessId p) => Letter p -> ProcessAction p ()
msgHandler letter =
        case letter^.msg of
          WarningMsg { _messageMasterColor=warning_color, _messageSenderColor=sender_color } -> handleWarningMsg (letter^.senderOf) warning_color sender_color
          ChildMsg { _messageChildColor=child_color } -> handleChildMsg child_color

-- When a process gets its first warning message or wants to initiate a snapshot it will call this function.
-- Args:
--    warning_color - The color of the master process of this snapshot.
--    sender_color  - The parent color of this process.
changeColor :: (ProcessId p) => p -> p -> ProcessAction p ()
changeColor warning_color sender_color = do
        -- Increment the count of the number of snapshots this process has been involved in by 1, set the local color
        -- and parent color of the process to be the warning color and sender color respectively. The warning color tells
        -- the process what process is its master and the parent color tells the process which process is its parent
        -- for this snapshot. 
        modify (set parentColor sender_color . set localColor warning_color . over snapshotCount succ)
        ps <- get
        if (not . isWhiteId) (ps^.parentColor)
           -- If the parent of this process is not the WHITE color, then alert the parent process that this process
           -- is a child.
           then tell . pure $ makeChildMsg (ps^.idColor) (ps^.parentColor) (ps^.idColor)
           -- Else, this process is initiating a new snapshot so do nothing.
           else return ()
        -- Save the local state of this process that is needed for the snapshot.
        takeSnapshot warning_color
        -- Clear warning received set. This accomplishes the effect of starting to record messages on all incoming channels.
        modify (warningRecSet .~ mempty)
        -- Make a warning message to send to every channel.
        id_color <- gets (view idColor)
        gets (view outChannels) >>= (tell . map (\channel -> makeWarningMsg id_color channel warning_color id_color))

-- Saves the local state of the process that is needed for the snapshot.
-- Args: warning_color - The color of the master process of this snapshot.
-- TODO: I think warning_color should be in the process state, so I might be able to remove this parameter.
takeSnapshot :: (ProcessId p) => p -> ProcessAction p ()
takeSnapshot warning_color = do
        ps <- get
        let snapshot = makeStateInfo (ps^.idColor) (ps^.opCount) (ps^.snapshotCount) warning_color (ps^.parentColor)
         in modify (set recStateInfo snapshot)

handleWarningMsg :: (ProcessId p) => p -> p -> p -> ProcessAction p ()
handleWarningMsg sender warning_color sender_color = do
        -- Check if this process is currently involved in a snapshot.
        not_in_state <- gets (isWhiteId . view localColor)
        if not_in_state
           -- If this process is not apart of a snapshot, change its color to signal that it is in the current snapshot.
           then changeColor warning_color sender_color
           else return ()
        -- Check if the warning message was sent by a node in a different region.
        is_neighbor_warning <- gets ((/=) warning_color . view localColor)
        if is_neighbor_warning
           -- We were sent a warning message by a process in a neighboring region, so add its master process to the border set.
           then modify . over idBorderSet . mappend . S.singleton $ warning_color
           else return ()
        -- Signal that we have received a warning from the process.
        modify . over warningRecSet . mappend . S.singleton $ sender_color
        -- Check if we have received a warning message from all incoming channels.
        received_all_warnings <- gets ((==) <$> (view warningRecSet) <*> (S.fromList . view inChannels))
        -- Check if this process is a child process.
        is_leaf_process <- gets ((==) mempty . view childSet)
        if received_all_warnings && is_leaf_process
           then return ()
           else return ()

-- Add the color of the new child to this processes' childSet.
-- Args:
--    child_color - The color of the new child process.
handleChildMsg :: (ProcessId p) => p -> ProcessAction p ()
handleChildMsg = modify . over childSet . mappend . S.singleton 
 
makeLetter :: (ProcessId p) => p -> p -> Message p -> Letter p
makeLetter sender recipient msg = Letter { _letterSenderOf=sender, _letterRecipientOf=recipient, _letterMsg=msg }

makeChildMsg :: (ProcessId p) => p -> p -> p -> Letter p
makeChildMsg sender recipient child_color = makeLetter sender recipient (ChildMsg { _messageChildColor=child_color })

makeWarningMsg :: (ProcessId p) => p -> p -> p -> p -> Letter p
makeWarningMsg sender recipient warning_color sender_color = makeLetter sender recipient (WarningMsg { _messageMasterColor=warning_color, _messageSenderColor=sender_color })

makeDataLetter :: (ProcessId p) => p -> p -> StateBundle p -> Letter p
makeDataLetter sender recipient state_bundle = makeLetter sender recipient (DataMsg { _messageStateBundle=state_bundle })

makeDoneLetter :: (ProcessId p) => p -> p -> p -> Letter p
makeDoneLetter sender recipient id_color = makeLetter sender recipient (DoneMsg { _messageMyColor=id_color })

makeStateInfo :: (ProcessId p) => p -> Count -> Count -> p -> p -> StateInfo p
makeStateInfo id_color op_count snapshot_count warning_color parent_color = StateInfo { _stateInfoIdColor=id_color, _stateInfoOpCount=op_count, _stateInfoSnapshotCount=snapshot_count, _stateInfoWarningColor=warning_color, _stateInfoParentColor=parent_color }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
