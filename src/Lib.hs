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

-- Type Parameters:
-- ProcessId p
data Message p = 
        -- A warning message is sent to alert processes that a snapshot has begun.
    WarningMsg { _messageWarningColor :: p -- The color of the initiator of the current snapshot.
               , _messageSenderColor  :: p -- The color of the process that sent this warning message.
               }
  | ChildMsg { _messageChildColor :: p -- The color of the child process.
             } 
  deriving (Show, Eq)
makeFields ''Message

data ProcessConfig = ProcessConfig
  deriving (Show, Eq)

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

-- A SnapshotInfo contains all of the local state information of a process that is needed
-- to build the snapshot by master process of a snapshot.
data SnapshotInfo p = SnapshotInfo { _snapshotInfoIdColor :: p            -- The color of the process this SnapshotInfo is for.
                                   , _snapshotInfoOpCount :: Count        -- The operation count of the process.
                                   , _snapshotInfoSnapshotCount :: Count  -- The number of snapshots this process has been in.
                                   , _snapshotInfoWarningColor :: p       -- The color of the initiator of the snapshot this process is in.
                                   , _snapshotInfoParentColor :: p        -- The parent color of this process.
                                   }
  deriving (Show, Eq)
makeFields ''SnapshotInfo

-- A StateBundle is a collection of SnapshotInfos from various processes. Child processes accumulate and send their
-- bundles to their master processes which build the snapshots.
newtype StateBundle p = StateBundle { _stateBundleSnapshotInfos :: [SnapshotInfo p] }
        deriving (Show, Eq, Monoid)
makeFields ''StateBundle

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
          WarningMsg { _messageWarningColor=warning_color, _messageSenderColor=sender_color } -> handleWarningMsg (letter^.senderOf) warning_color sender_color
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
           then tell . mempty $ makeChildMsg (ps^.idColor) (ps^.parentColor) (ps^.idColor)
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
        let snapshot = makeSnapshotInfo (ps^.idColor) (ps^.opCount) (ps^.snapshotCount) warning_color (ps^.parentColor)
         in modify (stateBundle .~ mempty snapshot)

handleWarningMsg :: (ProcessId p) => p -> p -> p -> ProcessAction p ()
handleWarningMsg sender warning_color sender_color = do
        -- Check if this process is currently involved in a snapshot.
        not_in_snapshot <- gets (isWhiteId . view localColor)
        if not_in_snapshot
           -- If this process is not apart of a snapshot, change its color to signal that it is in the current snapshot.
           then changeColor warning_color sender_color
           else return ()
        

-- Add the color of the new child to this processes' childSet.
-- Args:
--    child_color - The color of the new child process.
handleChildMsg :: (ProcessId p) => p -> ProcessAction p ()
handleChildMsg = modify . over childSet . mappend . mempty
 
makeLetter :: (ProcessId p) => p -> p -> Message p -> Letter p
makeLetter sender recipient msg = Letter { _letterSenderOf=sender, _letterRecipientOf=recipient, _letterMsg=msg }

makeChildMsg :: (ProcessId p) => p -> p -> p -> Letter p
makeChildMsg sender recipient child_color = makeLetter sender recipient (ChildMsg { _messageChildColor=child_color })

makeWarningMsg :: (ProcessId p) => p -> p -> p -> p -> Letter p
makeWarningMsg sender recipient warning_color sender_color = makeLetter sender recipient (WarningMsg { _messageWarningColor=warning_color, _messageSenderColor=sender_color })

makeSnapshotInfo :: (ProcessId p) => p -> Count -> Count -> p -> p -> SnapshotInfo p
makeSnapshotInfo id_color op_count snapshot_count warning_color parent_color = SnapshotInfo { _snapshotInfoIdColor=id_color, _snapshotInfoOpCount=op_count, _snapshotInfoSnapshotCount=snapshot_count, _snapshotInfoWarningColor=warning_color, _snapshotInfoParentColor=parent_color }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
