{-# LANGUAGE RecordWildCards #-}

module Channel where

import           Control.Applicative
import           Control.Concurrent.STM
import           Data.Set               (Set)
import qualified Data.Set               as S

import Types

-- The reason why a client has left/joined a channel.
data LeaveReason
    = LeaveReasonLeft
    | LeaveReasonDisconnected

data JoinReason
    = JoinReasonJoined
    | JoinReasonConnected

-- Chat channel datatype, not to be confused with a TChan.
data Channel = Channel
    { channelName          :: ChannelName
    , channelClients       :: TVar (Set ClientName)
    , channelBroadcastChan :: TChan Message
    }

newChannel :: ChannelName -> STM Channel
newChannel name = Channel name <$> newTVar S.empty <*> newBroadcastTChan

-- Remove a ClientName from this Channel's clients. Notify the channel of the
-- client leaving or disconnecting.
chanRemoveClient :: LeaveReason -> Channel -> ClientName -> STM ()
chanRemoveClient LeaveReasonLeft         = chanRemoveClient' chanNotifyHasLeft
chanRemoveClient LeaveReasonDisconnected = chanRemoveClient' chanNotifyHasDisconnected

chanRemoveClient' :: (Channel -> ClientName -> STM ()) -> Channel -> ClientName -> STM ()
chanRemoveClient' notifyAction chan@Channel{..} name = do
    notifyAction chan name
    modifyTVar channelClients . S.delete $ name

-- Add a ClientName to this Channel's clients.
chanAddClient :: JoinReason -> Channel -> ClientName -> STM ()
chanAddClient JoinReasonJoined    = chanAddClient' chanNotifyHasJoined
chanAddClient JoinReasonConnected = chanAddClient' chanNotifyHasConnected

chanAddClient' :: (Channel -> ClientName -> STM ()) -> Channel -> ClientName -> STM ()
chanAddClient' notifyAction chan@Channel{..} name = do
    notifyAction chan name
    modifyTVar channelClients . S.insert $ name

-- Notify the channel a client has left.
chanNotifyHasLeft :: Channel -> ClientName -> STM ()
chanNotifyHasLeft chan name = chanNotify chan (name ++ " has left the channel.")

-- Notify the channel a client has disconnected.
chanNotifyHasDisconnected :: Channel -> ClientName -> STM ()
chanNotifyHasDisconnected chan name = chanNotify chan (name ++ " has disconnected.")

-- Notify the channel a client has joined.
chanNotifyHasJoined :: Channel -> ClientName -> STM ()
chanNotifyHasJoined chan name = chanNotify chan (name ++ " has joined the channel.")

-- Notify the channel a client has connected.
chanNotifyHasConnected :: Channel -> ClientName -> STM ()
chanNotifyHasConnected chan name = chanNotify chan (name ++ " has connected.")

-- Send a Message to the channel.
chanMessage :: Channel -> Message -> STM ()
chanMessage = writeTChan . channelBroadcastChan

-- Send a Broadcast to the channel, from a client.
chanBroadcast :: Channel -> ClientName -> String -> STM ()
chanBroadcast chan@Channel{..} clientName msg = chanMessage chan (Broadcast channelName clientName msg)

-- Send a Notice to the channel.
chanNotify :: Channel -> String -> STM ()
chanNotify chan = chanMessage chan . Notice
