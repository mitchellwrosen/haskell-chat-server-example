{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-} 

module Client where

import Control.Concurrent.STM
import System.IO              (Handle, hPutStrLn)

import Channel
import Types

data Client = Client
    { clientName          :: ClientName
    , clientHandle        :: Handle
    , clientKicked        :: TVar (Maybe (String,String))
    , clientPersonalChan  :: TChan Message
    -- If the client is in a Channel, it needs to keep two data: the Channel
    -- itself (so it's possible to later leave the channel), and its dup'd
    -- broadcast TChan. Store these concomitant values in a tuple.
    , clientChan          :: TVar (Maybe (Channel, TChan Message))
    }

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
    {-broadcastChan <- dupTChan broadcastChan-}
    personalChan  <- newTChan
    broadcastChan <- newTVar Nothing
    kicked        <- newTVar Nothing
    return Client
        { clientName          = name
        , clientHandle        = handle
        , clientKicked        = kicked
        , clientPersonalChan  = personalChan
        , clientChan          = broadcastChan
        }

-- Get the client's current channel.
clientGetChan :: Client -> STM (Maybe Channel)
clientGetChan = (fmap . fmap) fst . readTVar . clientChan

-- Get the client's current broadcast TChan.
clientGetTChan :: Client -> STM (Maybe (TChan Message))
clientGetTChan = (fmap . fmap) snd . readTVar . clientChan

-- Read a message from either the personal channel or the broadcast channel.
clientReadMessage :: Client -> STM Message
clientReadMessage client = readPersonal `orElse` readBroadcast
  where
    readPersonal :: STM Message 
    readPersonal = readTChan (clientPersonalChan client)

    readBroadcast :: STM Message
    readBroadcast = clientGetTChan client >>= maybe retry readTChan

-- Join the specified channel. Return the channel that was left, if there is
-- one, so the server can purge empty channels.
clientJoinChannel :: JoinReason -> Client -> Channel -> STM (Maybe Channel)
clientJoinChannel reason client@Client{..} newChan = readTVar clientChan >>= \case
    -- Not in a channel - just join the new one.
    Nothing -> do
        clientJoinChannel'
        return Nothing
    -- In a channel - leave the current one. Old broadcast chan will get
    -- garbage collected.
    Just (curChan, _) 
        | channelName curChan == channelName newChan -> do
            notify client "You're already in that channel."
            return Nothing
        | otherwise -> do
            -- Join other channel first, because chanRemoveClient will broadcast a
            -- leave message. We don't want to see that message ourselves, because
            -- we know we've left the channel.
            clientJoinChannel'
            chanRemoveClient LeaveReasonLeft curChan clientName
            return (Just curChan)
  where
    clientJoinChannel' :: STM ()
    clientJoinChannel' = do
        notify client $ "Joined channel: " ++ channelName newChan

        -- Make this call before duping the channel, so we don't see our own
        -- join message broadcast to the channel.
        chanAddClient reason newChan clientName
        dupTChan (channelBroadcastChan newChan) >>=
            writeTVar clientChan . Just . (newChan,)

-- Leave the current channel.
clientLeaveChannel :: LeaveReason -> Client -> STM ()
clientLeaveChannel reason client@Client{..} = readTVar clientChan >>= \case
    -- Not in a channel - do nothing.
    Nothing -> notify client "You're not in a channel."
    -- In a channel - leave it, let old broadcast chan get garbage collected.
    Just (curChan, _) -> do
        notify client $ "Left channel: " ++ channelName curChan
        chanRemoveClient reason curChan clientName
        writeTVar clientChan Nothing

-- Broadcast a message from the client to his/her current channel. No-op if
-- the client isn't in a channel.
clientBroadcastToChannel :: Client -> String -> STM ()
clientBroadcastToChannel client@Client{..} msg = 
    clientGetChan client >>= maybe (return ()) (\c -> chanBroadcast c clientName msg)

-- Send a message to a client.
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientPersonalChan

-- Notify a client.
notify :: Client -> String -> STM ()
notify client = sendMessage client . Notice

-- Send raw bytes to a client.
sendBytes :: Client -> String -> IO ()
sendBytes Client{..} = hPutStrLn clientHandle
