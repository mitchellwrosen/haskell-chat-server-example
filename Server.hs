{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Server where

import           Control.Applicative
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           System.IO
import           Text.Printf              (hPrintf, printf)

import Channel
import Client
import Types

data Server = Server
    { serverChannels :: TVar (Map ChannelName Channel)
    , serverClients  :: TVar (Map ClientName Client)
    }

newServer :: IO Server
newServer = atomically $ do
    server <- Server <$> newTVar M.empty <*> newTVar M.empty
    addChannel server defaultChannelName
    return server

defaultChannelName :: ChannelName
defaultChannelName = "General"

-- Add a new channel. Assumes a channel with the same name does not already exist.
addChannel :: Server -> ChannelName -> STM ()
addChannel Server{..} name = newChannel name >>= modifyTVar serverChannels . M.insert name

-- Look up a channel on the server, by name.
lookupChannel :: Server -> ChannelName -> STM (Maybe Channel)
lookupChannel Server{..} name = M.lookup name <$> readTVar serverChannels

-- Look up or create a channel on the server and return it.
lookupOrCreateChannel :: Server -> ChannelName -> STM Channel
lookupOrCreateChannel server@Server{..} name = lookupChannel server name >>= \case
    Nothing -> do
        chan <- newChannel name
        modifyTVar serverChannels . M.insert name $ chan
        return chan
    Just chan -> return chan

-- Look up a client on the server, by name.
lookupClient :: Server -> ClientName -> STM (Maybe Client)
lookupClient Server{..} name = M.lookup name <$> readTVar serverClients

-- Remove a client from the server.
removeClient :: Server -> Client -> IO ()
removeClient Server{..} client@Client{..} = atomically $ do
    clientLeaveChannel LeaveReasonDisconnected client
    modifyTVar' serverClients $ M.delete clientName


-- Main entry point to communication with a client.
handleClient :: Handle -> Server -> IO ()
handleClient handle server@Server{..} = do
    -- Swallow carriage returns sent by telnet clients.
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    readName
  where
    readName :: IO ()
    readName = do
        hPutStrLn handle "What is your name?"
        name <- hGetLine handle
        if null name
            then readName
            else mask $ \restore -> tryAddClient server name handle >>= \case
                Nothing -> restore $ do
                    _ <- hPrintf handle "The name '%s' is in use, please choose another." name
                    readName
                Just client ->
                    restore (runClient server client)
                        `finally` removeClient server client


-- Try to add a client to the server; fail if the requested name is taken.
tryAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
tryAddClient server@Server{..} name handle = atomically $ do
    clients <- readTVar serverClients
    if M.member name clients
        then return Nothing
        else do
            client <- newClient name handle
            notify client welcomeMsg

            Just generalChat <- lookupChannel server defaultChannelName    -- Assume "General" chat always exists.
            _ <- clientJoinChannel JoinReasonConnected client generalChat  -- This will always be Nothing

            writeTVar serverClients $ M.insert name client clients
            return (Just client)

welcomeMsg :: String
welcomeMsg = unlines
    [ "Welcome to the server! Available commands:"
    , "/whisper name msg - whisper 'msg' to 'name'"
    , "/join    name     - join channel 'name'"
    , "/users            - list the users in the current channel"
    , "/whoami           - list your name and channel"
    , "/kick    name     - kick 'name'"
    , "/quit             - quit the server"
    ]

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..} = do
    race_ serverThread receiveThread
    return ()
  where
    receiveThread :: IO ()
    receiveThread = forever $ do
        msg <- hGetLine clientHandle
        atomically $ sendMessage client (Command msg)

    serverThread :: IO ()
    serverThread = join $ atomically $ do
        kicked <- readTVar clientKicked
        case kicked of
            Just (kicker,reason) -> return $
                sendBytes client $ "You have been kicked by " ++ kicker ++ ": " ++ reason
            Nothing -> do
                msg <- clientReadMessage client
                return $ do
                    continue <- handleMessage server client msg
                    when continue $ serverThread

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ client (Notice msg)    = sendBytes client ("*** " ++ msg) >> return True
handleMessage _ client (Tell name msg) = sendBytes client (name ++ " whispers: " ++ msg) >> return True
handleMessage _ client (Broadcast chName clName msg)
    | clName == clientName client = return True  -- Ignore messages from self.
    | otherwise                 = sendBytes client (printf "[%s] %s: %s" chName clName msg) >> return True
handleMessage server client@Client{..} (Command msg) = case words msg of
    "/whisper":who:what -> whisper server client who (unwords what)           >> return True
    "/w"      :who:what -> whisper server client who (unwords what)           >> return True

    "/join":which:[]    -> joinChannel server client which                    >> return True
    "/j"   :which:[]    -> joinChannel server client which                    >> return True

    "/users":[]         -> users client                                       >> return True

    "/whoami":[]        -> whoami client                                      >> return True

    "/kick":who:why     -> kick server client who (unwords why)               >> return True
    "/k"   :who:why     -> kick server client who (unwords why)               >> return True

    "/quit":[]          ->                                                       return False
    "/q"   :[]          ->                                                       return False

    ('/':_):_           -> sendBytes client ("Unrecognized command: " ++ msg) >> return True
    _                   -> atomically (clientBroadcastToChannel client msg)   >> return True

whisper :: Server -> Client -> ClientName -> String -> IO ()
whisper server from@Client{..} to msg = join $ atomically $ lookupClient server to >>= maybe noTell yesTell
  where
    noTell :: STM (IO ())
    noTell = return $ sendBytes from (to ++ " is not connected.")

    yesTell :: Client -> STM (IO ())
    yesTell client = do
        sendMessage client (Tell clientName msg)
        return (return ())

-- Handle a client joining a channel. Possibly purge the client's old channel
-- from the server, if he/she was the last person in it.
joinChannel :: Server -> Client -> ChannelName -> IO ()
joinChannel server@Server{..} client name = atomically $
    lookupOrCreateChannel server name >>= clientJoinChannel JoinReasonJoined client >>= \case
        Nothing      -> return ()
        Just oldChan -> possiblyDeleteChannel oldChan
  where
    -- Delete a channel if it's empty, but not if it's the default chat channel.
    possiblyDeleteChannel :: Channel -> STM ()
    possiblyDeleteChannel Channel{..}
        | channelName == defaultChannelName = return ()
        | otherwise = do
            clients <- readTVar channelClients
            when (S.null clients) $
                modifyTVar serverChannels (M.delete channelName)

users :: Client -> IO ()
users client = atomically $ clientGetChan client >>= \case
    Nothing -> notify client "You aren't in a channel."
    Just chan -> readTVar (channelClients chan) >>= notify client . show . S.toAscList

whoami :: Client -> IO ()
whoami client@Client{..} = atomically $ do
    msgSuffix <- maybe (", not in any channel.")
                       ((" in channel " ++) . channelName)
                       <$> clientGetChan client
    notify client $ printf "You are %s%s" clientName msgSuffix

kick :: Server -> Client -> ClientName -> String -> IO ()
kick Server{..} kicker kickee reason = atomically $
    readTVar serverClients >>= maybe noKick yesKick . M.lookup kickee
  where
    noKick :: STM ()
    noKick = notify kicker (kickee ++ " is not connected")

    yesKick :: Client -> STM ()
    yesKick Client{..} = do
      writeTVar clientKicked $ Just (clientName, reason)
      notify kicker $ "You kicked " ++ kickee
