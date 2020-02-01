module Main where

import Control.Concurrent (forkIO)
import Control.Exception  (bracket)
import Control.Monad      (forever, void)
import Network.Socket
import System.IO          (IOMode(..), hClose)
import Text.Printf        (printf)

import Server             (handleClient, newServer)

main :: IO ()
main = do
    server <- newServer
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    listen sock 5
    _ <- printf "Listening on port %d\n" port

    forever $ do
        bracket
            (do
                (sock', addr) <- accept sock
                handle <- socketToHandle sock' ReadWriteMode
                pure (handle, addr))
            (\(handle, _addr) -> hClose handle)
            (\(handle, SockAddrInet port' host) -> do
                printf "Accepted connection from %s: %s\n" (show (hostAddressToTuple host)) (show port')
                void (forkIO (handleClient handle server)))

port :: Int
port = 44444
