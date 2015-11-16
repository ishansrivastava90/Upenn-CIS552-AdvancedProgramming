module Client (local, client, send) where

-- A network client, for communicating with a server using sockets
-- Uses https://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket.html (basically the same interface as in C)

import Network.Socket hiding (send)
import System.IO

-- IP address of the local host
local :: HostName
local = "127.0.0.1"

-- start the client given an IP address and a port. The port should 
-- be a string number > 1024
client :: HostName -> ServiceName -> IO Handle
client ip port = do
  (sa:_) <- getAddrInfo Nothing (Just ip) (Just port)
  sock   <- socket (addrFamily sa) Stream defaultProtocol    
  connect sock (addrAddress sa)
  handle <- socketToHandle sock WriteMode
  hSetBuffering handle (BlockBuffering Nothing)
  return handle
  
-- send a message to the server  
send :: Handle -> String -> IO ()  
send h c = do   
  hPutStrLn h c
  hFlush h
  
