module Main where

import Network.Socket
import System.IO
import Data.List.Split
import qualified Data.ByteString.Char8 as C


type Msg = (String)

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr	1
	bind sock $ SockAddrInet 6969 iNADDR_ANY
	listen sock 2
	connLoop sock

connLoop :: Socket -> IO ()	
connLoop sock = do
	conn <- accept sock
	manageConn conn
	connLoop sock

manageConn :: (Socket,SockAddr) -> IO ()
manageConn (sock,_) = do
	dInfo <- recv sock 4096
	info <- return $ words dInfo
	filterConn (head info) info sock
	close sock

filterConn :: String -> [String] -> Socket -> IO ()
filterConn "JOIN_CHATROOM" info sock = joinChatroom info sock
filterConn "LEAVE_CHATROOM" _ sock = leaveChatroom sock
filterConn s _ _ = killConn s

joinChatroom _ _ = print "Join"
leaveChatroom _ = print "Leave"
killConn error = do
	 print error
