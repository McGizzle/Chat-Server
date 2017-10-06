module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v
type Chat_HT = TVar (HashTable String (Chan String)) 

data User = User {
		usr_id:: Int,
		name:: String,
		sock:: Socket
	}

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr	1
	bind sock $ SockAddrInet 6969 iNADDR_ANY
	listen sock 2
	ht <- newTVarIO ([] :: [String, Chan String])
	connLoop sock 0 ht

----Connection Functions---------
connLoop :: Socket -> Int -> TVar [String] -> IO ()	
connLoop sock userNum ht = do
	conn <- accept sock
	forkIO $ manageConn (fst conn) userNum ht
	connLoop sock (userNum + 1) ht

manageConn :: Socket -> Int -> TVar [String] -> IO ()
manageConn sock userNum ht = do
--	hdl <- socketToHandle (sock user) ReadWriteMode
--	hSetBuffering hdl NoBuffering
--	msg <- fmap init (hGetLine hdl)
	dInfo <- recv sock 4096
	info <- return $ words msg
	let user = User userNum "" sock 
	filterConn (head info) user info ht
	close sock


filterConn :: String -> User -> [String] -> TVar [String] -> IO ()
filterConn "Join" user info ht = joinChatroom user info ht
filterConn "LEAVE_CHATROOM" user _ _ = leaveChatroom user
filterConn s _ _ _ = killConn s
----------------------------------

---CHATROOM FUNCTIONS-------------
initChatroom :: String -> TVar [String] -> Chan String
initChatroom name ht = do
	

joinChatroom user info ht = do
	let num = show (usr_id user)
	send (sock user) ("Welcome " ++ num ++ "\n" )
	print ("User["++num++"] has joined.")
	chName <- return getChatroomName
	chatroom <- initChatroom chName ht
	chat chatroom user	

	

leaveChatroom _ = print "Leave"
killConn error = print error

getChatroomName :: String
getChatroomName = "Ch_room_1"

chat :: Chan String -> User -> IO ()
chat chatroom user = do
	hdl <- socketToHandle (sock user) ReadWriteMode
	hSetBuffering hdl NoBuffering
	thisChat <- dupChan chatroom

	forkIO $ forever $ do
		line <- readChan thisChat
		hPutStrLn hdl line

	forever $ do
		line <- fmap init (hGetLine hdl)
		writeChan chatroom line
----------------------------------

---USER FUNCTIONS-----------------
updateName :: String -> User -> User
updateName new_name user = User (usr_id user) new_name (sock user)
----------------------------------
