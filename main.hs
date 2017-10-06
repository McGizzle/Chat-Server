module Main where

import Network.Socket
import System.IO
import Data.List.Split
import Control.Concurrent
import Control.Monad.Fix

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
	connLoop sock 0

----Connection Functions---------
connLoop :: Socket -> Int -> IO ()	
connLoop sock userNum = do
	conn <- accept sock
	forkIO $ manageConn (fst conn) userNum
	connLoop sock (userNum + 1)

manageConn :: Socket -> Int -> IO ()
manageConn sock userNum = do
	dInfo <- recv sock 4096
	info <- return $ words dInfo
	let user = createUser userNum "" sock 
	filterConn (head info) user info
	close sock

filterConn :: String -> User -> [String] -> IO ()
filterConn "Join" user info = joinChatroom user info
filterConn "LEAVE_CHATROOM" user _ = leaveChatroom user
filterConn s _ _ = killConn s
----------------------------------

---CHATROOM FUNCTIONS-------------
joinChatroom user info = do
	let num = show (usr_id user)
	send (sock user) ("Welcome " ++ num ++ "\n" )
	print ("User["++num++"] has joined.")
	chatroom <- initChatroom
	chat chatroom user	

initChatroom :: IO (Chan String)
initChatroom = newChan

leaveChatroom _ = print "Leave"
killConn error = print error

getChatroomName :: [String] -> String
getChatroomName _ = "Ch_room_1"

chat :: Chan String -> User -> IO ()
chat chatroom user = do
	hdl <- socketToHandle (sock user) ReadWriteMode
	hSetBuffering hdl NoBuffering
	thisChat <- dupChan chatroom

	forkIO $ fix $ \loop -> do
		line <- readChan thisChat
		hPutStrLn hdl line
		loop

	fix $ \loop -> do
		line <- fmap init (hGetLine hdl)
		writeChan chatroom line
		loop
----------------------------------

---USER FUNCTIONS-----------------
createUser :: Int -> String -> Socket -> User
createUser usr_id name sock = User usr_id name sock

updateName :: String -> User -> User
updateName new_name user = User (usr_id user) new_name (sock user)
----------------------------------
