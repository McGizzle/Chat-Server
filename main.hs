module Main where
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad (replicateM)
import Data.Maybe

type Chatroom = TVar [(String, Chan Msg)]
data User = User { name :: String, usr_id :: Int, hdl :: Handle }
 deriving Show

type Msg = (String, Int)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet 6969 iNADDR_ANY
  listen sock 2
  chats <- atomically $ newTVar []
  connLoop sock chats 0

connLoop :: Socket -> Chatroom -> Int -> IO ()
connLoop sock chats num = do
  conn <- accept sock
  forkIO $ manageConn (fst conn) chats num
  connLoop sock chats (num + 1)

manageConn :: Socket -> Chatroom -> Int -> IO ()
manageConn sock chats num = do
  print ("User[" ++ show num ++ "] has joined the network.")
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  dInfo <- fmap init (hGetLine hdl)
  info <- return $ words dInfo
  let usr = User "" num hdl
  let chName = head info
  chan <- getChannel chats chName
  runChat chan usr

hGetLines :: Int -> Handle -> IO [String]
hGetLines n hdl = replicateM n (hGetLine hdl)
 
runChat :: Chan (String,Int) -> User -> IO ()
runChat chatroom usr = do
  let sendMe msg = hPutStrLn (hdl usr) msg
  let num = (usr_id usr)
  let sendMsg msg = writeChan chatroom (msg,num) 
  print ("User[" ++ show num ++ "] has joined a chatroom.")
  thisChat <- dupChan chatroom
  sendMsg ("---> ["++ show num ++"] has joined the chat.")   
  sendMe ("Welcome to the chat.") 

  forkIO $ forever $ do
    (line, msgNum) <- readChan thisChat
    when (msgNum /= num) $ sendMe (line)
 
  forever $ do
    line <- fmap init (hGetLine (hdl usr))
    sendMsg ("["++ show num  ++"]: " ++ line)

getChannel :: Chatroom -> String -> IO (Chan Msg)
getChannel chats name = do
			  chan <- newChan
			  atomically $ do list <- readTVar chats
					  let result = findCHR list name
				          if isNothing result
					    then do 
						   let newList = (name, chan) : list
						   writeTVar chats newList
					    	   return chan
					    else return $ fromJust result

findCHR :: [(String, Chan Msg)] -> String -> Maybe (Chan Msg)
findCHR [] _ = Nothing
findCHR (x:xs) name  
  | name == (fst x) = Just (snd x)
  | otherwise = findCHR xs name




