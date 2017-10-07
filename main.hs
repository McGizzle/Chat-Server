module Main where
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

type Chatroom = TVar [(String, Chan String)]
data User = User { name :: String, usr_id :: Int, hdl :: Handle }


main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet 6969 iNADDR_ANY
  listen sock 2
  chan <- newChan
  let lobby = ("lobby",chan) : []
  chats <- atomically $ newTVar []
  connLoop sock chats 0

connLoop :: Socket -> Chatroom -> Int -> IO ()
connLoop sock chats num = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  let usr = User "" num hdl
  chName <- fmap init (hGetLine hdl)
  chan <- getChannel chats chName
  forkIO $ runChat chan usr
  connLoop sock chats
 
runChat :: Chan String -> User -> IO ()
runChat chatroom hdl = do
  thisChat <- dupChan chatroom
  
  forkIO $ forever $ do
    line <- readChan thisChat
    hPutStrLn (hdl usr) line
  
  forever $ do
    line <- fmap init (hGetLine (hdl usr))
    writeChan chatroom line


getChannel :: Chatroom -> String -> IO (Chan String)
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

findCHR :: [(String, Chan String)] -> String -> Maybe (Chan String)
findCHR [] _ = Nothing
findCHR (x:xs) name  
  | name == (fst x) = Just (snd x)
  | otherwise = findCHR xs name




