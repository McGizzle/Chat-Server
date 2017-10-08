module Main where
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.Map as M
import Control.Monad
import Control.Monad (replicateM)
import Control.Break

type Msg = (String, Int)
type Chatrooms = TVar (Map String (Chan Msg))
data User = User { name :: String, usr_id :: Int, hdl :: Handle }

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet 6969 iNADDR_ANY
  listen sock 2
  chats <- atomically $ newTVar M.empty
  connLoop sock chats 0

connLoop :: Socket -> Chatrooms -> Int -> IO ()
connLoop sock chats num = do
  conn <- accept sock
  forkIO $ manageConn (fst conn) chats num
  connLoop sock chats (num + 1)

manageConn :: Socket -> Chatrooms -> Int -> IO ()
manageConn sock chats num = do
  print ("User[" ++ show num ++ "] has joined the network.")
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering
  dInfo <- fmap init (hGetLine hdl)
  info <- return $ words dInfo
  let usr = User "" num hdl
  let chName = head info
  chan <- getChan chats chName
  runChat (chName,chan) usr
  close sock

hGetLines :: Int -> Handle -> IO [String]
hGetLines n hdl = replicateM n (hGetLine hdl)
 
runChat :: (String, Chan Msg) -> User -> IO ()
runChat (name,chan) usr = do
  let sendMe msg = hPutStrLn (hdl usr) msg
  let num = (usr_id usr)
  let sendMsg msg = writeChan chan (msg,num) 
  
  thisChat <- dupChan chan
  
  print ("User[" ++ show num ++ "] has joined ["++ name ++ "] chatroom.")
  sendMsg ("---> ["++ show num ++"] has joined the chat.")   
  sendMe ("Welcome to the '"++ name  ++ "' chat.") 

  reader <- forkIO $ forever $ do
    (line, msgNum) <- readChan thisChat
    when (msgNum /= num) $ sendMe (line)
 
  forever $ do
    line <- fmap init (hGetLine (hdl usr))
-- TODO: Exit loop ::  when (leaveChat line) $
    sendMsg ("["++ show num  ++"]: " ++ line)

  killThread reader
  sendMsg ("<--- ["++ show num  ++"] has left the chat.")

leaveChat :: String -> Bool
leaveChat ":leave" = True
leaveChat _ = False

getChan :: Chatrooms -> String -> IO (Chan Msg)
getChan chats name = do
  chan <- newChan
  atomically $ do 
    list <- readTVar chats
    let result = M.lookup name list
    if isNothing result
      then do 
        let newList = M.insert name chan list
        writeTVar chats newList
        return chan
    else return $ fromJust result

