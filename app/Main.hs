module Main where
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.Map as M
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT) 
import Control.Monad.Trans.Class (lift)

type Msg = (String, Int)
type Chatrooms = TVar (Map String (Chan Msg))
data User = User { name :: String, usrId :: Int, hdl :: Handle }

port :: PortNumber
port = 5555

main :: IO ()
main = do 
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet port iNADDR_ANY
  listen sock 5
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
  let chName = head $ words dInfo
  let usr = User "" num hdl
  chan <- getChan chats chName
  runChat (chName,chan) usr
  hClose hdl

hGetLines :: Int -> Handle -> IO [String]
hGetLines n hdl = replicateM n (hGetLine hdl)
 
runChat :: (String, Chan Msg) -> User -> IO ()
runChat (name,chan) usr = do
  let sendMe = hPutStrLn (hdl usr)
  let num = usrId usr
  let sendMsg msg = writeChan chan (msg,num) 
  
  thisChat <- dupChan chan
  
  print ("User[" ++ show num ++ "] has joined ["++ name ++ "] chatroom.")
  sendMsg ("---> ["++ show num ++"] has joined the chat.")   
  sendMe ("Welcome to the '"++ name  ++ "' chat.") 

  reader <- forkIO $ forever $ do
    (line, msgNum) <- readChan thisChat
    when (msgNum /= num) $ sendMe line
 
  runMaybeT $ forever $ do
    line <- lift $ fmap init (hGetLine (hdl usr))
    when (leaveChat line) $ mzero
    lift $ sendMsg ("["++ show num  ++"]: " ++ line)

  killThread reader
  sendMsg ("<--- ["++ show num  ++"] has left the chat.")
  print ("User ["++ show num  ++"] has left the '"++ name  ++ "' chat.")

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

