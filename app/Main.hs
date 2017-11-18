module Main where
import Network.Socket hiding (Broadcast)
import System.IO
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Map as Map hiding (map)
import Data.Char
import Data.Hashable (hash)
import Data.Maybe
import Data.Either
import Utils
import Client
import Chatroom

handleMessage :: Chatrooms -> Client -> Message -> IO Bool
handleMessage chatrooms client message = do
  case message of
    Response msg                 -> display $ msg
    Broadcast roomRef sender msg -> display ("CHAT:" ++ roomRef ++ "\nCLIENT_NAME:" ++ sender  ++ "\nMESSAGE:" ++ msg ++ "\n")
    Error code msg               -> display ("ERROR_CODE:" ++ code ++ "\nERROR_DESCRIPTION:" ++ msg)
    Kill                         -> return False
    Command msg info             -> 
      case msg of 
       [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",clientName]]     -> do
         addClient client roomName chatrooms
         broadcastMessage (Broadcast (show $ hash $ roomName) name (name ++ " has joined the chatroom.")) client (hash roomName) chatrooms
         putStrLn ("client["++ name ++"] added to room: " ++ roomName)
         return True
       [["JOIN_ID:",id],["CLIENT_NAME:",clientName]]                  -> do
         left <- removeClient client (read roomRef :: Int) chatrooms True
         when left $ do
           putStrLn ("client["++ name ++"] has left chatroom: " ++ roomRef)
         return True
       [["JOIN_ID:",id],["CLIENT_NAME:",clientName],("MESSAGE:":msg),[]] -> do
         broadcastMessage (Broadcast roomRef name $ unwords msg) client (read roomRef :: Int) chatrooms
         putStrLn ("client["++ id ++ "] successfully sent a chat message.")
         return True   
       [["PORT:",_],["CLIENT_NAME:",clientName]]                      -> do
         disconnClient client chatrooms
         return True
       _                          -> do error; return True
       where 
        roomName = info
        roomRef = info
        error = atomically $ do sendMessage client $ Error "300" "Arguments do not match expected"
        name = clientName client   
    where
     display x = do hPutStrLn (clientHdl client) x; return True

runClient :: Chatrooms -> Client -> IO ()
runClient chatrooms client = do
  putStrLn ("Client["++ clientName client  ++"] running.")
  race receiver sender
  putStrLn ("Client "++ show (clientID client) ++ " is leavin'")
  return ()
  where
   sender = forever $ do
     cmd <- hGetLine $ clientHdl client 
     case words cmd of 
      ["JOIN_CHATROOM:",roomName] -> do
            log "join a chatroom"
            cmds <- repeat 3
            send cmds roomName
      ["LEAVE_CHATROOM:",roomRef] -> do
            log "leave a chatroom"
            cmds <- repeat 2
            send cmds roomRef
      ["CHAT:",roomRef]           -> do
            log "send a chat"
            cmds <- repeat 4
            send cmds roomRef
      ["DISCONNECT:",_]           -> do
            log "disconnect"
            cmds <- repeat 2
            send cmds ""
      ["KILL_SERVICE"]            -> do putStrLn "Killing..."      
      _                           -> error
      where
       send :: [String] -> String -> IO ()
       send x y = atomically $ do sendMessage client $ Command (map words x) y 
       error = atomically $ do sendMessage client $ Error "200" "Unknown initial command"
       repeat n = replicateM n $ hGetLine $ clientHdl client
       log msg = putStrLn $ "Client[" ++ show  (clientID client) ++"] is attempting to " ++ msg ++ "..."

   receiver = join $ atomically $ do
     msg <- readTChan (clientChan client)
     return $ do
       continue <- handleMessage chatrooms client msg
       when continue $ receiver
       
buildClient :: Chatrooms -> Int -> Handle -> (String, String) -> IO ()
buildClient chatrooms num hdl (ip,port) = do
  loop
  return ()
  where 
   loop = do
     cmd <- hGetLine hdl
     case words cmd of
       ["JOIN_CHATROOM:",roomName] -> do
         cmds <- replicateM 3 $ hGetLine hdl
         case map words cmds of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",clientName]] -> do
             client <- newClient num clientName hdl
             addClient client roomName chatrooms
             putStrLn ("New client["++ clientName ++"]["++ show num ++"] added to room: " ++ roomName)
             broadcastMessage (Broadcast (show roomRef) clientName (clientName ++" has joined the chat." )) client roomRef chatrooms
             runClient chatrooms client
           _                                                   -> hPutStrLn hdl "ERROR_CODE:100\nERROR_DESCRIPTION:Incomplete." >> loop 
         where roomRef = hash roomName
       _                            -> hPutStrLn hdl "ERROR_CODE:100\nERROR_DESCRIPTION:Please join a chatroom before continuing." >> loop

heloBase :: Handle -> String -> IO ()
heloBase hdl port = do
  msg <- hGetLine hdl
  case words msg of 
    ["HELO",t]       -> do
      hPutStrLn hdl resp
      where resp = "HELO "++ t ++"\nIP:134.226.44.50\nPort:"++ port ++"\nStudentID:14314836"

getConns :: Chatrooms -> Socket -> Int -> String -> IO ()
getConns chatrooms sock num port = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  sockName <- getSocketName sock
  (ip,_) <- getNameInfo [] True False sockName
  forkFinally (buildClient chatrooms num hdl (fromJust ip, port)) (\_ -> do putStrLn ("Client["++ show num ++"] disconnected"); hClose hdl)
  getConns chatrooms sock (num + 1) port

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  args <- getArgs
  let port = head args
  bind sock $ SockAddrInet (read port :: PortNumber) iNADDR_ANY
  listen sock 5
  chatrooms <- atomically $ newTVar Map.empty
  putStrLn ("Server started... Listening on port: "++ port)
  (conn,addr) <- accept sock
  hdl <- socketToHandle conn ReadWriteMode
  hSetBuffering hdl NoBuffering
  heloBase hdl port >> getConns chatrooms sock 1 port

  return ()
