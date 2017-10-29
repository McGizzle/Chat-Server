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

broadcastMessage :: Message -> Client -> Int -> Chatrooms -> IO ()
broadcastMessage msg client roomRef chatrooms = do
  c <- fetchChatroom roomRef chatrooms
  atomically $ do
    case c of
      Nothing -> sendMessage client $ Error "200" "This chatroom does not exist"
      Just room -> do
        roomMap <- readTVar (clients room)
        mapM_ (\c -> sendMessage c msg) (Map.elems roomMap)
 
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
         left <- removeClient client (read roomRef :: Int) chatrooms
         when left $ do
           atomically $ do sendMessage client $ Broadcast roomRef name (name ++ " has left the chatroom.")
           broadcastMessage (Broadcast roomRef name (name ++" has left the chatroom.")) client (read roomRef :: Int) chatrooms
           putStrLn ("client["++ name ++"] has left chatroom: " ++ roomRef)
         return True
       [["JOIN_ID:",id],["CLIENT_NAME:",clientName],("MESSAGE:":msg),[]] -> do
         broadcastMessage (Broadcast roomRef name $ unwords msg) client (read roomRef :: Int) chatrooms
         return True   
       [["PORT:",_],["CLIENT_NAME:",clientName]]                      -> do
         return False    
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
  return ()
  where
   sender = do
     cmd <- hGetLine $ clientHdl client 
     case words cmd of 
      ["JOIN_CHATROOM:",roomName] -> do
            log "join a chatroom"
            cmds <- repeat 3
            send cmds roomName
            sender
      ["LEAVE_CHATROOM:",roomRef] -> do
            log "leave a chatroom"
            cmds <- repeat 2
            send cmds roomRef
            sender
      ["CHAT:",roomRef]           -> do
            log "send a chat"
            cmds <- repeat 4
            mapM (\ s -> putStrLn ("C: " ++ s)) cmds
            send cmds roomRef
            sender
      ["DISCONNECT:",_]           -> do
            log "disconnect"
            cmds <- repeat 2
            send cmds ""
            sender
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
       
buildClient :: Chatrooms -> Int -> Handle -> (HostName, String) -> IO ()
buildClient chatrooms num hdl (ip,port) = do
  loop
  return ()
  where 
   loop = do
     cmd <- hGetLine hdl
     case words cmd of
       ["KILL_SERVICE"]            -> return ()
       ["HELO","BASE_TEST"]        -> do
         hPutStrLn hdl ("HELO BASE_TEST\nIP:"++ ip ++"\nPort:"++ port ++"\nStudentID: 14314836\n") >> loop
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

getClients :: Chatrooms -> Socket -> Int -> String -> IO ()
getClients chatrooms sock num port = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  --hSetNewlineMode hdl noNewlineTranslation
  sockName <- getSocketName sock
  (ip,_) <- getNameInfo [] True False sockName
  forkFinally (buildClient chatrooms num hdl (fromJust ip, port)) (\_ -> hClose hdl)
  getClients chatrooms sock (num + 1) port

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
  getClients chatrooms sock 1 port

