module Main where
import Network.Socket hiding (Broadcast)
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Map as Map hiding (map)
import Data.Char
import Data.Hashable
import Data.List.Split
import Data.Maybe

data Message = Command [[String]] String | Error String String | Broadcast String String String | Response String

data Client = Client { clientName :: String, clientChan :: TChan Message, clientHdl :: Handle, clientID :: Int }
data Chatroom = Chatroom { roomName :: String, roomRef:: Int, clients :: TVar (Map Int Client) }

type Chatrooms = TVar (Map Int Chatroom)

newClient :: Int -> String -> Handle -> IO Client
newClient num name hdl = do
  chan <- newTChanIO
  return Client { clientName = name, clientChan = chan, clientHdl = hdl, clientID = num }

newChatroom :: String -> Client -> STM Chatroom
newChatroom name client = do
  let d = Map.insert (clientID client) client Map.empty
  c <- newTVar d
  return Chatroom { roomName = name, roomRef = (hash name),clients = c }

fetchChatroom :: Int -> Chatrooms -> IO (Maybe Chatroom)
fetchChatroom roomRef chatrooms = atomically $ do
  chatmap <- readTVar chatrooms
  let c = Map.lookup roomRef chatmap
  case c of 
    Nothing -> return Nothing
    Just c -> return (Just c)

broadcastMessage :: Message -> Client -> Int -> Chatrooms -> IO ()
broadcastMessage msg client roomRef chatrooms = do
  c <- fetchChatroom roomRef chatrooms
  atomically $ do
    case c of
      Nothing -> sendMessage client $ Error "200" "This chatroom does not exist"
      Just room -> do
        roomMap <- readTVar (clients room)
        mapM_ (\c -> sendMessage c msg) (Map.elems roomMap)

sendMessage :: Client -> Message -> STM ()
sendMessage client msg = writeTChan (clientChan client) msg
 
handleMessage :: Chatrooms -> Client -> Message -> IO Bool
handleMessage chatrooms client message = do
  case message of
    Response msg                 -> display $ msg
    Broadcast roomRef sender msg -> display ("CHAT:" ++ roomRef ++ "\nCLIENT_NAME:" ++ sender  ++ "\nMESSAGE:" ++ msg)
    Error code msg               -> display ("ERROR_CODE:" ++ code ++ "\nERROR_DESCRIPTION:" ++ msg)
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
           broadcastMessage (Broadcast roomRef name (name ++" has left the chatroom.")) client (read roomRef :: Int) chatrooms
           putStrLn ("client["++ name ++"] has left chatroom: " ++ roomRef)
         return True
       [["JOIN_ID:",id],["CLIENT_NAME:",clientName],("MESSAGE:":msg)] -> do
         broadcastMessage (Broadcast roomRef name (unwords msg)) client (read roomRef :: Int) chatrooms
         return True   
       [["PORT:",_],["CLIENT_NAME:",clientName]]                      -> do
         return False    
       _                          -> do error; return True
       where 
        roomName = info
        roomRef = info
        error = atomically $ do sendMessage client $ Error "300" "Error pal"
        name = clientName client   
    where
     display x = do hPutStrLn (clientHdl client) x; return True

removeClient :: Client -> Int -> Chatrooms -> IO Bool
removeClient client roomRef chatrooms = do
  c <- fetchChatroom roomRef chatrooms
  atomically $ do
    case c of 
      Nothing -> return False
      Just room -> do
        modifyTVar' (clients room) $ Map.delete (clientID client)
        sendMessage client $ Response ("LEFT_CHATROOM:" ++ show roomRef  ++ "\nJOIN_ID:" ++ (show $ clientID client))
        return True    

addClient :: Client -> String -> Chatrooms -> IO ()
addClient client roomName chatrooms = atomically $ do
  chatmap <- readTVar chatrooms
  let c = Map.lookup (hash roomName) chatmap
  case c of 
    Nothing -> do
      chat <- newChatroom roomName client
      let newChatmap = Map.insert (roomRef chat) chat chatmap
      writeTVar chatrooms newChatmap
    (Just room) -> do
      roomMap <- readTVar (clients room)
      let newRoom = Map.insert (clientID client) client roomMap
      writeTVar (clients room) newRoom
  sendMessage client $ Response ("JOINED_CHATROOM:" ++ roomName ++ "\nSERVER_IP:0\nPORT:0\nROOM_REF:"++ (show $ hash roomName) ++ "\nJOIN_ID:" ++ (show $ clientID client))

runClient :: Chatrooms -> Client -> IO ()
runClient chatrooms client = do
  race receiver sender
  return ()
  where
   sender = forever $ do
     msg <- hGetLine (clientHdl client)
     case words msg of 
      ["JOIN_CHATROOM:",roomName] -> do
            cmds <- repeat 3
            send cmds roomName
      ["LEAVE_CHATROOM:",roomRef] -> do
            cmds <- repeat 2
            send cmds roomRef
      ["CHAT:",roomRef]           -> do
            cmds <- repeat 3
            send cmds roomRef
      ["DISCONNECT:",_]           -> do
            cmds <- repeat 2
            send cmds ""
            
      _                           -> error
      where
       send :: [String] -> String -> IO ()
       send x y = atomically $ do sendMessage client $ Command (map words x) y
       error = atomically $ do sendMessage client $ Error "200" "Error pal"
       repeat n = replicateM n $ hGetLine $ clientHdl client

   receiver = join $ atomically $ do
     msg <- readTChan (clientChan client)
     return $ do
       continue <- handleMessage chatrooms client msg
       when continue $ receiver

buildClient :: Chatrooms -> Int -> Handle -> (HostName, String) -> IO ()
buildClient chatrooms num hdl (ip,port) = do
  loop
  hClose hdl
  where 
   loop = do
     cmd <- hGetLine hdl
     case words cmd of
       ["KILL_SERVICE"]            -> return ()
       ["HELO","BASE_TEST"]             -> do
         hPutStrLn hdl ("HELO text\nIP:"++ ip ++"\nPort:"++ port ++"\nStudentID: 14314836") >> loop
       ["JOIN_CHATROOM:",roomName] -> do
         cmds <- replicateM 3 $ hGetLine hdl
         print $ words cmd
         case map words cmds of
           [["CLIENT_IP:",_],["PORT:",_],["CLIENT_NAME:",clientName]] -> do
             client <- newClient num clientName hdl
             addClient client roomName chatrooms
             broadcastMessage (Broadcast (show roomRef) clientName (clientName ++" has joined the chat." )) client roomRef chatrooms 
             hPutStrLn hdl ("JOINED_CHATROOM:" ++ roomName ++ "\nSERVER_IP:0\nPORT:0\nROOM_REF:"++ (show $ hash roomName) ++ "\nJOIN_ID:" ++ (show $ num)) 
             putStrLn ("New client["++ clientName ++"]["++ show num ++"] added to room: " ++ roomName)
             runClient chatrooms client
           _                                                   -> hPutStrLn hdl "ERROR_CODE:100\nERROR_DESCRIPTION:Incomplete." >> loop 
         where roomRef = hash roomName
       _                            -> hPutStrLn hdl "ERROR_CODE:100\nERROR_DESCRIPTION:Please join a chatroom before continuing." >> loop

getClients :: Chatrooms -> Socket -> Int -> String -> IO ()
getClients chatrooms sock num port = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  sockName <- getSocketName sock
  (ip,_) <- getNameInfo [] True False sockName
  forkIO $ buildClient chatrooms num hdl (fromJust ip, port)
  getClients chatrooms sock (num + 1) port

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  args <- getArgs
  let port = head args
  bind sock $ SockAddrInet 5555 iNADDR_ANY
  listen sock 5
  chatrooms <- atomically $ newTVar Map.empty
  putStrLn ("Server started... Listening on port: "++ port)
  getClients chatrooms sock 1 port
  return ()

