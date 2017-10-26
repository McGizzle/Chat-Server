module Main where
import Network.Socket hiding (Broadcast)
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Map as Map
import Data.Char
import Data.Hashable
import Data.List.Split
import Data.Maybe

data Message = Command [[String]] | Error String String | Broadcast String String String | Response String

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
    Response msg               -> display $ msg
    Broadcast roomRef name msg -> display ("CHAT:" ++ roomRef ++ "\nCLIENT_NAME:" ++ name  ++ "\nMESSAGE:" ++ msg)
    Error code msg             -> display ("ERROR_CODE:" ++ code ++ "\nERROR_DESCRIPTION:" ++ msg)
    Command msg                -> 
      case msg of 
        [["CHAT:",roomRef],["JOIN_ID:",x],["CLIENT_NAME:",y],("MESSAGE:":msg),[],[]]       -> do
          if (x /= id || y /= name) then error
          else do
            broadcastMessage (Broadcast roomRef (clientName client) (unwords msg)) client (read roomRef :: Int) chatrooms       
            return True
        [["JOIN_CHATROOM:",roomName],["CLIENT_IP:","0"],["PORT:","0"],["CLIENT_NAME:",y]] -> do
          if ( y /= name) then error
          else do
            addClient client roomName chatrooms
            broadcastMessage (Broadcast (show $ hash roomName) name (name++" has joined the chat.")) client (hash roomName) chatrooms  
            putStrLn ("client["++ name ++"] added to room: " ++ roomName)
            return True
        [["LEAVE_CHATROOM:",roomRef],["JOIN_ID:",x],["CLIENT_NAME:",y]]                  -> do
          if (x /= id || y /= name) then error
          else do
            left <- removeClient client (read roomRef :: Int) chatrooms
            when left $ do
              broadcastMessage (Broadcast roomRef (clientName client) "Has left the chatroom.") client (read roomRef :: Int) chatrooms
              putStrLn ("client["++ name ++"] has left chatroom: " ++ roomRef)
            return True
        [["DISCONNECT:","0"],["PORT:","0"],["CLIENT_NAME:",y]]                            -> do
          if ( y /= name) then error
          else do
            putStrLn (name ++ " disconnected.")
            return False
        _                                                                                    -> atomically $ do
          sendMessage client $ Error "200" "Unknown command."
          return True
        where
         error = atomically $ do sendMessage client $ Error "300" "Inconsistent information provided"; return True
         name = clientName client
         id = show $ clientID client
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
  sendMessage client $ Response ("JOINED_CHATROOM:" ++ roomName ++ "\nSERVER_IP:\nPORT:\nROOM_REF:"++ (show $ hash roomName) ++ "\nJOIN_ID:" ++ (show $ clientID client))

runClient :: Chatrooms -> Client -> IO ()
runClient chatrooms client = do
  race receiver sender
  return ()
  where
   sender = forever $ do
     msg <- getCommand (clientHdl client)
     atomically $ do sendMessage client $ Command msg

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
     msg <- getCommand hdl
     case msg of
       [["HELO","text"],[]] -> do
         hPutStrLn hdl ("HELO text\nIP:"++ ip ++"\nPort:"++ port ++"\nStudentID: 14314836") >> loop
       [["JOIN_CHATROOM:",roomName],["CLIENT_IP:","0"],["PORT:","0"],["CLIENT_NAME:",clientName]] -> do
         let roomRef = hash roomName
         client <- newClient num clientName hdl
         addClient client roomName chatrooms
         broadcastMessage (Broadcast (show $ roomRef) clientName (clientName ++" has joined the chat." )) client roomRef chatrooms  
         putStrLn ("New client["++ clientName ++"]["++ show num ++"] added to room: " ++ roomName)
         runClient chatrooms client
       _                    -> hPutStrLn hdl "ERROR_CODE:100\nERROR_DESCRIPTION:Please join a chatroom before continuing." >> loop

getClients :: Chatrooms -> Socket -> Int -> String -> IO ()
getClients chatrooms sock num port = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  sockName <- getSocketName sock
  (ip,_) <- getNameInfo [] True False sockName
  forkIO $ buildClient chatrooms num hdl (fromJust ip, port)
  getClients chatrooms sock (num + 1) port


getCommand :: Handle -> IO [[String]]
getCommand hdl = do
  cmd <- hGetLine hdl
  when (cmd == "KILL_SERVICE\\n\r") $ exitSuccess
  return $ Prelude.map words $ splitOn "\\n" $ cmd


main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  args <- getArgs
  let port = head args
  ip <- inet_addr "0.0.0.0"
  bind sock $ SockAddrInet 5555 iNADDR_ANY
  listen sock 5
  chatrooms <- atomically $ newTVar Map.empty
  putStrLn ("Server started... Listening on port: "++ port)
  getClients chatrooms sock 1 port

