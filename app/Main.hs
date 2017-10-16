module Main where
import Network.Socket hiding (Broadcast)
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Map as Map
import Data.Char
import Data.Hashable
import Data.List.Split

data Message = Command [[String]] | Error String | Broadcast String | Response String

data Client = Client { clientName :: String, clientChan :: TChan Message, clientHdl :: Handle, clientID :: Int }
data Chatroom = Chatroom { roomName :: String, roomRef:: Int, clients :: TVar (Map Int Client) }

type Chatrooms = TVar (Map Int Chatroom)

newClient :: Int -> String -> Handle -> IO Client
newClient num name hdl = do
  chan <- newTChanIO
  return Client { clientName = name,
                  clientChan = chan,
                  clientHdl = hdl,
                  clientID = num
                }

newChatroom :: String -> Client -> STM Chatroom
newChatroom name client = do
  let d = Map.insert (clientID client) client Map.empty
  c <- newTVar d
  return Chatroom { roomName = name,
                    roomRef = (hash name),
                    clients = c
                  }

fetchChatroom :: Int -> Chatrooms -> STM (Maybe Chatroom)
fetchChatroom roomRef chatrooms = do
  chatmap <- readTVar chatrooms
  let c = Map.lookup roomRef chatmap
  case c of 
    Nothing -> return Nothing
    Just c -> return (Just c)

broadcastMessage :: Message -> Client -> Int -> Chatrooms -> IO ()
broadcastMessage msg client roomRef chatrooms = atomically $ do
  c <- fetchChatroom roomRef chatrooms
  case c of
    Nothing -> sendMessage client (Error "This chat does not exist")
    Just room -> do
      roomMap <- readTVar (clients room)
      mapM_ (\c -> sendMessage c msg) (Map.elems roomMap)

sendMessage :: Client -> Message -> STM ()
sendMessage client msg = writeTChan (clientChan client) msg
 
handleMessage :: Chatrooms -> Client -> Message -> IO Bool
handleMessage chatrooms client message = do
  case message of
    Response msg -> do
      hPutStrLn (clientHdl client) msg
      return True
    Broadcast msg -> do
      hPutStrLn (clientHdl client) msg
      return True
    Error msg -> do
      hPutStrLn (clientHdl client) msg
      return True
    Command msg ->
      case msg of
        [["chat",msg],["roomref",roomRef]] -> do
          broadcastMessage (Broadcast msg) client (read roomRef :: Int) chatrooms       
          return True
        [["leave",roomRef],["id",clientNum]] -> do
          removeClient client (read roomRef :: Int) chatrooms
          return True
        _                                  -> atomically $ do
          sendMessage client (Error "Error pal.")
          return True

removeClient :: Client -> Int -> Chatrooms -> IO ()
removeClient client roomRef chatrooms = atomically $ do
  c <- fetchChatroom roomRef chatrooms
  case c of 
    Nothing -> return ()
    Just room -> do
      modifyTVar' (clients room) $ Map.delete (clientID client)
  sendMessage client $ Response ("Left chatroom")

addClient :: Client -> String -> Chatrooms -> IO ()
addClient client chatName chatrooms = atomically $ do
  chatmap <- readTVar chatrooms
  let c = Map.lookup (hash chatName) chatmap
  case c of 
    Nothing -> do
      chat <- newChatroom chatName client
      let newChatmap = Map.insert (roomRef chat) chat chatmap
      writeTVar chatrooms newChatmap
    (Just room) -> do
      roomMap <- readTVar (clients room)
      let newRoom = Map.insert (clientID client) client roomMap
      writeTVar (clients room) newRoom
  sendMessage client $ Response ("JoinID: " ++ show (hash chatName))

runClient :: Chatrooms -> Client -> IO ()
runClient chatrooms client = do
  race receiver sender
  return ()
  where
   sender = forever $ do
     msg <- hGetLines (clientHdl client) ""
     atomically $ do sendMessage client (Command msg)
   receiver = join $ atomically $ do
     msg <- readTChan (clientChan client)
     return $ do
       continue <- handleMessage chatrooms client msg
       when continue $ receiver

buildClient :: Chatrooms -> Int -> Handle -> IO ()
buildClient chatrooms num hdl = do
  loop
  where 
   loop = do
     msg <- hGetLines hdl ""
     case msg of
       [["join",roomName],["name",clientName]] -> do
         client <- newClient num clientName hdl
         addClient client roomName chatrooms
         print ("New client["++ clientName ++"]["++ show num ++"] added to room: " ++ roomName)
         forkIO $ runClient chatrooms client
         return ()
       _                    -> print "ERROR" >> loop

getClients :: Chatrooms -> Socket -> Int -> IO ()
getClients chatrooms sock num = do
  conn <- accept sock
  hdl <- socketToHandle (fst conn) ReadWriteMode
  hSetBuffering hdl NoBuffering
  forkIO $ buildClient chatrooms num hdl
  getClients chatrooms sock (num + 1)


hGetLines :: Handle -> String -> IO [[String]]
hGetLines hdl acc = do
  line <- hGetLine hdl
  more <- hReady hdl
  if more then hGetLines hdl $ line ++ acc
  else do
    return $ Prelude.map words $ splitOn "\\n" $ line ++ acc

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet 5555 iNADDR_ANY
  listen sock 5
  chatrooms <- atomically $ newTVar Map.empty
  print "Server started"
  getClients chatrooms sock 1

