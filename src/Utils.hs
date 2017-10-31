module Utils where
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Map as Map
import System.IO

data Chatroom = Chatroom { roomName :: String, roomRef:: Int, clients :: TVar (Map Int Client)}
data Client = Client { clientName :: String, clientChan :: TChan Message, clientHdl :: Handle, clientID :: Int }
data Message = Command [[String]] String | Error String String | Broadcast String String String | Response String | Kill

type Chatrooms = TVar (Map Int Chatroom)
sendMessage :: Client -> Message -> STM ()
sendMessage client msg = writeTChan (clientChan client) msg

broadcastMessage :: Message -> Client -> Int -> Chatrooms -> IO ()
broadcastMessage msg client roomRef chatrooms = do
  chatmap <- atomically $ readTVar chatrooms
  let c = Map.lookup roomRef chatmap
  atomically $ do
    case c of
      Nothing -> sendMessage client $ Error "200" "This chatroom does not exist"
      Just room -> do
        roomMap <- readTVar (clients room)
        mapM_ (\c -> sendMessage c msg) (Map.elems roomMap)
 
