module Client where

import Data.Hashable (hash)
import Data.Map as Map
import Data.List
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Monad (unless, when)
import System.IO
import Utils
import Chatroom
------------------------ Client -------------------------------------
newClient :: Int -> String -> Handle -> IO Client
newClient num name hdl = do
  chan <- newTChanIO
  return Client { clientName = name, clientChan = chan, clientHdl = hdl, clientID = num }

removeClient :: Client -> Int -> Chatrooms -> Bool -> IO Bool
removeClient client roomRef chatrooms leaving = do
  c <- fetchChatroom roomRef chatrooms
  case c of 
    Nothing -> return False
    Just room -> atomically $ do
      clientList <- readTVar (clients room)
      when leaving $ sendMessage client $ Response ("LEFT_CHATROOM:" ++ show roomRef  ++ "\nJOIN_ID:" ++ (show $ clientID client))
      unless (Map.notMember (clientID client) clientList) $ do
        mapM_ (\c -> sendMessage c msg) (Map.elems clientList)
      modifyTVar' (clients room) $ Map.delete (clientID client)
      return True
   where
    name = clientName client    
    msg = Broadcast (show roomRef) (clientName client) ((clientName client) ++" has left the chatroom.")

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
  sendMessage client $ Response $ "JOINED_CHATROOM:" ++ roomName ++ "\nSERVER_IP:0.0.0.0\nPORT:0\nROOM_REF:"++ (show $ hash roomName) ++ "\nJOIN_ID:" ++ (show $ clientID client)

disconnClient :: Client -> Chatrooms -> IO ()
disconnClient client chatrooms = do
  chats <- atomically $ readTVar chatrooms
  let names = Prelude.map (\room -> roomName room) (Map.elems chats)
  mapM_ (\name -> remove $ hash name) (sort names)      
  where
   remove s = do
     removeClient client s chatrooms False 
     putStrLn ("removed client["++ show (clientID client) ++ "] from room["++ show s ++"]")
