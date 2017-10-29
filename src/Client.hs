module Client where

import Data.Hashable (hash)
import Data.Map as Map
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import System.IO
import Utils
import Chatroom
------------------------ Client -------------------------------------
newClient :: Int -> String -> Handle -> IO Client
newClient num name hdl = do
  chan <- newTChanIO
  return Client { clientName = name, clientChan = chan, clientHdl = hdl, clientID = num }

removeClient :: Client -> Int -> Chatrooms -> IO Bool
removeClient client roomRef chatrooms = do
  c <- fetchChatroom roomRef chatrooms
  atomically $ do
    case c of 
      Nothing -> return False
      Just room -> do
        sendMessage client $ Response ("LEFT_CHATROOM:" ++ show roomRef  ++ "\nJOIN_ID:" ++ (show $ clientID client))
        modifyTVar' (clients room) $ Map.delete (clientID client)
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
  sendMessage client $ Response $ "JOINED_CHATROOM:" ++ roomName ++ "\rSERVER_IP:0.0.0.0\rPORT:0\nROOM_REF:"++ (show $ hash roomName) ++ "\nJOIN_ID:" ++ (show $ clientID client)


