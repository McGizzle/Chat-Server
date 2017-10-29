module Chatroom where
import Control.Concurrent.STM
import Data.Map as Map
import Data.Hashable (hash)
import Utils

------------------------ Chatroom -------------------------------------
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


