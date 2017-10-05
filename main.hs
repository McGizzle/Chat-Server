module Main where

import Network.Socket
import System.IO
import Data.List.Split

type Msg = (String)

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOptions sock ReuseAddr	1
	bind sock $ SockAddrInet 6969 iNADDR_ANY
	listen sock 2
	connLoop sockn

connLoop :: Socket -> IO ()	
connLoop sock = do
	conn <- accept sock
	manageConn conn
	connLoop sock

manageConn :: (Socket,SocketAddr) -> IO ()
manageConn (sock,_) = do
	info <- recv sock
	checkConn $ unpack info
	close sock

checkConn :: Char -> IO ()
checkConn info = do
	splitInfo = map (splitOn ":") $ splitOn "\n" info
