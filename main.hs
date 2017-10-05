module Main where

import Network.Socket
import System.IO

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOptions sock ReuseAddr	1
	bind sock $ SockAddrInet 6969 iNADDR_ANY
	listen sock 2
	mainLoop sock	
