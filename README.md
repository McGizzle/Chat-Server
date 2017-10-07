# Chat-Server

A chat server written in Haskell.

## How it works

Once the server is running it will listen on port "" for any TCP connections. The server will then read in the protocol input from the client. 
If the protocol is incorrect the server will terminate the connection.
Otheriwse the server will check to see if the requested chatroom exists.
If the chatroom does exist then the server will add the client to this chatroom.
However if the chatroom does not exist the server will create it and add the client.


