# Chat-Server

A simple TCP chat server written in Haskell. The server interacts with a specific set of protocols detailed below.

## Protocol

#### Joining a chat room
```
		  JOIN_CHATROOM: [chatroom name]
		  CLIENT_IP: [IP Address of client if UDP | 0 if TCP]
		  PORT: [port number of client if UDP | 0 if TCP]
		  CLIENT_NAME: [string Handle to identifier client user]
```
#### The server will reply with
```
		  JOINED_CHATROOM: [chatroom name]
		  SERVER_IP: [IP address of chat room]
		  PORT: [port number of chat room]
		  ROOM_REF: [integer that uniquely identifies chat room on server]
		  JOIN_ID: [integer that uniquely identifies client joining]
```
### Use the ```JOIN_ID``` and ```ROOM_REF```to 

#### send messages

```
		  CHAT: [ROOM_REF]
		  JOIN_ID: [integer identifying client to server]
		  CLIENT_NAME: [string identifying client user]
		  MESSAGE: [string terminated with '\n\n']
```
#### leave chatrooms  
```
		  LEAVE_CHATROOM: [ROOM_REF]
		  JOIN_ID: [integer previously provided by server on join]
		  CLIENT_NAME: [string Handle to identifier client user]
```
#### disconnect
```
		  DISCONNECT: [IP address of client if UDP | 0 if TCP]
		  PORT: [port number of client it UDP | 0 id TCP]
		  CLIENT_NAME: [string handle to identify client user]
```
#### kill
```
 "KILL_SERVICE\n"
```

## Building & Running

### To compile
 ```bash compile.sh```

### To run
 ```bash run.sh <port>```

