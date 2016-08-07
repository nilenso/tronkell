{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types
import Tronkell.Game.Types as Game

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Control.Concurrent
-- import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Control.Monad.Fix (fix)
import Control.Exception

startServer :: Game.GameConfig -> IO ()
startServer gConfig = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  let maxQueuedConnections = 5
  listen sock maxQueuedConnections

  gameVar <- newMVar Nothing
  playersVar <- newMVar []
  serverChan <- newChan
  clientsChan <- newChan

  let server = Server gConfig gameVar playersVar sock serverChan clientsChan

  forkIO $ handleIncomingMessages server

  mainLoop server 0

type ClientId = Int

mainLoop :: Server -> ClientId -> IO ()
mainLoop server@Server{..} clientId = do
  (conn, _) <- accept serverSocket
  forkIO $ runClient conn server clientId
  mainLoop server (clientId + 1)

nickToUserId :: String -> UserID
nickToUserId = UserID . T.pack

isNickTaken :: String -> [User] -> Bool
isNickTaken nick users = elem (nickToUserId nick) . map userId $ users

mkUser :: String -> User
mkUser nick = User (nickToUserId nick) Waiting

runClient :: Socket -> Server -> ClientId -> IO ()
runClient clientSocket server@Server{..} clientId = do
  send clientSocket $ C.pack "Take a nick name : "
  let maxNameLength = 20
  nick <- fmap cleanByteString $ recv clientSocket maxNameLength
  let userId = nickToUserId nick
  added <- modifyMVar serverUsers $ \users ->
    if isNickTaken nick users
    then return (users, False)
    else return ((mkUser nick : users), True)
  if False == added
  then runClient clientSocket server clientId
  else do writeChan serverChan $ PlayerJoined userId
          send clientSocket $ C.pack ("Hi.. " ++
                                       nick ++
                                       ".. Type ready when you are ready to play.. quit to quit.")
          fix $ \loop -> do ready <- recv clientSocket 5
                            case cleanByteString ready of
                             "ready" -> playClient userId clientSocket server
                             "quit" -> writeChan serverChan (PlayerExit userId)
                             _ -> loop

cleanByteString :: C.ByteString -> String
cleanByteString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse . C.unpack

playClient :: UserID -> Socket -> Server -> IO ()
playClient clientId clientSocket Server{..} = do
  writeChan serverChan $ PlayerReady clientId
  send clientSocket $ C.pack "Here.. you go!!!"

  -- because every client wants same copy of the message, duplicate channel.
  outClientChan <- dupChan clientsChan
  writer <- forkIO $ fix $ \loop -> do
    outmsg <- readChan outClientChan
    send clientSocket $ C.pack (show outmsg)
    loop

  let maxBytesToRecv = 1024
  handle (\(SomeException _) -> return ()) $ fix $ \loop ->
    do
      inmsg <- recv clientSocket maxBytesToRecv
      if C.null inmsg
      then return ()
      else case decodeMessage clientId inmsg of
             Just (PlayerExit _) -> send clientSocket (C.pack "Sayonara !!!") >> return ()
             Just msg -> writeChan serverChan msg >> loop
             Nothing -> loop

  killThread writer

  writeChan serverChan (PlayerExit clientId)
  return ()

decodeMessage :: UserID -> C.ByteString -> Maybe InMessage
decodeMessage userId msg = case cleanByteString msg of
  "L" -> Just $ PlayerTurnLeft userId
  "R" -> Just $ PlayerTurnRight userId
  "Q" -> Just $ PlayerExit userId
  _ -> Nothing


handleIncomingMessages :: Server -> IO ()
handleIncomingMessages server@Server{..} = do
  inMsg <- readChan serverChan
  case inMsg of
    PlayerJoined _ -> return () -- may want to tell the clients..
    PlayerReady _ -> return () -- may want to tell the clients..
    PlayerExit clientId -> do modifyMVar_ serverUsers $ \users ->
                                let newUsers = filter (\user -> (userId user) /= clientId) users
                                in return newUsers
    PlayerTurnLeft _ -> return ()
    PlayerTurnRight _ -> return ()

  handleIncomingMessages server
