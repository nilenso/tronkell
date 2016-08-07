{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types
import Tronkell.Game.Types as Game

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Control.Concurrent
import qualified Data.Text.Encoding as E

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

  let server = Server gConfig gameVar playersVar sock serverChan
  mainLoop server 0

type ClientId = Int

mainLoop :: Server -> ClientId -> IO ()
mainLoop server@Server{..} clientId = do
  (conn, _) <- accept serverSocket
  forkIO $ runClient conn server clientId
  mainLoop server (clientId + 1)

nickToUserId :: C.ByteString -> UserID
nickToUserId = UserID . E.decodeUtf8

isNickTaken :: C.ByteString -> [User] -> Bool
isNickTaken nick users = elem (nickToUserId nick) . map userId $ users

mkUser :: C.ByteString -> User
mkUser nick = User (nickToUserId nick) Waiting

runClient :: Socket -> Server -> ClientId -> IO ()
runClient clientSocket server@Server{..} clientId = do
  send clientSocket $ C.pack "Take a nick name : "
  let maxNameLength = 20
  nick <- recv clientSocket maxNameLength
  added <- modifyMVar serverUsers $ \users ->
    if isNickTaken nick users
    then return (users, False)
    else return ((mkUser nick : users), True)
  if False == added
  then runClient clientSocket server clientId
  else return ()
