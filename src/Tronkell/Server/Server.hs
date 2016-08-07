{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types
import Tronkell.Game.Types as Game
import Tronkell.Types

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Control.Concurrent
-- import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Exception
import qualified Data.Map as M (fromList, elems, Map)


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
  internalChan <- newChan

  let server = Server gConfig gameVar playersVar sock serverChan clientsChan internalChan

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
  send clientSocket $ C.pack "Waiting for other players to start the game...!!!\n"

  -- because every client wants same copy of the message, duplicate channel.
  outClientChan <- dupChan clientsChan
  writer <- forkIO $ fix $ \loop -> do
    outmsg <- readChan outClientChan
    send clientSocket $ C.pack (show outmsg)
    loop

  -- block on ready-signal from server-thread to start the game.
  signal <- readChan internalChan
  case signal of
    GameReadySignal config players -> writeChan outClientChan $ GameReady config players

  send clientSocket $ C.pack "Here.. you go!!!\n"
  send clientSocket $ C.pack "Movements: type L for left , R for right, Q for quit... enjoy.\n"

  let maxBytesToRecv = 1024
  handle (\(SomeException _) -> return ()) $ fix $ \loop ->
    do
      inmsg <- recv clientSocket maxBytesToRecv
      if C.null inmsg
      then return () -- client ended the connection.
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
    PlayerReady clientId -> do (readyUsersCount, allUsersCount) <-  modifyMVar serverUsers $ \users ->
                                    let [user] = filter (\u -> (userId u) == clientId) users
                                        newUsers = user{ userState = Ready } : (filter (\u -> (userId u) /= clientId) users)
                                        readyUsers = filter (\u -> (userState u) == Ready) users
                                    in return (newUsers, (length readyUsers, length users))
                               -- if all users are ready, start the game.
                               when (allUsersCount > 1 && readyUsersCount == allUsersCount) $ do
                                 users <- readMVar serverUsers
                                 let players = playersFromUsers users
                                 modifyMVar_ serverGame $ \_ -> return $ Just $ Game Nothing players InProgress serverGameConfig
                                 writeChan internalChan (GameReadySignal serverGameConfig (M.elems players))

    PlayerExit clientId -> do modifyMVar_ serverUsers $ \users ->
                                return $ filter (\user -> (userId user) /= clientId) users
    PlayerTurnLeft _ -> return ()
    PlayerTurnRight _ -> return ()

  handleIncomingMessages server

playersFromUsers :: [User] -> M.Map PlayerNick Player
playersFromUsers users = M.fromList $ map (\u -> let nick = PlayerNick (T.unpack . getUserID . userId $ u)
                                                     player = Player nick Alive (0,0) North []
                                                 in (nick, player)) users
