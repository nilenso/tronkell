{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types as Server
import Tronkell.Game.Types as Game
import Tronkell.Types
import Tronkell.Game.Engine as Engine

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Control.Concurrent
-- import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Exception
import qualified Data.Map as M (fromList, elems, Map)

import System.IO

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
  clientHdl <- socketToHandle conn ReadWriteMode
  hSetBuffering clientHdl NoBuffering
  forkIO $ runClient clientHdl server clientId
  mainLoop server (clientId + 1)

nickToUserId :: String -> UserID
nickToUserId = UserID . T.pack

isNickTaken :: String -> [User] -> Bool
isNickTaken nick users = elem (nickToUserId nick) . map userId $ users

mkUser :: String -> User
mkUser nick = User (nickToUserId nick) Waiting

runClient :: Handle -> Server -> ClientId -> IO ()
runClient clientHdl server@Server{..} clientId = do
  hPutStr clientHdl "Take a nick name : "
  nick <- fmap cleanString $ hGetLine clientHdl
  let userId = nickToUserId nick
  added <- modifyMVar serverUsers $ \users ->
    if isNickTaken nick users
    then return (users, False)
    else return ((mkUser nick : users), True)
  if False == added
  then runClient clientHdl server clientId
  else do writeChan serverChan $ PlayerJoined userId
          hPutStrLn clientHdl $ "Hi.. " ++ nick ++ ".. Type ready when you are ready to play.. quit to quit."
          fix $ \loop -> do ready <- fmap cleanString $ hGetLine clientHdl
                            case ready of
                             "ready" -> playClient userId clientHdl server
                             "quit" -> writeChan serverChan (PlayerExit userId)
                             _ -> loop

cleanString :: String -> String
cleanString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse -- . C.unpack

playClient :: UserID -> Handle -> Server -> IO ()
playClient clientId clientHdl Server{..} = do
  hPutStrLn clientHdl "Waiting for other players to start the game...!!!"

  -- because every client wants same copy of the message, duplicate channel.
  outClientChan <- dupChan clientsChan
  writer <- forkIO $ fix $ \loop -> do
    outmsg <- readChan outClientChan
    hPutStrLn clientHdl $ show outmsg
    loop

  clientInternalChan <- dupChan internalChan

  -- only after duplicating the internalChan, send the PlayerReady msg,
  -- otherwise gameready msg for this second client joining is lost.
  writeChan serverChan $ PlayerReady clientId

  -- block on ready-signal from server-thread to start the game.
  signal <- readChan clientInternalChan
  case signal of
    GameReadySignal config players -> writeChan outClientChan $ GameReady config players

  hPutStrLn clientHdl "Here.. you go!!!"
  hPutStrLn clientHdl "Movements: type L for left , R for right, Q for quit... enjoy."

  handle (\(SomeException _) -> return ()) $ fix $ \loop ->
    do
      canRead <- hIsReadable clientHdl
      when canRead $ do
        inmsg <- fmap cleanString $ hGetLine clientHdl
        case decodeMessage clientId inmsg of
          Just (PlayerExit _) -> hPutStrLn clientHdl "Sayonara !!!" >> return ()
          Just msg -> writeChan serverChan msg >> loop
          Nothing -> loop

  killThread writer

  writeChan serverChan (PlayerExit clientId)
  hClose clientHdl
  return ()

decodeMessage :: UserID -> String -> Maybe InMessage
decodeMessage userId msg = case msg of
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
                                        readyUsers = filter (\u -> (userState u) == Ready) newUsers
                                    in return (newUsers, (length readyUsers, length users))
                               -- if all users are ready, start the game.
                               when (allUsersCount > 1 && readyUsersCount == allUsersCount) $ do
                                 users <- readMVar serverUsers
                                 let players = playersFromUsers users
                                 modifyMVar_ serverGame $ \_ -> return $ Just $ Game Nothing players InProgress serverGameConfig
                                 writeChan internalChan (GameReadySignal serverGameConfig (M.elems players))

    PlayerExit clientId -> do usersCount <- modifyMVar serverUsers $ \users -> do
                                let leftUsers = filter (\user -> (userId user) /= clientId) users
                                return (leftUsers, length leftUsers)
                              when (usersCount < 2) $ do outmsgs <- runGame serverGame $ PlayerQuit (userIdToPlayerNick clientId)
                                                         mapM_ (writeChan clientsChan) outmsgs

    PlayerTurnLeft clientId -> do outmsgs <- runGame serverGame $ TurnLeft (userIdToPlayerNick clientId)
                                  mapM_ (writeChan clientsChan) outmsgs
    PlayerTurnRight clientId -> do outmsgs <- runGame serverGame $ TurnRight (userIdToPlayerNick clientId)
                                   mapM_ (writeChan clientsChan) outmsgs

  handleIncomingMessages server

playersFromUsers :: [User] -> M.Map PlayerNick Player
playersFromUsers users = M.fromList $ map (\u -> let nick = userIdToPlayerNick . userId $ u
                                                     player = Player nick Alive (0,0) North []
                                                 in (nick, player)) users

userIdToPlayerNick :: UserID -> PlayerNick
userIdToPlayerNick = PlayerNick . T.unpack . getUserID

runGame :: MVar (Maybe Game) -> InputEvent -> IO ([OutMessage])
runGame serverGame event = do mgame <- readMVar serverGame
                              case mgame of
                                Nothing -> return []
                                Just game -> do let (outevents, game') = Engine.runEngine Engine.gameEngine game [event]
                                                modifyMVar serverGame $ \_ -> return (Just game', outEventsToOutMsgs outevents)

outEventsToOutMsgs :: [OutEvent] -> [OutMessage]
outEventsToOutMsgs outEvents = map encode outEvents
  where encode oevent = case oevent of
          Game.PlayerMoved nick coord orien -> Server.PlayerMoved (playerNickToUserId nick) coord orien
          Game.PlayerDied nick coord -> Server.PlayerDied (playerNickToUserId nick) coord
          Game.GameEnded nick -> Server.GameEnded (playerNickToUserId nick)

playerNickToUserId :: PlayerNick -> UserID
playerNickToUserId = UserID . T.pack . getPlayerNick
