{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types as Server
import Tronkell.Game.Types as Game
import Tronkell.Types
import Tronkell.Game.Engine as Engine

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Text as T

import Control.Monad (void, when, foldM)
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

  playersVar <- newMVar []
  serverChan <- atomically $ newTChan
  clientsChan <- newChan
  internalChan <- newChan

  let server = Server gConfig playersVar sock serverChan clientsChan internalChan

  forkIO $ handleIncomingMessages server Nothing

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
isNickTaken nick = elem (nickToUserId nick) . map userId

mkUser :: String -> User
mkUser nick = User (nickToUserId nick) Waiting

runClient :: Handle -> Server -> ClientId -> IO ()
runClient clientHdl server@Server{..} clientId = do
  hPutStr clientHdl "Take a nick name : "
  nick <- cleanString <$> hGetLine clientHdl
  let userId = nickToUserId nick
  failedToAdd <- modifyMVar serverUsers $ \users ->
    if isNickTaken nick users
    then return (users, True)
    else return (mkUser nick : users, False)

  if failedToAdd
  then runClient clientHdl server clientId
  else do atomically $ writeTChan serverChan $ PlayerJoined userId
          hPutStrLn clientHdl $ "Hi.. " ++ nick ++ ".. Type ready when you are ready to play.. quit to quit."
          fix $ \loop -> do ready <- cleanString <$> hGetLine clientHdl
                            case ready of
                             "ready" -> playClient userId clientHdl server
                             "quit" -> atomically $ writeTChan serverChan $ PlayerExit userId
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
    hPrint clientHdl outmsg
    loop

  clientInternalChan <- dupChan internalChan

  -- only after duplicating the internalChan, send the PlayerReady msg,
  -- otherwise gameready msg for this second client joining is lost.
  atomically $ writeTChan serverChan $ PlayerReady clientId

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
        inmsg <- cleanString <$> hGetLine clientHdl
        case decodeMessage clientId inmsg of
          Just (PlayerExit _) -> void $ hPutStrLn clientHdl "Sayonara !!!"
          Just msg -> atomically (writeTChan serverChan msg) >> loop
          Nothing -> loop

  killThread writer

  atomically $ writeTChan serverChan (PlayerExit clientId)
  hClose clientHdl

decodeMessage :: UserID -> String -> Maybe InMessage
decodeMessage userId msg = case msg of
  "L" -> Just $ PlayerTurnLeft userId
  "R" -> Just $ PlayerTurnRight userId
  "Q" -> Just $ PlayerExit userId
  _   -> Nothing

-- Adds user to user list and returns whether all users are ready
updateUserReady :: UserID -> [User] -> IO ([User], Bool)
updateUserReady clientId users =
  let newUsers   = map (\u -> if userId u == clientId then u{ userState = Ready } else u) users
      -- We have at least 2 users, and all users are ready
      ready      = length users > 1 && all ((Ready ==) . userState) newUsers
  in return (newUsers, ready)

readMsgs :: TChan a -> IO [a]
readMsgs channel = atomically $ readAll channel
  where readAll c = do
          emptyChan <- isEmptyTChan c
          if emptyChan
          then return []
          else (:) <$> readTChan c <*> readAll c

oneSecond :: Int
oneSecond = 1000000

processEvents :: Server -> Maybe Game -> [InMessage] -> IO (Maybe Game)
processEvents server@Server{..} game inMsgs = foldM threadGameOverEvent game inMsgs
  where threadGameOverEvent g' inMsg = case inMsg of
          PlayerJoined _           -> return Nothing -- bug : should not process this in ongoing event.
          PlayerReady clientId     -> processPlayerReady clientId
          PlayerTurnLeft  clientId -> processEvent server g' $ TurnLeft (userIdToPlayerNick clientId)
          PlayerTurnRight clientId -> processEvent server g' $ TurnRight (userIdToPlayerNick clientId)
          PlayerExit      clientId -> do
            modifyMVar_ serverUsers (return . filter ((clientId /=) . userId))
            processEvent server g' $ PlayerQuit (userIdToPlayerNick clientId)

        processPlayerReady clientId = do
          ready <- modifyMVar serverUsers $ updateUserReady clientId
          -- if all users are ready, start the game.
          if ready
          then do
            users <- readMVar serverUsers
            let players = playersFromUsers users
            writeChan internalChan (GameReadySignal serverGameConfig (M.elems players))
            return $ Just $ Game Nothing players InProgress serverGameConfig
          else
            return Nothing

processEvent :: Server -> Maybe Game -> InputEvent -> IO (Maybe Game)
processEvent Server{..} g event = do
  let (outmsgs, game') = runGame g event
  writeList2Chan clientsChan outmsgs
  return game'

handleIncomingMessages :: Server -> Maybe Game -> IO ()
handleIncomingMessages server@Server{..} game = do
  threadDelay . quot oneSecond . gameTicksPerSecond $ serverGameConfig
  inMsgs <- readMsgs serverChan
  game' <- processEvents server game inMsgs
  game'' <- processEvent server game' Tick
  handleIncomingMessages server game''

playersFromUsers :: [User] -> M.Map PlayerNick Player
playersFromUsers = M.fromList . map userToPlayer
  where
    userToPlayer u =
      let nick   = userIdToPlayerNick . userId $ u
          player = Player nick Alive (10,10) North []
      in (nick, player)

userIdToPlayerNick :: UserID -> PlayerNick
userIdToPlayerNick = PlayerNick . T.unpack . getUserID

runGame :: Maybe Game -> InputEvent -> ([OutMessage], Maybe Game)
runGame game event =
  case game of
    Nothing -> ([], Nothing)
    Just g  -> (\(es, g') -> (outEventToOutMessage <$> es, Just g')) $ Engine.runEngine Engine.gameEngine g [event]

outEventToOutMessage :: OutEvent -> OutMessage
outEventToOutMessage event =
  case event of
    Game.PlayerMoved nick coord orien -> Server.PlayerMoved (playerNickToUserId nick) coord orien
    Game.PlayerDied  nick coord       -> Server.PlayerDied  (playerNickToUserId nick) coord
    Game.GameEnded   nick             -> Server.GameEnded   (fmap playerNickToUserId nick)

playerNickToUserId :: PlayerNick -> UserID
playerNickToUserId = UserID . T.pack . getPlayerNick
