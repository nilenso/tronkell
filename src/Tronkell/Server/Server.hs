{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types as Server
import Tronkell.Game.Types as Game
import Tronkell.Types
import Tronkell.Game.Engine as Engine
import Tronkell.Network.Websockets as W

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Text as T
import Data.Maybe (fromJust)

import Control.Monad (void, foldM, forever)
import Control.Monad.Fix (fix)
import qualified Data.Map as M

startServer :: Game.GameConfig -> IO ()
startServer gConfig = do
  firstUId              <- newMVar $ UserID 0
  playersVar            <- newMVar M.empty
  networkInChan         <- newChan
  clientSpecificOutChan <- newChan
  serverChan            <- atomically newTChan
  clientsChan           <- newChan
  internalChan          <- newChan

  let networkChans = (networkInChan, clientSpecificOutChan)
      server = Server gConfig playersVar networkChans serverChan clientsChan internalChan

  -- start game server : wrong : server after game has started.
  void $ forkIO $ void $ handleIncomingMessages server Nothing

  -- start websockets
  dupClientsChanWS <- dupChan clientsChan
  void $ forkIO $ W.start firstUId networkChans dupClientsChanWS

  clientsLoop server M.empty

clientsLoop :: Server -> M.Map UserID (TChan InMessage) -> IO ()
clientsLoop server@Server{..} userChans = do
  let (networkInChan, _) = networkChans
  msg <- readChan networkInChan
  case msg of
    PlayerJoined uId -> do
      userChan <- atomically newTChan
      let userChans' = M.insert uId userChan userChans -- check if uId already not there
      forkIO $ runClient uId userChan server
      clientsLoop server userChans'
    _ -> do
      let uId = getUserId msg
          userChan = M.lookup uId userChans
      maybe (return ()) (\c -> atomically $ writeTChan c msg) userChan
      clientsLoop server userChans

-- tcpMainLoop :: Server -> IO ()
-- tcpMainLoop server@Server{..} = do
--   (clientHdl, _, _) <- accept serverSocket
--   hSetBuffering clientHdl NoBuffering
--   forkIO $ runClient clientHdl server
--   tcpMainLoop server

runClient :: UserID -> TChan InMessage -> Server -> IO ()
runClient uId clientChan server@Server{..} = do
  let (_, clientSpecificOutChan) = networkChans
  writeChan clientSpecificOutChan $ (uId, ServerMsg "Take a nick name : ")
  msg <- atomically $ readTChan clientChan
  let nick = case msg of
               PlayerName _ name -> Just name
               _ -> Nothing

  case nick of
    Nothing -> runClient uId clientChan server
    Just _ -> do
      let user = User uId nick Waiting
      failedToAdd <- modifyMVar serverUsers $ \users ->
        -- for player we still have name as id ; so keep it unique.
        if isNickTaken users nick
        then return (users, True)
        else return (M.insert uId user users, False)

      if failedToAdd
      then runClient uId clientChan server
      else do atomically $ writeTChan serverChan $ PlayerJoined uId
              writeChan clientSpecificOutChan $ (uId, ServerMsg $ "Hi.. " ++ T.unpack (fromJust nick) ++ ".. Type ready when you are ready to play.. quit to quit.")
              fix $ \loop -> do m <- atomically $ readTChan clientChan
                                case m of
                                 PlayerReady _ -> playClient uId clientChan server
                                 PlayerExit  _ -> atomically $ writeTChan serverChan $ PlayerExit uId
                                 _ -> loop
  where
    isNickTaken users nick = any (\u -> nick == userNick u) users

cleanString :: String -> String
cleanString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse

playClient :: UserID -> TChan InMessage -> Server -> IO ()
playClient clientId inChan Server{..} = do
    -- because every client wants same copy of the message, duplicate channel.
  outClientChan <- dupChan clientsChan
  writeChan outClientChan $ ServerMsg "Waiting for other players to start the game...!!!"

  let (_, clientSpecificOutChan) = networkChans
  writer <- forkIO $ forever $ do
    outMsg <- readChan outClientChan
    writeChan clientSpecificOutChan (clientId, outMsg)

  clientInternalChan <- dupChan internalChan

  -- otherwise gameready msg for this second client joining is lost.
  atomically $ writeTChan serverChan $ PlayerReady clientId

  -- block on ready-signal from server-thread to start the game.
  signal <- readChan clientInternalChan
  case signal of
    GameReadySignal config players -> writeChan outClientChan $ GameReady config players

  writeList2Chan outClientChan [ ServerMsg "Here.. you go!!!"
                               , ServerMsg "Movements: type L for left , R for right, Q for quit... enjoy." ]

  -- flush all accumulated messages till now before allowing to play the game.
  _ <- readMsgs inChan

  fix $ \loop ->
    do
      msg <- atomically $ readTChan inChan
      case msg of
        PlayerExit _ -> void $ writeChan outClientChan $ ServerMsg "Sayonara !!!"
        _ -> atomically (writeTChan serverChan msg) >> loop

  killThread writer
  atomically $ writeTChan serverChan (PlayerExit clientId)

-- Adds user to user list and returns whether all users are ready
updateUserReady :: UserID -> M.Map UserID User -> IO (M.Map UserID User, Bool)
updateUserReady clientId users =
  let newUsers   = M.adjust (\u -> u { userState = Ready }) clientId users
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

processMessages :: Server -> Maybe Game -> [InMessage] -> IO (Maybe Game)
processMessages server@Server{..} game inMsgs = foldM threadGameOverEvent game inMsgs
  where threadGameOverEvent g' inMsg = case inMsg of
          PlayerJoined _           -> return game
          PlayerReady clientId     -> processPlayerReady clientId
          PlayerTurnLeft  clientId -> processEvent' g' TurnLeft clientId
          PlayerTurnRight clientId -> processEvent' g' TurnRight clientId
          PlayerExit      clientId -> do
            modifyMVar_ serverUsers (return . M.delete clientId)
            processEvent' g' PlayerQuit clientId
          PlayerName _ _           -> return game

        processPlayerReady clientId = case game of
          Just game' -> return $ Just game'
          Nothing -> do
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

        processEvent' game' evCons clientId = do
          let event = evCons . PlayerId . getUserID $ clientId
          processEvent server game' event

processEvent :: Server -> Maybe Game -> InputEvent -> IO (Maybe Game)
processEvent Server{..} g event = do
  users               <- readMVar serverUsers
  let (outEvs, game') = runGame g event
      outMsgs         = outEventToOutMessage users <$> outEvs
  writeList2Chan clientsChan outMsgs
  return game'

handleIncomingMessages :: Server -> Maybe Game -> IO Game
handleIncomingMessages server@Server{..} game = do
  threadDelay . quot oneSecond . gameTicksPerSecond $ serverGameConfig
  inMsgs <- readMsgs serverChan
  game' <- processMessages server game inMsgs
  game'' <- processEvent server game' Tick

  if isGameFinished game''
  then return . fromJust $ game''
  else handleIncomingMessages server game''

  where
    isGameFinished g = case g of
     Nothing -> False
     Just g'  -> Finished == gameStatus g'

playersFromUsers :: M.Map UserID User -> M.Map PlayerId Player
playersFromUsers = foldl userToPlayer M.empty
  where
    userToPlayer players u =
      let nick   = PlayerNick . fromJust . userNick $ u
          pid    = PlayerId . getUserID . userId $ u
          player = Player pid nick Alive (10,10) North []
      in M.insert pid player players

runGame :: Maybe Game -> InputEvent -> ([OutEvent], Maybe Game)
runGame game event =
  case game of
    Nothing -> ([], Nothing)
    Just g  -> Just <$> Engine.runEngine Engine.gameEngine g [event]

outEventToOutMessage :: M.Map UserID User -> OutEvent -> OutMessage
outEventToOutMessage users event =
  case event of
    Game.PlayerMoved pid coord orien -> Server.PlayerMoved (playerIdToUserId pid) coord orien
    Game.PlayerDied  pid coord       -> Server.PlayerDied  (playerIdToUserId pid) coord
    Game.GameEnded   pid             -> Server.GameEnded   (playerIdToUserId <$> pid)

playerIdToUserId :: PlayerId -> UserID
playerIdToUserId = UserID . getPlayerId

getUserId :: InMessage -> UserID
getUserId msg =
  case msg of
    PlayerJoined    uId -> uId
    PlayerReady     uId -> uId
    PlayerExit      uId -> uId
    PlayerTurnLeft  uId -> uId
    PlayerTurnRight uId -> uId
    PlayerName      uId _ -> uId
