{-# LANGUAGE RecordWildCards #-}

module Tronkell.Server.Server where

import Tronkell.Server.Types as Server
import Tronkell.Game.Types as Game
import Tronkell.Types
import Tronkell.Game.Engine as Engine

import Network (PortID(..), accept, listenOn, withSocketsDo)

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Text as T
import Data.Maybe (fromJust)

import Control.Monad (void, when, foldM)
import Control.Monad.Fix (fix)
import Control.Exception
import qualified Data.Map as M

import System.IO

startServer :: Game.GameConfig -> IO ()
startServer gConfig = withSocketsDo $ do
  sock         <- listenOn . PortNumber $ 4242

  firstUId     <- newMVar $ UserID 0
  playersVar   <- newMVar M.empty
  serverChan   <- atomically newTChan
  clientsChan  <- newChan
  internalChan <- newChan

  let server = Server gConfig firstUId playersVar sock serverChan clientsChan internalChan

  forkIO $ void $ handleIncomingMessages server Nothing

  mainLoop server

type ClientId = Int

mainLoop :: Server -> IO ()
mainLoop server@Server{..} = do
  (clientHdl, _, _) <- accept serverSocket
  hSetBuffering clientHdl NoBuffering
  forkIO $ runClient clientHdl server
  mainLoop server

runClient :: Handle -> Server -> IO ()
runClient clientHdl server@Server{..} = do
  hPutStr clientHdl "Take a nick name : "
  uId  <- modifyMVar serverLastUserId getNextUserID
  nick <- Just . T.pack . cleanString <$> hGetLine clientHdl
  let user = User uId nick Waiting
  failedToAdd <- modifyMVar serverUsers $ \users ->
    if isNickTaken users nick
    then return (users, True)
    else return (M.insert uId user users, False)

  if failedToAdd
  then runClient clientHdl server
  else do atomically $ writeTChan serverChan $ PlayerJoined uId
          hPutStrLn clientHdl $ "Hi.. " ++ T.unpack (fromJust nick) ++ ".. Type ready when you are ready to play.. quit to quit."
          fix $ \loop -> do ready <- cleanString <$> hGetLine clientHdl
                            case ready of
                             "ready" -> playClient uId clientHdl server
                             "quit" -> atomically $ writeTChan serverChan $ PlayerExit uId
                             _ -> loop
  where
    isNickTaken users nick = any (\u -> nick == userNick u) users
    getNextUserID uId =
      let
        next = UserID $ 1 + getUserID uId
      in
        return (next, next)

cleanString :: String -> String
cleanString = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse

playClient :: UserID -> Handle -> Server -> IO ()
playClient clientId clientHdl Server{..} = do
    -- because every client wants same copy of the message, duplicate channel.
  outClientChan <- dupChan clientsChan
  writeChan outClientChan $ ServerMsg "Waiting for other players to start the game...!!!"

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

  writeList2Chan outClientChan [ServerMsg "Here.. you go!!!",
                                 ServerMsg "Movements: type L for left , R for right, Q for quit... enjoy."]

  hFlush clientHdl
  
  handle (\(SomeException _) -> return ()) $ fix $ \loop ->
    do
      canRead <- hIsReadable clientHdl
      when canRead $ do
        inmsg <- cleanString <$> hGetLine clientHdl
        case decodeMessage clientId inmsg of
          Just (PlayerExit _) -> void $ writeChan outClientChan $ ServerMsg "Sayonara !!!"
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
          users    <- readMVar serverUsers
          let nick  = PlayerNick . fromJust . userNick . fromJust . M.lookup clientId $ users
              event = evCons nick
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

playersFromUsers :: M.Map UserID User -> M.Map PlayerNick Player
playersFromUsers = foldl userToPlayer M.empty
  where
    userToPlayer players u =
      let nick   = PlayerNick . fromJust . userNick $ u
          player = Player nick Alive (10,10) North []
      in M.insert nick player players

runGame :: Maybe Game -> InputEvent -> ([OutEvent], Maybe Game)
runGame game event =
  case game of
    Nothing -> ([], Nothing)
    Just g  -> Just <$> Engine.runEngine Engine.gameEngine g [event]

outEventToOutMessage :: M.Map UserID User -> OutEvent -> OutMessage
outEventToOutMessage users event =
  case event of
    Game.PlayerMoved nick coord orien -> Server.PlayerMoved (playerNickToUserId users nick) coord orien
    Game.PlayerDied  nick coord       -> Server.PlayerDied  (playerNickToUserId users nick) coord
    Game.GameEnded   nick             -> Server.GameEnded   (playerNickToUserId users <$> nick)

playerNickToUserId :: M.Map UserID User -> PlayerNick -> UserID
playerNickToUserId users nick = fst . M.elemAt 0 . M.filter (\u -> Just nick == (PlayerNick <$> userNick u)) $ users
