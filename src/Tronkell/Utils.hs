{-# LANGUAGE RecordWildCards #-}

module Tronkell.Utils where

import Tronkell.Server.Types as Server
import Tronkell.Game.Types as Game
import Tronkell.Types()

import Control.Concurrent.STM (TChan, atomically, readTChan, isEmptyTChan)
import qualified Data.Map as M
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import System.Random

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
    UserExit        uId -> uId

outEventToOutMessage :: M.Map UserID User -> OutEvent -> OutMessage
outEventToOutMessage _ event =
  case event of
    Game.PlayerMoved pid coord orien -> Server.PlayerMoved (playerIdToUserId pid) coord orien
    Game.PlayerDied  pid coord       -> Server.PlayerDied  (playerIdToUserId pid) coord
    Game.GameEnded   pid             -> Server.GameEnded   (playerIdToUserId <$> pid)

playersFromUsers :: GameConfig -> M.Map UserID User -> IO (M.Map PlayerId Player)
playersFromUsers GameConfig{..} = foldM userToPlayer M.empty
  where
    userToPlayer players u = do
      let nick   = PlayerNick . fromJust . userNick $ u
          pid    = PlayerId . getUserID . userId $ u
          (wStart, wEnd) = toRandomRange gameWidth
          (hStart, hEnd) = toRandomRange gameHeight
      x <- getStdRandom (randomR (wStart, wEnd))
      y <- getStdRandom (randomR (hStart, hEnd))
      orien <- getStdRandom random

      let player = Player pid nick Alive (x, y) orien []
      return $ M.insert pid player players

    toRandomRange i = (round $ (fromIntegral i :: Double) / 4,
                       round $ (fromIntegral i :: Double) / 4 * 3)

readMsgs :: TChan a -> IO [a]
readMsgs channel = atomically $ readAll channel
  where readAll c = do
          emptyChan <- isEmptyTChan c
          if emptyChan
          then return []
          else (:) <$> readTChan c <*> readAll c
