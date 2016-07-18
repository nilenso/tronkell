{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State
import Tronkell.Game.Types
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map

gameEngine :: GameEngine
gameEngine = fmap concat . mapM runEvent

runEngine :: GameEngine -> Game -> [InputEvent] -> ([OutEvent], Game)
runEngine engine game inEvents = runState (engine inEvents) game

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = do
  game@(Game _ ps _ _ ) <- get

  if not $ isValidEvent inputEvent game
  then return []
  else do
    let deadPlayers  = Map.filter (\p -> playerStatus p == Dead) ps
        alivePlayers = Map.filter (\p -> playerStatus p == Alive) ps
    put game {gamePlayers = alivePlayers}
    os <- case inputEvent of
      TurnLeft  nick -> turnLeft nick
      TurnRight nick -> turnRight nick
      Tick           -> tick
    curGame <- get
    put curGame { gamePlayers = Map.union (gamePlayers curGame) deadPlayers }
    return os

isValidEvent :: InputEvent -> Game -> Bool
isValidEvent event game = case event of
  Tick           -> True
  TurnLeft  nick -> isPlayerAlive nick game
  TurnRight nick -> isPlayerAlive nick game

isPlayerAlive :: PlayerNick -> Game -> Bool
isPlayerAlive nick =
  fromMaybe False
  . fmap ((== Alive) . playerStatus)
  . Map.lookup nick
  . gamePlayers

stopAtBoundary :: GameConfig -> Coordinate -> Coordinate
stopAtBoundary (GameConfig w h _ _) (x, y) =
  (min (w - 1) (max x 0),
   min (h - 1) (max y 0))

computePlayerStatus :: GameConfig -> Coordinate -> PlayerStatus
computePlayerStatus (GameConfig w h _ _) (x, y) =
  if x < 0 || x >= w || y < 0 || y >= h
  then Dead
  else Alive

movePlayerForward :: GameConfig -> Player -> Player
movePlayerForward gameConfig player@Player{..} =
  let (x,y)       = playerCoordinate
      newPosition = case playerOrientation of
                      North -> (x, y+1)
                      East  -> (x+1, y)
                      South -> (x, y-1)
                      West  -> (x-1, y)
  in player { playerCoordinate = stopAtBoundary gameConfig newPosition,
              playerStatus     = computePlayerStatus gameConfig newPosition }

tick :: State Game [OutEvent]
tick  = do
  game@Game{..} <- get
  let movedPlayers = Map.map (movePlayerForward gameConfig) gamePlayers
      newGame      = game { gamePlayers = movedPlayers }
      playerToPlayerMove (Player n _ c o _) = PlayerMoved n c o
  put newGame
  return . map playerToPlayerMove . Map.elems $ movedPlayers

turnRight :: PlayerNick -> State Game [OutEvent]
turnRight = turn (getNextEnum 1)

turnLeft :: PlayerNick -> State Game [OutEvent]
turnLeft = turn (getNextEnum 3)

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head. drop (turnTimes + (fromEnum i)) . cycle $ orientations

turn :: (Orientation -> Orientation) -> PlayerNick -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@Game{..} <- get
  let player         = fromJust . Map.lookup nick $ gamePlayers
      newPlayer      = player { playerOrientation = getNewOrientation . playerOrientation $ player}
      newGamePlayers = Map.insert nick newPlayer gamePlayers
      newGame        = game { gamePlayers = newGamePlayers }
  put newGame
  return []

orientations :: [Orientation]
orientations = [minBound..maxBound]
