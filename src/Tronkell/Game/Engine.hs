{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Tronkell.Types
import Tronkell.Game.Types

gameEngine :: GameEngine
gameEngine = fmap concat . mapM runEvent

runEngine :: GameEngine -> Game -> [InputEvent] -> ([OutEvent], Game)
runEngine engine game inEvents = runState (engine inEvents) game

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = do
  game <- get

  if not $ isValidEvent inputEvent game
  then return []
  else if not $ isGameInProgress game
  then return []
  else do
    outEvents <- case inputEvent of
      TurnLeft  nick -> turnLeft nick
      TurnRight nick -> turnRight nick
      Tick           -> tick
      PlayerQuit nick -> playerQuit nick

    statusEvents <- setGameStatus
    return $ outEvents ++ statusEvents

setGameStatus :: State Game [OutEvent]
setGameStatus = do
  game' <- get
  let alivePlayers   = filter ((== Alive) . playerStatus) $ Map.elems (gamePlayers game')
      noAlivePlayers = length alivePlayers
      winner         = if noAlivePlayers == 1 then Just (head alivePlayers) else Nothing
      status         = if noAlivePlayers > 1 then InProgress else Finished

  put game' { gameStatus  = status,
              gameWinner  = winner
            }
  return $ case status of
    InProgress -> []
    Finished -> [GameEnded (fmap playerNick winner)]

isValidEvent :: InputEvent -> Game -> Bool
isValidEvent event game = case event of
  Tick           -> True
  TurnLeft  nick -> isPlayerAlive nick game
  TurnRight nick -> isPlayerAlive nick game
  PlayerQuit nick -> isPlayerAlive nick game

isGameInProgress :: Game -> Bool
isGameInProgress = (== InProgress) . gameStatus

isPlayerAlive :: PlayerNick -> Game -> Bool
isPlayerAlive nick =
  maybe False ((== Alive) . playerStatus)
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
                      North -> (x, y-1)
                      East  -> (x+1, y)
                      South -> (x, y+1)
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

playerQuit :: PlayerNick -> State Game [OutEvent]
playerQuit nick = do
  game@Game{..} <- get
  let player         = fromJust . Map.lookup nick $ gamePlayers
      newPlayer      = player { playerStatus = Dead }
      newGamePlayers = Map.insert nick newPlayer gamePlayers
  put game { gamePlayers = newGamePlayers }
  return [PlayerDied nick (playerCoordinate newPlayer)]

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head . drop (turnTimes + fromEnum i) . cycle $ orientations

turn :: (Orientation -> Orientation) -> PlayerNick -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@Game{..} <- get
  let player         = fromJust . Map.lookup nick $ gamePlayers
      newOrientation = getNewOrientation . playerOrientation $ player
      newPlayer      = player { playerOrientation = newOrientation }
      newGamePlayers = Map.insert nick newPlayer gamePlayers
      newGame        = game { gamePlayers = newGamePlayers }
  put newGame
  return [PlayerMoved nick (playerCoordinate newPlayer) newOrientation]

orientations :: [Orientation]
orientations = [minBound..maxBound]
