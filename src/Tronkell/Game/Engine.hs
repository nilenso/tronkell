{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State
import Tronkell.Game.Types
import Data.Maybe (fromJust)
import qualified Data.Map as Map

gameEngine :: GameEngine
gameEngine = fmap concat . mapM runEvent

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = do
  game@(Game _ ps _ _ ) <- get
  let deadPlayers = Map.filter (\(Player _ s _ _ _) -> s == Dead) ps
      alivePlayers = Map.filter (\(Player _ s _ _ _) -> s == Alive) ps
  put game {gamePlayers = alivePlayers}
  os <- case inputEvent of
             TurnLeft nick -> turnLeft nick
             TurnRight nick -> turnRight nick
             Tick -> tick
  curGame <- get
  put curGame {gamePlayers = Map.union (gamePlayers curGame) deadPlayers }
  return os

stopAtBoundary :: GameConfig -> Coordinate -> Coordinate
stopAtBoundary (GameConfig w h _ _) (x, y) =
  (min (w - 1) (max x 0),
   min (h - 1) (max y 0))

computePlayerStatus :: GameConfig -> Coordinate -> PlayerStatus
computePlayerStatus (GameConfig w h _ _) (x, y) =
  if x < 0 || x >= w || y < 0 || y >= h then Dead else Alive

movePlayerForward :: GameConfig -> Player -> Player
movePlayerForward gameConfig player@Player{..} =
  let (x,y) = playerCoordinate
      newPosition = case playerOrientation of
                      North -> (x, y+1)
                      East -> (x+1, y)
                      South -> (x, y-1)
                      West ->  (x-1, y)
  in player { playerCoordinate = stopAtBoundary gameConfig newPosition,
              playerStatus =  computePlayerStatus gameConfig newPosition }

tick :: State Game [OutEvent]
tick  = do
  game@Game{..} <- get
  let movedPlayers = Map.map (movePlayerForward gameConfig) gamePlayers
      newGame = game { gamePlayers = movedPlayers }
      playerToPlayerMove (Player n _ c o _) = PlayerMoved n c o
  put newGame
  return . map playerToPlayerMove . Map.elems $ movedPlayers

turnRight :: PlayerNick -> State Game [OutEvent]
turnRight = turn (getNextEnum 1)

turnLeft :: PlayerNick -> State Game [OutEvent]
turnLeft nick = turn (getNextEnum 3)

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head. drop (turnTimes + (fromEnum i)) . cycle $ orientations

turn :: (Orientation -> Orientation) -> PlayerNick -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@Game{..} <- get
  let player = fromJust . Map.lookup nick $ gamePlayers
      newPlayer = player { playerOrientation = getNewOrientation . playerOrientation $ player}
      newGamePlayers = Map.insert nick newPlayer gamePlayers
      newGame = game { gamePlayers =  newGamePlayers }
  put newGame
  return []

orientations :: [Orientation]
orientations = [minBound..maxBound]

runSimulation :: ([OutEvent], Game)
runSimulation =
  let p1 = Player (PlayerNick "player 1") Alive (1,1) North []
      p2 = Player (PlayerNick "player 2") Alive (2,2) North []
      config = GameConfig 3 3 1 1
      gamePs = Map.fromList [(playerNick p1, p1), (playerNick p2, p2)]
      game = Game Nothing gamePs InProgress config
      engine = gameEngine [TurnLeft (playerNick p1), Tick] in
    runState engine game
