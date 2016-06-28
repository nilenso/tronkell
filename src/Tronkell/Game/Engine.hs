{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State
import Tronkell.Game.Types
import Data.Maybe (fromJust)
import qualified Data.Map as Map

gameEngine :: GameEngine
gameEngine inputEvents = do
  fmap concat . mapM runEvent $ inputEvents

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = case inputEvent of
  TurnLeft nick -> turnLeft nick
  TurnRight nick -> turnRight nick
  Tick -> moveForward

movePlayerForward :: Player -> Player
movePlayerForward player@(Player {..}) = let (x,y) = playerCoordinate
  in case playerOrientation of
       North -> player { playerCoordinate = (x, y+1) }
       East -> player { playerCoordinate = (x+1, y) }
       South -> player { playerCoordinate = (x, y-1) }
       West -> player { playerCoordinate = (x-1, y) }

moveForward :: State Game [OutEvent]
moveForward  = do
  game@(Game {..}) <- get
  let movedPlayers = Map.map movePlayerForward gamePlayers
      newGame = game { gamePlayers = movedPlayers }
      playerToPlayerMove (Player n _ c o _) = PlayerMoved n c o
  put newGame
  return . map playerToPlayerMove . Map.elems $ movedPlayers

turnRight :: PlayerNick -> State Game [OutEvent]
turnRight nick =
  turn (getNextEnum 1) nick

turnLeft :: PlayerNick -> State Game [OutEvent]
turnLeft nick =
  turn (getNextEnum 3) nick

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head. drop (turnTimes + (fromEnum i)) . cycle $ orientations

turn :: (Orientation -> Orientation) -> PlayerNick -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@(Game {..}) <- get
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
      config = GameConfig 10 10 1 1
      gamePs = Map.fromList [(playerNick p1, p1), (playerNick p2, p2)]
      game = Game Nothing gamePs InProgress config
      engine = gameEngine [TurnLeft (playerNick p1), Tick] in
    runState engine game
