{-# LANGUAGE RecordWildCards #-}

module Tronkell.Game.Engine where

import Control.Monad.State
import Tronkell.Game.Types
import Data.Maybe (fromJust)
import qualified Data.Map as Map

orientations :: [Orientation]
orientations = [minBound..maxBound]

turn :: (Orientation -> Orientation) -> PlayerNick -> State Game [OutEvent]
turn getNewOrientation nick = do
  game@(Game {..}) <- get
  let player = fromJust . Map.lookup nick $ gamePlayers
      newPlayer = player { playerOrientation = getNewOrientation . playerOrientation $ player}
      newGamePlayers = Map.insert nick newPlayer gamePlayers
      newGame = game { gamePlayers =  newGamePlayers }
  put newGame
  return []

getNextEnum :: Int -> Orientation -> Orientation
getNextEnum turnTimes i = head. drop (turnTimes + (fromEnum i)) . cycle $ orientations

turnLeft :: PlayerNick -> State Game [OutEvent]
turnLeft nick =
  turn (getNextEnum 3) nick

turnRight :: PlayerNick -> State Game [OutEvent]
turnRight nick =
  turn (getNextEnum 1) nick

moveForward :: State Game [OutEvent]
moveForward = undefined

gameEngine :: GameEngine
gameEngine inputEvents = do
  fmap concat . mapM runEvent $ inputEvents

runEvent :: InputEvent -> State Game [OutEvent]
runEvent inputEvent = case inputEvent of
  TurnLeft nick -> turnLeft nick
  TurnRight nick -> turnRight nick
  Tick -> moveForward
