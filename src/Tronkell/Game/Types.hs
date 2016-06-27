module Tronkell.Game.Types where

import Control.Monad.State
import Data.Map

data GameConfig = GameConfig { gameWidth :: Int
                             , gameHeight :: Int
                             , gamePlayerSpeed :: Int
                             , gameTicksPerSecond :: Int
                             }

data Game = Game { gameWinner :: Maybe Player
                 , gamePlayers :: Map PlayerNick Player
                 , gameStatus :: GameStatus
                 , gameConfig :: GameConfig
                 }

data GameStatus = InProgress | Finished

newtype PlayerNick = PlayerNick String deriving (Eq, Ord)

data Player = Player { playerNick :: PlayerNick
                     , playerStatus :: PlayerStatus
                     , playerCoordinate :: Coordinate
                     , playerOrientation :: Orientation
                     , playerTrail :: Trail
                     }

data PlayerStatus = Alive | Dead

data Orientation = North | East | South | West deriving (Enum, Bounded, Show)

type Coordinate = (Int, Int)

type Trail = [Coordinate]

data InputEvent = Tick
                | TurnLeft PlayerNick
                | TurnRight PlayerNick

data OutEvent = PlayerMoved PlayerNick Coordinate Orientation
              | PlayerDied PlayerNick Coordinate
              | GameEnded PlayerNick

-- type GameEngine = [InputEvent] -> Game -> ([OutEvent], Game)
type GameEngine = [InputEvent] -> State Game [OutEvent]
