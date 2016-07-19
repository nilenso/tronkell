module Tronkell.Game.Types where

import Control.Monad.State
import Data.Map

data GameConfig = GameConfig { gameWidth          :: Int
                             , gameHeight         :: Int
                             , gamePlayerSpeed    :: Int
                             , gameTicksPerSecond :: Int
                             } deriving (Show)

data Game = Game { gameWinner  :: Maybe Player
                 , gamePlayers :: Map PlayerNick Player
                 , gameStatus  :: GameStatus
                 , gameConfig  :: GameConfig
                 } deriving (Show)

data GameStatus = InProgress | Finished
                  deriving (Eq, Enum, Show)

newtype PlayerNick = PlayerNick { getPlayerNick :: String }
                     deriving (Eq, Ord, Show)

data Player = Player { playerNick        :: PlayerNick
                     , playerStatus      :: PlayerStatus
                     , playerCoordinate  :: Coordinate
                     , playerOrientation :: Orientation
                     , playerTrail       :: Trail
                     } deriving (Show)

data PlayerStatus = Alive | Dead
                    deriving (Show, Eq, Enum)

data Orientation = North | East | South | West
                   deriving (Eq, Enum, Bounded, Show)

type Coordinate = (Int, Int)

type Trail = [Coordinate]

data InputEvent = Tick
                | TurnLeft  PlayerNick
                | TurnRight PlayerNick
                deriving (Show)

data OutEvent = PlayerMoved PlayerNick Coordinate Orientation
              | PlayerDied  PlayerNick Coordinate
              | GameEnded   PlayerNick
              deriving (Show)

-- type GameEngine = [InputEvent] -> Game -> ([OutEvent], Game)
type GameEngine = [InputEvent] -> State Game [OutEvent]
