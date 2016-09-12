module Tronkell.Game.Types where

import Control.Monad.State.Strict
import Data.Map
import qualified Data.Text as T

import Tronkell.Types

data GameConfig = GameConfig { gameWidth          :: Int
                             , gameHeight         :: Int
                             , gamePlayerSpeed    :: Int
                             , gameTicksPerSecond :: Int
                             } deriving (Show)

data Game = Game { gameWinner  :: Maybe Player
                 , gamePlayers :: Map PlayerId Player
                 , gameStatus  :: GameStatus
                 , gameConfig  :: GameConfig
                 } deriving (Show)

data GameStatus = InProgress | Finished
                  deriving (Eq, Enum, Show)

newtype PlayerNick = PlayerNick { getPlayerNick :: T.Text }
                     deriving (Eq, Ord, Show)

newtype PlayerId = PlayerId { getPlayerId :: Int }
                   deriving (Eq, Ord, Show)

data Player = Player { playerId          :: PlayerId
                     , playerNick        :: PlayerNick
                     , playerStatus      :: PlayerStatus
                     , playerCoordinate  :: Coordinate
                     , playerOrientation :: Orientation
                     , playerTrail       :: Trail
                     } deriving (Show)

data PlayerStatus = Alive | Dead
                    deriving (Show, Eq, Enum)

type Trail = [Coordinate]

data InputEvent = Tick
                | TurnLeft  PlayerId
                | TurnRight PlayerId
                | PlayerQuit PlayerId
                deriving (Show)

data OutEvent = PlayerMoved PlayerId Coordinate Orientation
              | PlayerDied  PlayerId Coordinate
              | GameEnded   (Maybe PlayerId)
              deriving (Show, Eq, Ord)

-- type GameEngine = [InputEvent] -> Game -> ([OutEvent], Game)
type GameEngine = [InputEvent] -> State Game [OutEvent]
