module Tronkell.Game.Types where

data GameConfig = GameConfig { gameWidth :: Int
                             , gameHeight :: Int
                             , gamePlayerSpeed :: Int
                             , gameTicksPerSecond :: Int
                             }

data Game = Game { gameWinner :: Maybe Player
                 , gamePlayers :: [Player]
                 , gameStatus :: GameStatus
                 , gameConfig :: GameConfig
                 }

data GameStatus = InProgress | Finished

newtype PlayerNick = PlayerNick String

data Player = Player { playerNick :: PlayerNick
                     , playerStatus :: PlayerStatus
                     , playerCoordinate :: Coordinate
                     , playerOrientation :: Orientation
                     , playerTrail :: Trail
                     }

data PlayerStatus = Alive | Dead

data Orientation = North | East | West | South

type Coordinate = (Int, Int)

type Trail = [Coordinate]

data InputEvent = Tick
                | TurnLeft PlayerNick
                | TurnRight PlayerNick

data OutEvent = PlayerMoved PlayerNick Coordinate Orientation
              | PlayerDied PlayerNick Coordinate
              | GameEnded PlayerNick
