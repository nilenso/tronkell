module Tronkell.Game.Types where

data Game = Game { gameWinner :: Maybe Player
                 , gamePlayers :: [Player]
                 , gameStatus :: GameStatus
                 }

data GameStatus = Starting | Started | Finished

data Player = Player { playerNick :: String
                     , playerStatus :: PlayerStatus
                     , playerCoordinate :: Coordinate
                     , playerOrientation :: Orientation
                     , playerTrail :: Trail
                     }

data PlayerStatus = Joined | Ready | Playing | Dead

data Orientation = North | East | West | South

type Coordinate = (Int, Int)

type Trail = [Coordinate]
