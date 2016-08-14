module Main where

import Control.Monad.State
import Tronkell.Types
import Tronkell.Game.Types
import Tronkell.Game.Engine
import qualified Data.Map as Map

import Tronkell.Server.Server

main :: IO ()
main = startServer $ GameConfig 100 100 1 1
  -- let p1 = Player (PlayerNick "player 1") Alive (1,1) North []
  --     p2 = Player (PlayerNick "player 2") Alive (2,2) North []
  --     config = GameConfig 3 3 1 1
  --     gamePs = Map.fromList [(playerNick p1, p1), (playerNick p2, p2)]
  --     game = Game Nothing gamePs InProgress config
  --     engine = gameEngine [TurnLeft (playerNick p1), Tick] in
  --   print $ runState engine game
