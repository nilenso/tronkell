module Tronkell.Server.Types where

import Tronkell.Types
import Tronkell.Game.Types as Game
import Control.Concurrent
import Network.Socket
import qualified Data.Text as T

data Server = Server { serverGameConfig :: Game.GameConfig
                     , serverGame       :: MVar (Maybe Game.Game)
                     , serverUsers      :: MVar [User]
                     , serverSocket     :: Socket
                     , serverChan       :: Chan InMessage
                     , clientsChan      :: Chan OutMessage
                     }

newtype UserID = UserID { getUserID :: T.Text }
                 deriving (Eq, Show, Ord)

data User = User { userId    :: UserID
                 , userState :: UserStatus
                 } deriving (Show)

data UserStatus = Waiting | Ready
                  deriving (Eq, Enum, Bounded, Show)

data InMessage = PlayerJoined    UserID
               | PlayerReady     UserID
               | PlayerExit      UserID
               | PlayerTurnLeft  UserID
               | PlayerTurnRight UserID
               deriving (Show)

data OutMessage = GameReady   Game.GameConfig [Game.Player]
                | PlayerMoved UserID Coordinate Orientation
                | PlayerDied  UserID Coordinate
                | GameEnded   UserID
                deriving (Show)
