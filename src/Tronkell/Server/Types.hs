module Tronkell.Server.Types where

import Tronkell.Types
import Tronkell.Game.Types as Game
import Control.Concurrent.STM (TChan)
import Control.Concurrent (MVar, Chan)
import qualified Data.Map as M
import qualified Data.Text as T

type NetworkChans = (Chan InMessage, Chan (UserID, OutMessage))

data Server = Server { serverGameConfig :: Game.GameConfig
                     , serverUsers      :: MVar (M.Map UserID User)
                     , networkChans     :: NetworkChans
                     , serverChan       :: TChan InMessage
                     , clientsChan      :: Chan OutMessage
                     , internalChan     :: Chan ServerSignals
                     }

newtype UserID = UserID { getUserID :: Int }
                 deriving (Eq, Show, Ord)

data User = User { userId    :: UserID
                 , userNick  :: Maybe T.Text
                 , userState :: UserStatus
                 } deriving (Show)

data UserStatus = Waiting | Ready
                  deriving (Eq, Enum, Bounded, Show)

data InMessage = PlayerJoined    UserID -- should not these be UserJoined ?
               | PlayerReady     UserID
               | PlayerExit      UserID
               | PlayerTurnLeft  UserID
               | PlayerTurnRight UserID
               | PlayerName      UserID T.Text
               deriving (Show)

data OutMessage = GameReady   Game.GameConfig [Game.Player]
                | PlayerRegisterId UserID
                | PlayerMoved UserID Coordinate Orientation
                | PlayerDied  UserID Coordinate
                | GameEnded   (Maybe UserID)
                | ServerMsg   String
                deriving (Show)

data ServerSignals = GameReadySignal Game.GameConfig [Game.Player]
