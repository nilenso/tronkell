module Message exposing (..)

import Grid.Model as GM
import Grid.Message as GMsg
import Grid.Player as GP

type alias GameWidth = Float
type alias GameHeight = Float

type alias PlayerCells = List GM.PlayerCell

type Msg =
   -- From UI:
   -- Player Init and Game Moves
    PlayerName GP.PlayerName
   | RegisterNick
   | PlayerReady
   | PlayerQuit
   | MoveLeft
   | MoveRight
   -- From Server:
   -- Game State Related
   | GameReady GameWidth GameHeight PlayerCells
   | GameEnded (Maybe GP.PlayerId)
   | ServerMsg String
   -- Game Related
   | GridMsg GMsg.Msg
   | NoOp
