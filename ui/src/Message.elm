module Message exposing (..)

import Grid.Model as GM
import Grid.Message as GMsg
import Grid.Player as GP

type alias GameWidth = Float
type alias GameHeight = Float

type alias PlayerCells = List GM.PlayerCell

type Msg =
    -- Messages for mocking server  -- would go away
     GeneratePlayers
   | RandomPlayers (List (Int, String, List Int, (Int, Int)))
   | MovePlayer GP.PlayerId GP.Position GP.Orientation
   | KillPlayer GP.PlayerId
   -- From UI:
   -- Player Init
   | PlayerName GP.PlayerName
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
