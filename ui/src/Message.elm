module Message exposing (..)

import Grid.Model as GM
import Grid.Message as GMsg
import Grid.Player as GP

type alias GameWidth = Float
type alias GameHeight = Float

type alias PlayerCells = List GM.Cell

type Msg = GeneratePlayers -- would go away
         | RandomPlayers (List (Int, String, List Int, (Int, Int))) -- would go away
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
