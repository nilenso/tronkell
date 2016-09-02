module Message exposing (..)

import Grid.Model as GM

type alias GameWidth = Float
type alias GameHeight = Float

type Msg = GeneratePlayers -- would go away
         | RandomPlayers (List (Int, String, List Int, (Int, Int))) -- would go away
         -- From UI:
         -- Player Init
         | PlayerName GM.PlayerName
         | PlayerReady
         | PlayerQuit
         | MoveLeft
         | MoveRight
         -- From Server:
         -- Game State Related
         | GameReady GameWidth GameHeight (List GM.Player)
         | GameEnded (Maybe GM.PlayerId)
         | ServerMsg String
         -- Game Related
         | GridMsg GM.Msg
