module Message exposing (..)

import Grid.Model as GM

type Msg = GeneratePlayers
         | RandomPlayers (List (Int, String, List Int, (Int, Int)))
         | GridMsg GM.Msg
         | PlayerNameInput GM.PlayerName
         | PlayerReadyInput
         | PlayerQuitInput
