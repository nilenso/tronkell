module Model exposing (Model)

import Grid.Model as GM
import Grid.Player as GP

type alias Model =
    { grid     : Maybe GM.Grid
    , nick     : Maybe GP.PlayerName
    , myId     : Maybe GP.PlayerId
    , winnerId : Maybe GP.PlayerId
    }
