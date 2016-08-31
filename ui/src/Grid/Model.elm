module Grid.Model exposing (..)

type alias PlayerId = Int
type alias PlayerName = String
type alias Player =
    {
        playerId : PlayerId,
        playerName : PlayerName
    }

type CellType = EmptyCell | PlayerCell Player | Trail
type alias Cell =
         { ctype : CellType
         , x     : Int
         , y     : Int
         }

type alias Grid = List Cell

type Msg = NoOp

gridToList : Grid -> List Cell
gridToList grid = grid
