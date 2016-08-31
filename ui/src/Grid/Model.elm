module Grid.Model exposing (..)

import Color exposing (Color)

type alias PlayerId = Int
type alias PlayerName = String
type alias Player =
    { id : PlayerId
    , name : PlayerName
    , color : Color
    }

type CellType = EmptyCell | PlayerCell Player | Trail
type alias Cell =
         { ctype : CellType
         , x     : Int
         , y     : Int
         }

type alias Grid = List (List Cell)

type Msg = NoOp

gridToList : Grid -> List Cell
gridToList grid = List.concatMap identity grid

generateGrid : List (Int, String, List Int, (Int, Int)) -> Grid
generateGrid playersData =
    let playersCell = List.map genPlayerCell playersData
        emptyCells  = List.map (\ (x,y) -> Cell EmptyCell x y) [(0,0), (1,1), (2,2)]
    in Debug.log "playersCell: " [playersCell, emptyCells]

genPlayerCell : (Int, String, List Int, (Int, Int)) -> Cell
genPlayerCell (id, name, cs, (x', y')) =
    case cs of
        r::g::b::_ -> Cell (PlayerCell (Player id name (Color.rgb r g b))) x' y'
        _          -> Cell EmptyCell x' y'
