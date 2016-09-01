module Grid.Model exposing (..)

import Color exposing (Color)
import List.Extra as ListE

type alias PlayerId = Int
type alias PlayerName = String
type alias Position = (Int, Int)
type alias Trail = List Position

type alias Player =
    { id : PlayerId
    , name : PlayerName
    , color : Color
    , trail : Trail
    }

type CellType = EmptyCell | PlayerCell Player | Trail
type alias Cell =
         { ctype : CellType
         , x     : Float
         , y     : Float
         }

type alias Grid =
     { playerCells : List Cell
     , width       : Float
     , height      : Float
     }

type Msg = NoOp

init : Float -> Float -> Grid
init w h = Grid [] w h

gridToList : Grid -> List Cell
gridToList grid =
    List.map (\ (w, h) -> Cell EmptyCell w h) (ListE.lift2 (,) [0 .. grid.height - 1]  [0 .. grid.width - 1])

generateGrid : List (Int, String, List Int, (Int, Int)) -> Float -> Float -> Grid
generateGrid playersData w h =
    let playersCell = List.map genPlayerCell playersData
    in Debug.log "playersCell: " (Grid playersCell w h)

genPlayerCell : (Int, String, List Int, (Int, Int)) -> Cell
genPlayerCell (id, name, cs, (x', y')) =
    case cs of
        r::g::b::_ -> Cell (PlayerCell (Player id name (Color.rgb r g b) [])) (toFloat x') (toFloat y')
        _          -> Cell EmptyCell (toFloat x') (toFloat y')
