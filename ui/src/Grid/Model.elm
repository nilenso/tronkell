module Grid.Model exposing (..)

import Color exposing (Color)
import List.Extra as ListE

import Grid.Player exposing (..)
import Grid.Message exposing (..)

type CellType = EmptyCell | PlayerCell Player | Trail Color
type alias Cell =
         { ctype : CellType
         , x     : Float
         , y     : Float
         }

type alias Grid =
     { width       : Float
     , height      : Float
     , playerCells : List Cell
     }

init : Float -> Float -> List Cell -> Grid
init w h playerCells = Grid w h playerCells

gridToList : Grid -> List Cell
gridToList grid =
    let cellToTrailCell cell = case cell.ctype of
                                   PlayerCell p -> List.map (\ (x,y) -> Cell (Trail p.color) x y)  p.trail
                                   _ -> []
    in List.concat [ (List.map (\ (w, h) -> Cell EmptyCell w h) (ListE.lift2 (,) [0 .. grid.height - 1]  [0 .. grid.width - 1]))
                   , grid.playerCells
                   , (List.concatMap cellToTrailCell grid.playerCells)
                   ]

generateGrid : List (Int, String, List Int, (Int, Int)) -> Float -> Float -> Grid
generateGrid playersData w h =
    let playersCell = List.map genPlayerCell playersData
    in Debug.log "playersCell: " (Grid w h playersCell)

genPlayerCell : (Int, String, List Int, (Int, Int)) -> Cell
genPlayerCell (id, name, cs, (x, y)) =
    let (x', y') = (toFloat x, toFloat y)
    in case cs of
           r::g::b::_ -> Cell (PlayerCell (Player id name (Color.rgb r g b) [(x', y')])) x' y'
           _          -> Cell EmptyCell x' y'
