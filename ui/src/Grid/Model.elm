module Grid.Model exposing (..)

import Color exposing (Color)
import List.Extra as ListE

import Grid.Player exposing (..)
import Grid.Message exposing (..)

-- Grid is main Model
type alias Grid =
     { width       : Float
     , height      : Float
     , playerCells : List PlayerCell
     }

type alias PlayerCell =
    { player : Player
    , pos    : Position
    }

-- Cells are for View.
type CellType = EmptyCellType | PlayerCellType Player | TrailCellType Color
type alias Cell =
         { ctype : CellType
         , pos   : Position
         }

init : Float -> Float -> List PlayerCell -> Grid
init w h playerCells = Grid w h playerCells

gridToList : Grid -> List Cell
gridToList grid =
    let playerToTrailCell p = List.map (Cell (TrailCellType p.player.color))  p.player.trail
    in List.concat [ List.map (Cell EmptyCellType) (ListE.lift2 (,) [0 .. grid.height - 1]  [0 .. grid.width - 1])
                   , List.map (\p -> Cell (PlayerCellType p.player) p.pos) grid.playerCells
                   , List.concatMap playerToTrailCell grid.playerCells
                   ]

generateGrid : List (Int, String, List Int, (Int, Int)) -> Float -> Float -> Grid
generateGrid playersData w h =
    let playersCell = List.map genPlayerCell playersData
    in Debug.log "playersCell: " (Grid w h playersCell)

genPlayerCell : (Int, String, List Int, (Int, Int)) -> PlayerCell
genPlayerCell (id, name, cs, (x, y)) =
    let pos = (toFloat x, toFloat y)
        (r,g,b) = case cs of
                      r'::g'::b'::_ -> (r, g, b)
                      _             -> (100,100,100)
    in PlayerCell (Player id name (Color.rgb r g b) North True [pos]) pos
