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
                   , List.concatMap playerToTrailCell grid.playerCells
                   , List.map (\p -> Cell (PlayerCellType p.player) p.pos) grid.playerCells
                   ]
