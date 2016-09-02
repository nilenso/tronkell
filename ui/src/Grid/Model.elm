module Grid.Model exposing (..)

import Color exposing (Color)
import List.Extra as ListE

type alias PlayerId = Int
type alias PlayerName = String
type alias Position = (Float, Float)
type alias Trail = List Position

type alias Player =
    { id : PlayerId
    , name : PlayerName
    , color : Color
    , trail : Trail
    }

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

type Msg = PlayerMoved PlayerId Position
         | PlayerDied PlayerId Position
         | NoOp

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


update : Msg -> Grid -> (Grid, Cmd Msg)
update msg grid =
    case msg of
        PlayerMoved pid pos -> ( { grid | playerCells = addPos pid pos grid.playerCells }, Cmd.none)
        _ -> (grid, Cmd.none)

addPos : PlayerId -> Position -> List Cell -> List Cell
addPos pid (posx, posy) playerCells =
    List.map
        (\pc ->
             case pc.ctype of
                 PlayerCell p -> if p.id == pid
                                 then let p' = { p | trail = List.append [(posx, posy)] p.trail }
                                      in { pc | ctype = PlayerCell p', x = posx, y = posy }
                                 else pc
                 _ -> pc)
            playerCells
