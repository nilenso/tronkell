module Grid.View exposing (..)

import Html exposing (div, Html)
import Collage as C
import Color exposing (Color)
import Element as E

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

view : Grid -> Html Msg
view grid = List.map renderCell grid
            |> C.collage 300 300
            |> E.toHtml

renderCell : Cell -> C.Form
renderCell cell =
    cellStructure cell
    |> fillCell cell
    |> C.move (toFloat (cell.x * 30), toFloat (cell.y * 30))


cellStructure : Cell -> C.Shape
cellStructure cell =
    case cell.ctype of
        EmptyCell -> C.square 30
        PlayerCell p -> C.oval 30 20
        Trail -> C.oval 20 30

fillCell : Cell -> C.Shape -> C.Form
fillCell cell shape =
    case cell.ctype of
        EmptyCell    -> C.filled (Color.rgb 100 100 100) shape
        PlayerCell _ -> C.filled (Color.rgb 200 100 150) shape
        Trail        -> C.filled (Color.rgb 200 100 100) shape
