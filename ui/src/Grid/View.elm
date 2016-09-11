module Grid.View exposing (..)

import Grid.Model exposing (..)
import Grid.Message exposing (..)

import Html exposing (div, text, Html)
import Collage as C
import Color exposing (Color)
import Element as E

cellWidth : Float
cellWidth = 10

cellHeight : Float
cellHeight = 10

type alias Boundary = (Float, Float)

view : Grid -> Html Msg
view grid =
    div []
        [ gridToList grid
           |> List.map (renderCell (grid.width, grid.height))
           |> C.collage (round (grid.width * cellWidth)) (round (grid.height * cellHeight))
           |> E.toHtml

        -- debugging..
        , div []
              ( List.map playerInfoView grid.playerCells )
        ]

renderCell : Boundary -> Cell -> C.Form
renderCell b cell =
    let (cx, cy) = bringToCorner b cell.pos
    in cellStructure cell
        |> fillCell cell
        |> C.move (cx * cellWidth, cy * cellHeight)

playerInfoView : PlayerCell -> Html Msg
playerInfoView cell =
    div []
        [ text (toString cell.player) ]

bringToCorner : Boundary -> (Float, Float) -> (Float, Float)
bringToCorner (w, h) (x, y) = (x - w / 2 + 0.5, (h - y) - h / 2 + 0.5)

cellStructure : Cell -> C.Shape
cellStructure cell =
    case cell.ctype of
        EmptyCellType    -> C.square (cellWidth - 1)
        PlayerCellType p -> C.oval (cellWidth - 1) (cellHeight - 1)
        TrailCellType _  -> C.oval (cellHeight - 1 ) (cellWidth - 1)

fillCell : Cell -> C.Shape -> C.Form
fillCell cell shape =
    C.filled (colorOfCell cell) shape

colorOfCell : Cell -> Color
colorOfCell cell =
    case cell.ctype of
        EmptyCellType    -> Color.grey
        PlayerCellType p -> if p.alive
                            then p.color
                            else Color.red
        TrailCellType c -> c
