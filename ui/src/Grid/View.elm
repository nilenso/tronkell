module Grid.View exposing (..)

import Grid.Model as GM

import Html exposing (div, Html)
import Collage as C
import Color exposing (Color)
import Element as E

cellWidth : Float
cellWidth = 10

cellHeight : Float
cellHeight = 10

cellPadding : Float
cellPadding = 0

type alias Boundary = (Float, Float)

view : GM.Grid -> Html GM.Msg
view grid =
    GM.gridToList grid
    |> List.map (renderCell (grid.width, grid.height))
    |> C.collage (round (grid.width * cellWidth)) (round (grid.height * cellHeight))
    |> E.toHtml

renderCell : Boundary -> GM.Cell -> C.Form
renderCell b cell =
    let (cx, cy) = bringToCorner b (cell.x, cell.y)
    in cellStructure cell
        |> fillCell cell
        |> C.move (cx * cellWidth, cy * cellHeight)

bringToCorner : Boundary -> (Float, Float) -> (Float, Float)
bringToCorner (w, h) (x, y) = (x - w / 2 + 0.5, y - h / 2 + 0.5)

cellStructure : GM.Cell -> C.Shape
cellStructure cell =
    case cell.ctype of
        GM.EmptyCell -> C.square (cellWidth - 1)
        GM.PlayerCell p -> C.oval (cellWidth - 1) (cellHeight - 1)
        GM.Trail -> C.oval (cellHeight - 1 ) (cellWidth - 1)

fillCell : GM.Cell -> C.Shape -> C.Form
fillCell cell shape =
    C.filled (colorOfCell cell) shape

colorOfCell : GM.Cell -> Color
colorOfCell cell =
    case cell.ctype of
        GM.EmptyCell    -> Color.rgb 100 100 100
        GM.PlayerCell p -> p.color
        GM.Trail        -> Color.rgb 200 100 100
