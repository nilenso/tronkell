module Grid.View exposing (..)

import Grid.Model as GM

import Html exposing (div, Html)
import Collage as C
import Color exposing (Color)
import Element as E


view : GM.Grid -> Html GM.Msg
view grid = List.map renderCell grid
            |> C.collage 300 300
            |> E.toHtml

renderCell : GM.Cell -> C.Form
renderCell cell =
    cellStructure cell
    |> fillCell cell
    |> C.move (toFloat (cell.x * 30), toFloat (cell.y * 30))


cellStructure : GM.Cell -> C.Shape
cellStructure cell =
    case cell.ctype of
        GM.EmptyCell -> C.square 30
        GM.PlayerCell p -> C.oval 30 20
        GM.Trail -> C.oval 20 30

fillCell : GM.Cell -> C.Shape -> C.Form
fillCell cell shape =
    case cell.ctype of
        GM.EmptyCell    -> C.filled (Color.rgb 100 100 100) shape
        GM.PlayerCell _ -> C.filled (Color.rgb 200 100 150) shape
        GM.Trail        -> C.filled (Color.rgb 200 100 100) shape
