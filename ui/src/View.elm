module View exposing (view)

import Message exposing (..)
import Model exposing (Model)

import Grid.Model as GM
import Grid.Player as GP
import Grid.Message as GMsg
import Grid.View as GV

import Html.App as App
import Html exposing (Html, div, button, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)
import Color exposing (Color)

view : Model -> Html Msg
view model =
    div []
        (List.concat
             [ -- Grid View
               [ Maybe.withDefault (div [] [text "Game coming"]) (Maybe.map (App.map GridMsg << GV.view) model.grid)]

             -- Main Player buttons
             , [ div []
                     [ input [ onInput PlayerName
                             , placeholder "Take a nick"]
                           []
                     , button [onClick PlayerReady] [text "Ready"]
                     , button [onClick PlayerQuit] [text "Quit"]]]

             -- Simulation code.
             , [ div []
                     [ button [onClick GeneratePlayers] [text "Start Random Game"]
                     , button [] [text "Stop Game"]]]

             , model.grid
             |> Maybe.map (List.map (\p -> div []
                                               [ text (getName p)
                                               , button [ onClick (leftMoveMsg p)] [text "<- "]
                                               , button [ onClick (rightMoveMsg p)] [text  " ->"]
                                               , button [ onClick (killPlayerMsg p)] [text "kill"]
                                               ])
                               << .playerCells)
             |> Maybe.withDefault []
             ])

leftMoveMsg : GM.Cell -> Msg
leftMoveMsg cell =
    case cell.ctype of
        GM.PlayerCell p -> MovePlayer p.id (cell.x - 1, cell.y) p.orientation
        _ -> NoOp

rightMoveMsg : GM.Cell -> Msg
rightMoveMsg cell =
    case cell.ctype of
        GM.PlayerCell p -> MovePlayer p.id (cell.x + 1, cell.y) p.orientation
        _ -> NoOp

killPlayerMsg : GM.Cell -> Msg
killPlayerMsg cell =
    case cell.ctype of
        GM.PlayerCell p -> KillPlayer p.id
        _ -> NoOp

getName : GM.Cell -> String
getName cell =
    case cell.ctype of
        GM.PlayerCell p -> p.name
        _ -> "<bad>"
