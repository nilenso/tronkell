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
             -- Grid View
             [[ Maybe.withDefault (div [] [text "Game coming"]) (Maybe.map (App.map GridMsg << GV.view) model.grid)
              , button [onClick GeneratePlayers] [text "Generate game"]
              ]

             -- Left buttons of all players -- will go away
             , model.grid
             |> Maybe.map (List.map (\p -> button [onClick (GridMsg (leftMoveMsg p))] [text "<- "])
                               << .playerCells)
             |> Maybe.withDefault []

             -- Right buttons of all players -- will go away
             , model.grid
             |> Maybe.map (List.map (\p -> button [onClick (GridMsg (rightMoveMsg p))] [text  " ->"])
                               << .playerCells)
             |> Maybe.withDefault []

             -- Main Player buttons
             , [ div []
                     [ input [ onInput PlayerName
                             , placeholder "Take a nick"]
                           []
                     , button [onClick PlayerReady] [text "Ready"]
                     , button [onClick PlayerQuit] [text "Quit"]]]
             ])

playerMoveMsg : GP.Position -> GM.Cell -> GMsg.Msg
playerMoveMsg pos cell =
    case cell.ctype of
        GM.PlayerCell p -> GMsg.PlayerMoved p.id pos p.orientation
        _ -> GMsg.NoOp

leftMoveMsg : GM.Cell -> GMsg.Msg
leftMoveMsg cell = playerMoveMsg (cell.x - 1, cell.y) cell

rightMoveMsg : GM.Cell -> GMsg.Msg
rightMoveMsg cell = playerMoveMsg (cell.x + 1, cell.y) cell
