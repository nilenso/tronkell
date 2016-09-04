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
             |> Maybe.map (List.map (\p -> button [onClick (leftMoveMsg p)] [text "<- "])
                               << .playerCells)
             |> Maybe.withDefault []

             -- Right buttons of all players -- will go away
             , model.grid
             |> Maybe.map (List.map (\p -> button [onClick (rightMoveMsg p)] [text  " ->"])
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
