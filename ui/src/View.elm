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
               [ Maybe.withDefault (div [] [ text "Waiting for your inputs..." ]) (Maybe.map (App.map GridMsg << GV.view) model.grid)]

             -- Main Player buttons
             , [ div []
                     [ text "Register with a nick.."
                     , input [ onInput PlayerName
                             , placeholder "Take a nick"]
                           []
                     ,  button [onClick RegisterNick] [text "Register"]

                     , div []
                         [ text "Send Ready when you are ready to join the game !!!"
                         , button [onClick PlayerReady] [text "Ready"]
                         , button [onClick PlayerQuit] [text "Quit"]
                         ]
                     ]
               ]
             ])
