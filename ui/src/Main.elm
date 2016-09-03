module Main exposing (main)

import Model exposing (Model)
import Message exposing (..)
import Update exposing (update)
import View exposing (view)

import Html.App as App
import Keyboard exposing (KeyCode)

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init = (Model Nothing Nothing Nothing, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.ups keyToMsg

keyToMsg : KeyCode -> Msg
keyToMsg keycode =
    case keycode of
        37 -> MoveLeft
        39 -> MoveRight
        _  -> NoOp
