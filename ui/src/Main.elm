module Main exposing (main)

import Model exposing (Model)
import Message exposing (..)
import Update exposing (update)
import View exposing (view)
import Subscription exposing (subscriptions)

import Html.App as App

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : (Model, Cmd Msg)
init = (Model Nothing Nothing Nothing, Cmd.none)
