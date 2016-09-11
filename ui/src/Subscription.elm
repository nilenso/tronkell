module Subscription exposing (subscriptions)

import Model exposing (..)
import Message exposing (..)

import Keyboard exposing (KeyCode)
import ServerMsg exposing (listenServerMsg)
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.ups keyToMsg
              , listenServerMsg
              ]

keyToMsg : KeyCode -> Msg
keyToMsg keycode =
    case keycode of
        37 -> MoveLeft
        38 -> MoveUp
        39 -> MoveRight
        40 -> MoveDown
        _  -> NoOp
