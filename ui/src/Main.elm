module Main exposing (main)

import Model exposing (Model)
import Message exposing (..)
import Update exposing (update)
import View exposing (view)

import Html.App as App

import Keyboard exposing (KeyCode)
import WebSocket
import Json.Decode as Decode exposing ((:=), Decoder)

wsserver = "ws://echo.websocket.org"

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
subscriptions model =
    Sub.batch [ Keyboard.ups keyToMsg
              , WebSocket.listen wsserver decodeMsg
              ]

keyToMsg : KeyCode -> Msg
keyToMsg keycode =
    case keycode of
        37 -> MoveLeft
        39 -> MoveRight
        _  -> NoOp

decodeMsg : String -> Msg
decodeMsg json =
    let decodeMsg msgType =
            case msgType of
                "GameReady" ->
                    Decode.object2 (\w h -> GameReady w h []) ("width" := Decode.float) ("height" := Decode.float)
                "GameEnded" -> Decode.object1 GameEnded (nullOr ("winner" := Decode.int))
                _           -> Decode.succeed NoOp
        decoder = (Decode.andThen ("type" := Decode.string ) decodeMsg)
        res     = Decode.decodeString decoder json
    in case res of
           Ok m -> m
           Err _ -> NoOp

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    Decode.oneOf [ Decode.null Nothing
                 , Decode.map Just decoder]
