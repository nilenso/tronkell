module Subscription exposing (subscriptions)

import Model exposing (..)
import Message exposing (..)

import Grid.Player as GP
import Grid.Model as GM
import Grid.Message as GMsg

import Keyboard exposing (KeyCode)
import WebSocket
import Json.Decode as Decode exposing ((:=), Decoder)
import Color exposing (Color)

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
                "GameReady"  ->
                    Decode.object3 GameReady
                        ("width" := Decode.float) ("height" := Decode.float) ("players" := Decode.list decodePlayer)
                "GameEnded"   -> Decode.object1 GameEnded (nullOr ("winner" := Decode.int))
                "PlayerDied"  -> Decode.object1 (\id -> GridMsg (GMsg.PlayerDied id)) ("id" := Decode.int)
                "PlayerMoved" ->
                    Decode.object3 (\id coordinate orientation ->
                                        GridMsg (GMsg.PlayerMoved id coordinate orientation))
                        ("id" := Decode.int) ("coordinate" := decodePosition) ("orientation" := decodeOrientation)
                _             -> Decode.succeed NoOp
        decoder = Decode.andThen ("type" := Decode.string ) decodeMsg
        res     = Decode.decodeString decoder json
    in case res of
           Ok m -> m
           Err _ -> NoOp

nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    Decode.oneOf [ Decode.null Nothing
                 , Decode.map Just decoder]

decodePlayer : Decoder GM.Cell
decodePlayer =
    (Decode.object4
         (\ id name (x,y) o -> GM.Cell (GM.PlayerCell (GP.Player id name (Color.rgb 100 100 100) o [])) x y)
         ("id"          := Decode.int)
         ("name"        := Decode.string)
         ("coordinate"  := decodePosition)
         ("orientation" := decodeOrientation))

decodePosition : Decoder GP.Position
decodePosition = Decode.object2 (\ x y -> (x,y)) ("x" := Decode.float) ("y" := Decode.float)

decodeOrientation : Decoder GP.Orientation
decodeOrientation =
    Decode.andThen
        Decode.string
        (\ s -> Decode.succeed (case s of
                                    "North" -> GP.North
                                    "East"  -> GP.East
                                    "South" -> GP.South
                                    "West"  -> GP.West
                                    _       -> GP.North))


wsserver = "ws://echo.websocket.org"
