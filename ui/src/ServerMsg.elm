module ServerMsg exposing (listenServerMsg, sendServerMsg)

import Message exposing (..)
import Grid.Player as GP
import Grid.Model as GM
import Grid.Message as GMsg

import Keyboard exposing (KeyCode)
import WebSocket
import Json.Decode as Decode exposing ((:=), Decoder)
import Json.Encode as Encode
import Color exposing (Color)

wsserver = "ws://echo.websocket.org"
playerColors = List.append [Color.yellow, Color.blue, Color.green] (List.repeat 100 Color.black)

listenServerMsg = WebSocket.listen wsserver (decodeMsg playerColors)

sendServerMsg : Msg -> Cmd Msg
sendServerMsg msg = WebSocket.send wsserver (encodeMsg msg)

decodeMsg : List Color -> String -> Msg
decodeMsg colors json =
    let decodeMsg msgType =
            case msgType of
                "GameReady"  ->
                    Decode.object3 (\w h ps -> GameReady w h (List.map2 changeColorOfPlayerCell ps colors))
                        ("width" := Decode.float) ("height" := Decode.float) ("players" := Decode.list (decodePlayer Color.red))
                "GameEnded"   -> Decode.object1 GameEnded (nullOr ("winner" := Decode.int))
                "PlayerDied"  -> Decode.object1 (\id -> GridMsg (GMsg.PlayerDied id)) ("id" := Decode.int)
                "PlayerMoved" ->
                    Decode.object3 (\id coordinate orientation ->
                                        GridMsg (GMsg.PlayerMoved id coordinate orientation))
                        ("id" := Decode.int) ("coordinate" := decodePosition) ("orientation" := decodeOrientation)
                "ServerMsg"   -> Decode.object1 ServerMsg ("message" := Decode.string)
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

decodePlayer : Color -> Decoder GM.Cell
decodePlayer color =
    (Decode.object4
         (\ id name (x,y) o -> GM.Cell (GM.PlayerCell (GP.Player id name color o True [])) x y)
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

encodeMsg : Msg -> String
encodeMsg msg =
    let msgObject =
            case Debug.log "Message: " msg of
                GridMsg (GMsg.PlayerMoved id (x, y) orien) ->
                    [ ("id", Encode.int id)
                    , ("coordinate", Encode.object [ ("x", Encode.float x)
                                                   , ("y", Encode.float y)])
                    , ("orientation", Encode.string (toString orien))
                    , ("type", Encode.string "PlayerMoved") -- simulates message from server.
                    ]
                  |> Just

                GridMsg (GMsg.PlayerDied id) ->
                    [ ("id", Encode.int id)
                    , ("type", Encode.string "PlayerDied")
                    ]
                  |> Just

                GameReady w h ps ->
                    [ ("width", Encode.float w)
                    , ("height", Encode.float h)
                    , ("players", Encode.list (List.map encodePlayer ps))
                    , ("type", Encode.string "GameReady")
                    ]
                  |> Just

                GameEnded mPid ->
                    [ ("winner", case mPid of
                                     Nothing -> Encode.null
                                     Just pid -> Encode.int pid)
                    , ("type", Encode.string "GameEnded")
                    ]
                  |> Just
                ServerMsg msg ->
                    [ ("message", Encode.string msg)
                    , ("type", Encode.string "ServerMsg")
                    ]
                  |> Just
                _ -> Nothing
    in case msgObject of
           Just obj -> obj |> Encode.object |> Encode.encode 0
           Nothing -> ""

encodePlayer : GM.Cell -> Encode.Value
encodePlayer pc =
    case pc.ctype of
        GM.PlayerCell p ->
            [ ("id", Encode.int p.id)
            , ("name", Encode.string p.name)
            , ("coordinate", encodePosition (pc.x, pc.y))
            , ("orientation", Encode.string (toString p.orientation))
            ]
            |> Encode.object

        _            -> Encode.null

encodePosition : GP.Position -> Encode.Value
encodePosition (x, y) =
    [ ("x", Encode.float x)
    , ("y", Encode.float y)
    ]
    |> Encode.object

changeColorOfPlayerCell : GM.Cell -> Color -> GM.Cell
changeColorOfPlayerCell cell c =
    case cell.ctype of
        GM.PlayerCell p -> { cell | ctype = GM.PlayerCell { p | color = c }}
        _ -> cell
