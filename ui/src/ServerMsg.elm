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

wsserver = "ws://localhost:8331"
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
                        ("width" := Decode.float) ("height" := Decode.float) ("players" := decodePlayers colors)
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

decodePlayer : Color -> Decoder GM.PlayerCell
decodePlayer color =
    (Decode.object4
         (\ id name coord o -> GM.PlayerCell (GP.Player id name color o True []) coord)
         ("id"          := Decode.int)
         ("name"        := Decode.string)
         ("coordinate"  := decodePosition)
         ("orientation" := decodeOrientation))

decodePlayers : List Color -> Decoder (List GM.PlayerCell)
decodePlayers colors =
    decodeList (List.map decodePlayer colors)

decodeList : List (Decoder a) -> Decoder (List a)
decodeList decoders =
    Decode.customDecoder
        (Decode.list Decode.value)
        (\jsonList ->
             List.foldr (Result.map2 (::)) (Ok [])
                 (List.map2 (\decoder json -> Decode.decodeValue decoder json)
                      decoders jsonList))

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

                PlayerReady ->
                    Just [ ("type", Encode.string "Ready") ]
                PlayerName p ->
                    Just [ ("name", Encode.string p)
                         , ("type", Encode.string "Name") ]
                PlayerQuit ->
                    Just [ ("type", Encode.string "Quit") ]
                MoveLeft ->
                    Just [ ("type", Encode.string "Left") ]
                MoveRight ->
                    Just [ ("type", Encode.string "Right") ]

                _ -> Nothing
    in case msgObject of
           Just obj -> obj |> Encode.object |> Encode.encode 0
           Nothing -> ""

encodePlayer : GM.PlayerCell -> Encode.Value
encodePlayer pc =
    let p = pc.player
    in [ ("id", Encode.int p.id)
       , ("name", Encode.string p.name)
       , ("coordinate", encodePosition pc.pos)
       , ("orientation", Encode.string (toString p.orientation))
       ]
        |> Encode.object


encodePosition : GP.Position -> Encode.Value
encodePosition (x, y) =
    [ ("x", Encode.float x)
    , ("y", Encode.float y)
    ]
    |> Encode.object

changeColorOfPlayerCell : GM.PlayerCell -> Color -> GM.PlayerCell
changeColorOfPlayerCell cell c =
    let p = cell.player
        p' = { p | color = c }
    in { cell | player = p' }
