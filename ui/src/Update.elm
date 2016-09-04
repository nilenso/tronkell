module Update exposing (update)

import Message exposing (..)
import Model exposing (Model)

import Grid.Model as GM
import Grid.Player as GP
import Grid.Update as GU

import Random
import Random.String as RString
import Random.Char as RChar

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GeneratePlayers -> ( model, randomGridCmd )
        RandomPlayers playersData -> ( Model (Just (GM.generateGrid playersData gridWidth gridHeight)) Nothing Nothing, Cmd.none)

        PlayerName name  -> ( { model | nick = Just name }, Cmd.none )
        PlayerReady      -> ( model, readyCmds model.nick )
        PlayerQuit       -> ( model, webSocketSend "quit" )
        MoveLeft         -> ( model, webSocketSend "L" )
        MoveRight        -> ( model, webSocketSend "R" )

        GameReady w h ps -> ( { model | grid = Just (GM.init w h ps) }, Cmd.none)
        GameEnded wId    -> ( { model | winnerId = wId }, Cmd.none )
        ServerMsg msg    -> ( model, Cmd.none )

        GridMsg m        ->
            model.grid
            |> Maybe.map (\gg -> let (g', m') = GU.update m gg
                                 in ( { model | grid = Just g' },
                                      Cmd.map GridMsg m'))
            |> Maybe.withDefault ( model, Cmd.none )

        NoOp -> ( model, Cmd.none )

webSocketSend : String -> Cmd Msg
webSocketSend s = Debug.log s Cmd.none

readyCmds : Maybe GP.PlayerName -> Cmd Msg
readyCmds pn =
    case pn of
        Just p -> Cmd.batch [ webSocketSend p
                            , webSocketSend "ready"]
        Nothing -> Cmd.none

gridWidth = 50
gridHeight = 50

randomGridCmd : Cmd Msg
randomGridCmd =
    Random.generate RandomPlayers
        (Random.list 3 (Random.map4 (,,,)
                            (Random.int 1 10)
                            (RString.string 5 RChar.english)
                            (Random.list 3 (Random.int 0 255))
                            (Random.pair (Random.int 0 (gridWidth - 1)) (Random.int 0 (gridHeight - 1)))))
