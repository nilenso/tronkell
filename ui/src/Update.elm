module Update exposing (update)

import Message exposing (..)
import Model exposing (Model)
import ServerMsg exposing (sendServerMsg)

import Grid.Model as GM
import Grid.Player as GP
import Grid.Update as GU
import Grid.Message as GMsg

import Random
import Random.String as RString
import Random.Char as RChar
import String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PlayerName name  -> ( { model | nick = Just name }, Cmd.none )
        RegisterNick     -> ( model, registerNickCmd model.nick )
        PlayerReady      -> ( model, sendServerMsg PlayerReady )
        PlayerQuit       -> ( model, sendServerMsg PlayerQuit )
        -- todo : send only when game is playing.
        MoveLeft         -> ( model, sendServerMsg MoveLeft )
        MoveRight        -> ( model, sendServerMsg MoveRight )

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

registerNickCmd : Maybe GP.PlayerName -> Cmd Msg
registerNickCmd nick =
    case nick of
        Nothing -> Cmd.none
        Just p -> if String.length p > 0
                  then sendServerMsg (PlayerName p)
                  else Cmd.none
