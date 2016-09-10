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
        -- GeneratePlayers -> ( model, randomGridCmd )
        -- RandomPlayers playersData -> let grid = (GM.generateGrid playersData gridWidth gridHeight)
        --                              in ( model, sendServerMsg (GameReady gridWidth gridHeight grid.playerCells))
        -- MovePlayer pid pos orien -> ( model, sendServerMsg (GridMsg (GMsg.PlayerMoved pid pos orien)))
        -- KillPlayer pid           -> ( model, sendServerMsg (GridMsg (GMsg.PlayerDied pid)))

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

gridWidth = 50
gridHeight = 50

-- randomGridCmd : Cmd Msg
-- randomGridCmd =
--     Random.generate RandomPlayers
--         (Random.list 3 (Random.map4 (,,,)
--                             (Random.int 1 10)
--                             (RString.string 5 RChar.english)
--                             (Random.list 3 (Random.int 0 255))
--                             (Random.pair (Random.int 0 (gridWidth - 1)) (Random.int 0 (gridHeight - 1)))))

registerNickCmd : Maybe GP.PlayerName -> Cmd Msg
registerNickCmd nick =
    case nick of
        Nothing -> Cmd.none
        Just p -> if String.length p > 0
                  then sendServerMsg (PlayerName p)
                  else Cmd.none
