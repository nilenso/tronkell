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
        MoveLeft         -> ( model, getMoveCmd model MoveLeft )
        MoveRight        -> ( model, getMoveCmd model MoveRight )
        MoveUp           -> ( model, getMoveCmd model MoveUp )
        MoveDown         -> ( model, getMoveCmd model MoveDown )

        GameReady w h ps -> ( { model | grid = Just (GM.init w h ps) }, Cmd.none)
        GameEnded wId    -> ( { model | winnerId = wId }, Cmd.none )
        PlayerRegisterId pid -> ( { model | myId = Just pid }, Cmd.none )
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

getMoveCmd : Model -> Msg -> Cmd Msg
getMoveCmd model msg =
    let getMoveCmd' orien msg =
          let nextMove = case (orien, msg) of
                (GP.North, MoveLeft)  -> Just MoveLeft
                (GP.North, MoveRight) -> Just MoveRight
                (GP.North, _)         -> Nothing

                (GP.South, MoveLeft)  -> Just MoveRight
                (GP.South, MoveRight) -> Just MoveLeft
                (GP.South, _)         -> Nothing

                (GP.East, MoveUp)     -> Just MoveLeft
                (GP.East, MoveDown)   -> Just MoveRight
                (GP.East, _)          -> Nothing

                (GP.West, MoveUp)     -> Just MoveRight
                (GP.West, MoveDown)   -> Just MoveLeft
                (GP.West, _)          -> Nothing
              log_ = Debug.log "orien, msg, nextMove : " (orien, msg, nextMove)
          in case nextMove of
                 Nothing -> Cmd.none
                 Just move -> sendServerMsg move
    in case (model.myId, model.grid) of
           (Just pid, Just grid) ->
               let mes = List.filter (\pc -> pc.player.id == pid) grid.playerCells
               in case mes of
                      me::[] -> getMoveCmd' me.player.orientation msg
                      _      -> Cmd.none -- some serious error msg.

           _ -> Cmd.none
