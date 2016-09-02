module Main exposing (main)

import Grid.Model as GM
import Grid.View  as GV
import Color exposing (Color)

import Html.App as App
import Html exposing (Html, div, button, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)
import Random
import Random.String as RString
import Random.Char as RChar
import Message exposing (..)

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

gridWidth = 50
gridHeight = 50

init : (Model, Cmd Msg)
init = (Model (GM.init gridWidth gridHeight []) Nothing Nothing, Cmd.none)

type alias Model =
    { grid : GM.Grid
    , nick : Maybe GM.PlayerName
    , winnerId : Maybe GM.PlayerId
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GeneratePlayers -> ( model, randomGridCmd )
        RandomPlayers playersData ->
            ( Model (GM.generateGrid playersData model.grid.width model.grid.height) Nothing Nothing
            , Cmd.none)

        PlayerName name  -> ( { model | nick = Just name }, Cmd.none )
        PlayerReady      -> ( model, readyCmds model.nick )
        PlayerQuit       -> ( model, webSocketSend "quit" )
        MoveLeft         -> ( model, webSocketSend "L" )
        MoveRight        -> ( model, webSocketSend "R" )

        GameReady w h ps -> ( { model | grid = (GM.init w h ps) }, Cmd.none) -- set players
        GameEnded wId    -> ( { model | winnerId = wId }, Cmd.none )
        ServerMsg msg    -> ( model, Cmd.none )
        GridMsg m        -> let (g, _) = GM.update m model.grid
                            in ({ model | grid = g }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
    div []
        (List.concat [ [ (App.map GridMsg (GV.view model.grid))
                      , button [onClick GeneratePlayers] [text "Generate Players"]
                      ]
                     , List.map (\p -> button [onClick (GridMsg (leftMoveMsg p))] [text "<- "]) model.grid.playerCells
                     , List.map (\p -> button [onClick (GridMsg (rightMoveMsg p))] [text  " ->"]) model.grid.playerCells
                     , [ div []
                             [ input [ onInput PlayerName
                                     , placeholder "Take a nick"]
                                     []
                             , button [onClick PlayerReady] [text "Ready"]
                             , button [onClick PlayerQuit] [text "Quit"]]]
                     ])

randomGridCmd : Cmd Msg
randomGridCmd =
    Random.generate
        RandomPlayers
        (Random.list 3
             (Random.map4
                  (,,,)
                  (Random.int 1 10)
                  (RString.string 5 RChar.english)
                  (Random.list 3 (Random.int 0 255))
                  (Random.pair (Random.int 0 (gridWidth - 1)) (Random.int 0 (gridHeight - 1)))))

cellsToPlayers : List GM.Cell -> List GM.Player
cellsToPlayers cells =
    List.concatMap (\c -> case c.ctype of
                              GM.PlayerCell p -> [p]
                              _ -> [])
        cells

playerMoveMsg : GM.Position -> GM.Cell -> GM.Msg
playerMoveMsg pos cell =
    case cell.ctype of
        GM.PlayerCell p -> GM.PlayerMoved p.id pos
        _ -> GM.NoOp

leftMoveMsg : GM.Cell -> GM.Msg
leftMoveMsg cell = playerMoveMsg (cell.x - 1, cell.y) cell

rightMoveMsg : GM.Cell -> GM.Msg
rightMoveMsg cell = playerMoveMsg (cell.x + 1, cell.y) cell

webSocketSend : String -> Cmd Msg
webSocketSend _ = Cmd.none

readyCmds : Maybe GM.PlayerName -> Cmd Msg
readyCmds pn =
    case pn of
        Just p -> Cmd.batch [ webSocketSend p
                            , webSocketSend "ready"]
        Nothing -> Cmd.none
