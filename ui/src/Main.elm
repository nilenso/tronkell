module Main exposing (main)

import Grid.Model as GM
import Grid.View  as GV
import Color exposing (Color)

import Html.App as App
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Random
import Random.String as RString
import Random.Char as RChar

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
init = (Model (GM.init gridWidth gridHeight) [], Cmd.none)

type alias Model =
    { grid : GM.Grid
    , players : List GM.Cell
    }

type Msg = GeneratePlayers
         | RandomPlayers (List (Int, String, List Int, (Int, Int)))
         | GridMsg GM.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GeneratePlayers -> ( model, randomGridCmd )
        GridMsg _       -> ( model, Cmd.none )
        RandomPlayers playersData ->
            let players = List.map GM.genPlayerCell playersData
            in ( Model (GM.generateGrid playersData model.grid.width model.grid.height) players
               , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
    div []
        [ (App.map GridMsg (GV.view model.grid))
        , button [onClick GeneratePlayers] [text "Generate Players"]
        ]

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
