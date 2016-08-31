module Main exposing (main)

import Grid.Model as M
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

init : (Model, Cmd Msg)
init = (Model [] [], Cmd.none)

type alias Model =
    { grid : M.Grid
    , players : List M.Cell
    }

type Msg = GeneratePlayers
         | RandomPlayers (List (Int, String, List Int, (Int, Int)))
         | GridMsg M.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GeneratePlayers -> ( model, randomGridCmd )
        GridMsg _       -> ( model, Cmd.none )
        RandomPlayers playersData -> ( let players = List.map ( M.genPlayerCell) playersData
                                       in Model (M.generateGrid playersData) players, Cmd.none)

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
                  (Random.pair (Random.int 0 4) (Random.int 0 4))))
