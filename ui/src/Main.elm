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
init = (Model (GM.init gridWidth gridHeight), Cmd.none)

type alias Model =
    { grid : GM.Grid
    }

type Msg = GeneratePlayers
         | RandomPlayers (List (Int, String, List Int, (Int, Int)))
         | GridMsg GM.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GeneratePlayers -> ( model, randomGridCmd )
        GridMsg m       -> let (g, _) = GM.update m model.grid
                           in ({ model | grid = g }, Cmd.none)
        RandomPlayers playersData ->
            ( Model (GM.generateGrid playersData model.grid.width model.grid.height)
            , Cmd.none)

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
        GM.PlayerCell p -> GM.MovePlayer p.id pos
        _ -> GM.NoOp

leftMoveMsg : GM.Cell -> GM.Msg
leftMoveMsg cell = playerMoveMsg (cell.x - 1, cell.y) cell

rightMoveMsg : GM.Cell -> GM.Msg
rightMoveMsg cell = playerMoveMsg (cell.x + 1, cell.y) cell
