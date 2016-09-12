module Grid.Player exposing (..)

import Color exposing (Color)

type alias PlayerId = Int
type alias PlayerName = String
type alias Position = (Float, Float)
type alias Trail = List Position
type Orientation = North | East | South | West

type alias Player =
    { id : PlayerId
    , name : PlayerName
    , color : Color
    , orientation : Orientation
    , alive : Bool
    , trail : Trail
    }
