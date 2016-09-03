module Grid.Player exposing (..)

import Color exposing (Color)

type alias PlayerId = Int
type alias PlayerName = String
type alias Position = (Float, Float)
type alias Trail = List Position

type alias Player =
    { id : PlayerId
    , name : PlayerName
    , color : Color
    , trail : Trail
    }
