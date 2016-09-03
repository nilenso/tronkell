module Grid.Message exposing (..)

import Grid.Player exposing (..)

type Msg = PlayerMoved PlayerId Position
         | PlayerDied PlayerId Position
         | NoOp
