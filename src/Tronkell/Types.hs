module Tronkell.Types where

type Coordinate = (Int, Int)

data Orientation = North | East | South | West
                   deriving (Eq, Enum, Bounded, Show)
