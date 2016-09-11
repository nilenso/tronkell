module Tronkell.Types where

import System.Random

type Coordinate = (Int, Int)

data Orientation = North | East | South | West
                   deriving (Eq, Enum, Bounded, Show, Ord)

instance Random Orientation where
  random gen =
    randomR (North, West) gen
  randomR (a, b) gen =
    let (n, gen') = next gen
        aInt = fromEnum a
        bInt = (fromEnum b)
        diffAB = bInt - aInt + 1
        n' = (n `rem` diffAB) + aInt
        orien = case n' of
                  0 -> North
                  1 -> East
                  2 -> South
                  3 -> West
                  _ -> North
    in (orien, gen')
