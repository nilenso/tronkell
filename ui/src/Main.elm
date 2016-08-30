module Main exposing (main)

import Grid.View as G

main = G.view [ G.Cell G.EmptyCell 0 0
              , G.Cell G.EmptyCell 0 1
              , G.Cell (G.PlayerCell (G.Player 1 "ashish")) 1 0
              , G.Cell G.Trail 1 1
              ]
