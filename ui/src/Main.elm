module Main exposing (main)

import Grid.Model as M
import Grid.View  as G

main = G.view [ M.Cell M.EmptyCell 0 0
              , M.Cell M.EmptyCell 0 1
              , M.Cell (M.PlayerCell (M.Player 1 "ashish")) 1 0
              , M.Cell M.Trail 1 1
              ]
