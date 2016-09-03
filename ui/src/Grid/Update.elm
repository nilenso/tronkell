module Grid.Update exposing (update)

import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Grid.Player exposing (..)

update : Msg -> Grid -> (Grid, Cmd Msg)
update msg grid =
    case msg of
        PlayerMoved pid pos -> ( { grid | playerCells = addPos pid pos grid.playerCells }, Cmd.none)
        PlayerDied _ _ -> (grid, Cmd.none) -- todo
        _ -> (grid, Cmd.none)

addPos : PlayerId -> Position -> List Cell -> List Cell
addPos pid (posx, posy) playerCells =
    List.map
        (\pc ->
             case pc.ctype of
                 PlayerCell p -> if p.id == pid
                                 then let p' = { p | trail = List.append [(posx, posy)] p.trail }
                                      in { pc | ctype = PlayerCell p', x = posx, y = posy }
                                 else pc
                 _ -> pc)
            playerCells
