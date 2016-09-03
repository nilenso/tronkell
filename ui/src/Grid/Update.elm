module Grid.Update exposing (update)

import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Grid.Player exposing (..)

update : Msg -> Grid -> (Grid, Cmd Msg)
update msg grid =
    case msg of
        PlayerMoved pid pos orientation -> ( { grid | playerCells = addPos pid pos orientation grid.playerCells }, Cmd.none)
        PlayerDied _ -> (grid, Cmd.none) -- todo
        _ -> (grid, Cmd.none)

addPos : PlayerId -> Position -> Orientation -> List Cell -> List Cell
addPos pid (posx, posy) orien playerCells =
    List.map
        (\pc ->
             case pc.ctype of
                 PlayerCell p -> if p.id == pid
                                 then let p' = { p | trail = List.append [(pc.x, pc.y)] p.trail, orientation = orien }
                                      in { pc | ctype = PlayerCell p', x = posx, y = posy }
                                 else pc
                 _ -> pc)
            playerCells
