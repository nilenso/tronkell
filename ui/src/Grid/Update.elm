module Grid.Update exposing (update)

import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Grid.Player exposing (..)

update : Msg -> Grid -> (Grid, Cmd Msg)
update msg grid =
    case msg of
        PlayerMoved pid pos orientation -> ( { grid | playerCells = addPos pid pos orientation grid.playerCells }, Cmd.none)
        PlayerDied pid -> ({ grid | playerCells = playerDied pid grid.playerCells }, Cmd.none)
        _ -> (grid, Cmd.none)

addPos : PlayerId -> Position -> Orientation -> List Cell -> List Cell
addPos pid (posx, posy) orien playerCells =
    updatePlayer
        pid
        (\pc ->
             case pc.ctype of
                 PlayerCell p -> let p' = { p | trail = List.append [(pc.x, pc.y)] p.trail, orientation = orien }
                                 in { pc | ctype = PlayerCell p', x = posx, y = posy }
                 _ -> pc)
            playerCells

playerDied : PlayerId -> List Cell -> List Cell
playerDied pid playerCells =
    updatePlayer
        pid
        (\pc -> case pc.ctype of
                    PlayerCell p -> { pc | ctype = PlayerCell { p | alive = False }}
                    _            -> pc)
             playerCells

updatePlayer : PlayerId -> (Cell -> Cell) -> List Cell -> List Cell
updatePlayer pid f playerCells =
    List.map
        (\pc -> case pc.ctype of
                    PlayerCell p -> if p.id == pid
                                    then f pc
                                    else pc
                    _            -> pc)
        playerCells
