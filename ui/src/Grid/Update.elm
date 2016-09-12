module Grid.Update exposing (update)

import Grid.Model exposing (..)
import Grid.Message exposing (..)
import Grid.Player exposing (..)

update : Msg -> Grid -> (Grid, Cmd Msg)
update msg grid =
    case msg of
        PlayerMoved pid pos orientation ->
            ( { grid | playerCells = addPos pid pos orientation grid.playerCells }, Cmd.none)
        PlayerDied pid ->
            ( { grid | playerCells = playerDied pid grid.playerCells }, Cmd.none)
        NoOp -> (grid, Cmd.none)

addPos : PlayerId -> Position -> Orientation -> List PlayerCell -> List PlayerCell
addPos pid pos' orien playerCells =
    updatePlayer
        pid
        (\pc -> let p = pc.player
                    p' = { p | trail = List.append [pc.pos] p.trail, orientation = orien }
                in { pc | player = p', pos = pos' })
        playerCells

playerDied : PlayerId -> List PlayerCell -> List PlayerCell
playerDied pid playerCells =
    updatePlayer pid
        (\pc -> let p = pc.player
                    p' = { p | alive = False }
                in { pc | player = p' })
        playerCells

updatePlayer : PlayerId -> (PlayerCell -> PlayerCell) -> List PlayerCell -> List PlayerCell
updatePlayer pid f playerCells =
    List.map
        (\pc -> if pc.player.id == pid
                then f pc
                else pc)
        playerCells
