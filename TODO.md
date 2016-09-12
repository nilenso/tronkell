## Features
- Boost
- How did a player die? WallCollision | TrailCollision | PlayerCollision
- More intelligent starting positions
- Player can quit the game
- PlayerNick feels unwieldly for a protocol, use an ID instead or in addition.

## Implementation todo
- Flush the client in channel before taking user game inputs (l/r)
- In UI - need to take Up/Down keys and convert to Left/Right as well.
- Ability to change playernick at any point of time.
- receiving bad data from server regularly :  { slice = <function:slice>, size = 33, type = "" }