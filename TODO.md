## Features
- Boost
- How did a player die? WallCollision | TrailCollision | PlayerCollision
- More intelligent starting positions
- Player can quit the game
- PlayerNick feels unwieldly for a protocol, use an ID instead or in addition.

## Implementation todo
- Server grid starts from top-left (0,0) .. client grid is bottom-left (0,0)
- Have a triangle in ui for player to show his/her orientation.
- Add tcp socket in server.
- Ability to change playernick at any point of time.
- receiving bad data from server regularly :  { slice = <function:slice>, size = 33, type = "" }