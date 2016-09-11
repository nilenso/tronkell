## Features
- Boost
- How did a player die? WallCollision | TrailCollision | PlayerCollision
- More intelligent starting positions
- Player can quit the game
- PlayerNick feels unwieldly for a protocol, use an ID instead or in addition.

## Implementation todo
- In UI - need to take Up/Down keys and convert to Left/Right as well.
- Server need to send Id of player to client so that client can know its own current state.
- Have a triangle in ui for player to show his/her orientation.
- Add tcp socket in server.
- Ability to change playernick at any point of time.
- receiving bad data from server regularly :  { slice = <function:slice>, size = 33, type = "" }