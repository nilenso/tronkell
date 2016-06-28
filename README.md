## Rules
- The Game runs on a square grid
- The Players (2 or more) can move along horizontal and vertical lines of the grid
- As the players move along the lines they leave behind a Wall
- Players die if they collide with:
     - Boundaries of the grid
	 - Their own walls
     - Other players's walls
	 - Other players (Both die)
- Player movement:
     - Does not stop (always goes straight)
     - Is at constant speed
	 - Can only turn left or right
- Boost:
     - Can be activated or deactivated by player
	 - Increases speed by a constant factor (boost)
     - Is limited by a maximum "boost fuel"
	 - Fuel decays at a constant rate (when used)
	 - Fuel refills at a constant rate (when not used)
	 - decay rate > refill rate
	 - Deactivated when fuel is exhausted
- Winning a round:
     - Last player standing
	 - Variants
         - Player wall disappears on death
		 - Time spent on the board is score
		 - Points if player dies on your wall
		 - Wall decays at a constant rate
		 - Boost is only available when near a player wall
		 - Pickups -- positive or negative
		 - ...
- User inputs
    - Turn left or right
	- Activate or deactivate boost
	- Self-destruct

## Implementation
- Client/server
- Client
    - Join server with a nick
	    - Can't join if nick is taken
	    - Can't join if game is in progress
	- Send ready
	- Game starts when all players are ready
	- Get starting state from server (players - id, position, orientation)
	- Get periodic state updates
    - Send user inputs
	- Game ends
	- Results are received
	- Player is kicked out, need to rejoin
	- (?) Create/join a rooms, etc.
- UI
    - The game is visible on every client
	- Curses/Slang
	- (?) SDL
	- (?) Elm on the browser
- Server
    - Waits for client joins
	- Waits for all clients to send ready
	- Starts game with random starting position and orientiation
	- Sends periodic state updates to clients
	- Receives inputs from clients
	- Computes new state
	- GOTO 10
- Transport
    - TCP/IP
	- (?) Websocket
- Protocol
- Acceptable lag
    - Lag = time between input and change in UI
    - 30 fps = 33.33 ms
    - 60 fps = 16.67 ms
	- Around 40 fps of lag is okay?
- Grid is WxH discrete
- Player position is discrete

### Server Implementation

#### Game Play
- There is a 'tick' event generator inside the server.
- Players generate their own events: 'left', 'right'
- These events are linearized into a stream, and processed by the server
- The server computes the new state of the game for each tick event
- The server sends out the state of the game for every tick


#### Grid Shape
For the following grid, p1 is at (1,1) and p2 is at (2,3)

``` shell
            N
          W   E
            S

|   Y |   |    |    |   |
|-----+---+----+----+---|
|   3 |   |    | p2 |   |
|-----+---+----+----+---|
|   2 |   |    |    |   |
|-----+---+----+----+---|
|   1 |   | p1 |    |   |
|-----+---+----+----+---|
|   0 |   |    |    |   |
|-----+---+----+----+---|
| X-> | 0 |  1 |  2 | 3 |
```
