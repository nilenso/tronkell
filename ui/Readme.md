# Tronkell ui in elm

## Interaction with server:
1. UI would be listening on websockets from server.
2. Starts by registering with name.
3. Sends "ready" when ready to join game. Otherwise "quit" for quitting.
4. Receives game start message from server with details about initial game.
   * Initial game : Game { width, height, players }
   * players : { name, position, orientation }
5. Sends events to server in json.
6. Receives delta-update events from server.
7. Delta-updates means only diffs and not complete game state.


## Implementation details:
1. Based on canvas.
2. Grid of cells.
3. Cell can be empty, player, player's tail.
4. Handles keyboard events of Left key, Right key.
5. Always keeps current state of game.
6. Updates state with delta update events from server.

## Todo for tomorrow:
1. Start moving players left and right with buttons.
