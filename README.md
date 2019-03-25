# Battleships
Creating single player functionality of battleships using functional programming
The game battleship is a famous 2 player board game in which the opponents try to guess the location of other various ships. 
Each player hides his ships on a (usually 10*10 square) grid containing horizontal or vertical space coordinates. Players take turns calling out row and columns coordinates on the other player's grid in an attempt to identify a square that contains a ship.
A ship is sunk when all the squares occupied by it are hit. The goal of the game is to sink all the opponents' ships. The player who does that first wins.

RULES OF THE GAME:

1. Each player has 5 ships initially of lengths 2,3,3,4,5 respectively.
2. Players A and B take turns calling out the opponent's squares. After A calls out a square on B's grid, B is supposed to declare if its a hit or a miss. 
3. If a block containing a part of one of B's ships is hit, then A gets another turn.
4. If all blocks containing one of B's ships is hit, then B declares the size of the ship which has sunk.
5. The game ends when all of a player's ships have been sunk. The other player is the winner.

IDEA OF SOLUTION:
A primitive algorithm for a computer player would be randomly guessing the squares for the opponents ship throughout the game. As obvious, this is a very inefficient solution to the problem, and takes a lot of time to finally be able to figure out the positions of the opponent’s ships.

The primitive algorithm is then improved by taking into account the fact that the squares occupied by any of the ships would be adjacent to each other. Hence, after the random algorithm gets a hit, the algorithm moves on to explore the rest of the squares adjacent to it. Further optimisations to the algorithm are then performed by taking into consideration the possible positions and the lengths of the ships left on the board. 
