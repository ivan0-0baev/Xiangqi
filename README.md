# Xiangqi
Two Player Xiangqi Game inside the Terminal


Some info on the files: 

Uses Fen Strings to interpret the Board Positions 

InterpretFen.hs translates the fen string to a board and vice versa
depending on the appropriate format.
UpdateBoard.hs changes the fen string given a certain move.
GameBoard.hs contains functions that display the board in the terminal

The listMoves function, using isLegal in MoveLogic.hs, in XiangqiBot.hs gives a list of all 
Legal Moves givena certain board position and depending on whose turn it is.
The legality of moves given by each player is checked and either accepted
or refused.


To do: 

Currently Missing Winning Condition.
Fen string usage can be optimised.
...


Board representation information: 

Red Player is represented by the uppercase letters.
Black Player is represented by the lowercase letters.

Each square on the Board is given by square brackets [], 
with the exception of the castle given in parentheses (). The General
and Advisor pieces are both bounded to move only inside the castle.
Between rows 4 and 5 is the river/border, that gives the division 
between the two player fields, affecting the movement of the 
Elephant and Soldier pieces.
