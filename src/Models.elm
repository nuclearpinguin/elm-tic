module Models exposing (..)

-- Marking of board
type Mark = Empty | X | O


-- String representation of mark
fromMark : Mark -> String
fromMark m =
    case m of
        Empty -> " "
        X -> "X"
        O -> "O"


-- Changes next player
change : Mark -> Mark
change m =
    case m of
        X -> O
        O -> X
        Empty -> Empty


-- Board for tic-tac-toe
type alias Board = 
    { a1 : Mark, a2 : Mark, a3 : Mark
    , b1 : Mark, b2 : Mark, b3 : Mark
    , c1 : Mark, c2 : Mark, c3 : Mark
    }

-- Represnts state of game
type GameState = Draw | WinX | WinO | On


-- Model of the game, it includes: 
-- game - a board with markings
-- player - mark of player who made move
-- state - sate of the game
type alias Game = 
    { board  : Board
    , player : Mark
    , state : GameState
    }

-- Converts a board to list of marks
listBoard : Board -> List (Mark)
listBoard brd = 
    [ brd.a1, brd.a2, brd.a3 
    , brd.b1, brd.b2, brd.b3 
    , brd.c1, brd.c2, brd.c3]
