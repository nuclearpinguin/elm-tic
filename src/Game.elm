module Game exposing (..)
import List exposing (foldr)

-- Marking of board
type Mark = Empty | X | O


type alias Board = 
    { a1 : Mark, a2 : Mark, a3 : Mark
    , b1 : Mark, b2 : Mark, b3 : Mark
    , c1 : Mark, c2 : Mark, c3 : Mark
    }

type GameState = Draw | WinX | WinO | On | NewGame


type GameType = PlayerPlayer | PlayerComputer | Nil


type PlayerAction = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3 | None


type alias Game = 
    { board  : Board
    , player : Mark
    , state : GameState
    , gameType : GameType
    }


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


-- Converts a board to list of marks
listBoard : Board -> List (Mark)
listBoard brd = 
    [ brd.a1, brd.a2, brd.a3 
    , brd.b1, brd.b2, brd.b3 
    , brd.c1, brd.c2, brd.c3]


emptyBoard : Board
emptyBoard = 
    { a1 = Empty, a2 = Empty, a3 = Empty
    , b1 = Empty, b2 = Empty, b3 = Empty
    , c1 = Empty, c2 = Empty, c3 = Empty
    }


init : Game
init = 
    { board = emptyBoard
    , player = O
    , state = NewGame
    , gameType = Nil
    }

-- UPDATE

-- Check if any three same marks in a row
checkGame : Board -> Mark -> GameState
checkGame brd mrk = 
    let 
        -- Check if someone has won
        check = \ms-> foldr (\x y -> y && (x == mrk))  True ms
        r1 = check [brd.a1, brd.a2, brd.a3]
        r2 = check [brd.b1, brd.b2, brd.b3]
        r3 = check [brd.c1, brd.c2, brd.c3]
        c1 = check [brd.a1, brd.b1, brd.c1]
        c2 = check [brd.a2, brd.b2, brd.c2]
        c3 = check [brd.a3, brd.b3, brd.c3]
        d1 = check [brd.a1, brd.b2, brd.c3]
        d2 = check [brd.a3, brd.b2, brd.c1]
        win =  r1 || r2 || r3 || c1 || c2 || c3 || d1 || d2

        -- Check if board is full
        end = foldr (\x y -> y && (x /= Empty)) True (listBoard brd)
    in
     case win of
        True -> case mrk of
            X -> WinX
            O -> WinO
            Empty -> On
        False -> case end of
            True -> Draw
            False -> On


stateToString : GameState -> Mark -> String
stateToString s m =
    case s of
        WinX -> "Player X wins!"
        WinO -> "Player O wins!"
        Draw -> "Draw."
        On -> "Move of player " ++ (fromMark (change m))
        NewGame -> "Select game type"


updateBoard : Board -> Mark -> PlayerAction -> Board
updateBoard b mark act =
    case act of
        A1 -> { b | a1 = mark }
        A2 -> { b | a2 = mark }
        A3 -> { b | a3 = mark }
        B1 -> { b | b1 = mark }
        B2 -> { b | b2 = mark }
        B3 -> { b | b3 = mark }
        C1 -> { b | c1 = mark }
        C2 -> { b | c2 = mark }
        C3 -> { b | c3 = mark }
        None -> b

