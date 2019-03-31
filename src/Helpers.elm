module Helpers exposing (..)
-- import Debug exposing(log)
-- import Bool.Extra exposing (toString)

import Models exposing (..)
import List exposing (foldr)


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
        end = foldr (\x y -> y && (x == Empty)) True (listBoard brd)
    in
     case win of
        True -> case mrk of
            X -> WinX
            O -> WinO
            Empty -> On
        False -> case end of
            True -> Draw
            False -> On


-- Translates game state to string
checkGameString : GameState -> Mark -> String
checkGameString s m =
    case s of
        WinX -> "Player X wins!"
        WinO -> "Player O wins!"
        Draw -> "Draw."
        On -> "Move of player " ++ (fromMark (change m))
