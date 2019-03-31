module Strategy exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Random exposing (int, step, initialSeed)

import Models exposing (..)
import Helpers exposing (checkGame)

-- type FieldType = Corner | Middle | Edge

-- checkFieldType : 
type Edges = A2 | B1 | B3 | C2

type Corners = A1 | A3 | C1 | C3

edges : List (Edges)
edges = [A2, B1, B3, C2]

corners : List (Corners)
corners = [A1, A3, C1, C3]

cornersValues : Board -> List (Mark)
cornersValues b = [b.a1, b.a3, b.c1, b.c3]


edgesValues : Board -> List (Mark)
edgesValues b = [b.a2, b.b1, b.b3, b.c2]


-- Opponents mark and board
checkOpInCorner : Mark -> Board -> Bool
checkOpInCorner m b = member m (cornersValues b)


-- Check if middle is still free
isMiddleFree : Board -> Bool
isMiddleFree b =
    case b.b2 of
        Empty -> True
        _ -> False


isEdgeFree : Board -> Bool
isEdgeFree b = member Empty (edgesValues b)


putInCorner : Mark -> Board -> Board
putInCorner m b = 
    let
        zipped = List.map2 (\x y -> (x, y)) (cornersValues b)  corners
        (p, q) = unzip (filter (\(x, y) -> if (x == Empty) then True else False) zipped)
    in
    case (length p) of
        0 -> b
        _ -> case (head q) of
            Nothing -> b
            Just A1 -> {b | a1 = m}
            Just A3 -> {b | a3 = m}
            Just C1 -> {b | c1 = m}
            Just C3 -> {b | c3 = m}


putOnEdge : Mark -> Board -> Board
putOnEdge m b = 
    let
        zipped = List.map2 (\x y -> (x, y)) (edgesValues b) edges
        (p, q) = unzip (filter (\(x, y) -> if (x == Empty) then True else False) zipped)
        (i, _) = step (int 1 (length p)) (initialSeed 34) 
    in
    case (length p) of
        0 -> b
        _ -> case (head (reverse (take i q))) of
            Nothing -> b
            Just A2 -> {b | a2 = m}
            Just B1 -> {b | b1 = m}
            Just B3 -> {b | b3 = m}
            Just C2 -> {b | c2 = m}


makeMove : Mark -> Board -> Board
makeMove m b =
    let
        opMark = change m
    in
    case (isMiddleFree b) of
        True -> {b | b2 = opMark}
        False -> case (isEdgeFree b) of
            True -> putOnEdge opMark b
            False -> putInCorner opMark b
            

updateGame : Game -> Game
updateGame g =
    let
        brd = makeMove g.player g.board
        plr = change  g.player
        state = checkGame brd plr
    in
        { g | board = brd, player = plr, state = state}
    
    
