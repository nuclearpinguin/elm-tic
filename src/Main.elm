module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, button, div, text, p)
import Html.Attributes exposing (style, attribute, hidden)
import Html.Events exposing (onClick)
import List exposing (..)
import Maybe exposing (..)
import Random exposing (int, step, initialSeed)
import Debug exposing (log)

import Style 


main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

-- Marking of board
type Mark = Empty | X | O


type alias Board = 
    { a1 : Mark, a2 : Mark, a3 : Mark
    , b1 : Mark, b2 : Mark, b3 : Mark
    , c1 : Mark, c2 : Mark, c3 : Mark
    }

type GameState = Draw | WinX | WinO | On | NewGame


type GameType = PlayerPlayer | PlayerComputer | Nil


type alias Game = 
    { board  : Board
    , player : Mark
    , state : GameState
    , gameType : GameType
    }


type PlayerAction = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3 | None


type alias Model = 
    { gameType : GameType,  action : PlayerAction}


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


checkGameString : GameState -> Mark -> String
checkGameString s m =
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


updateGame : Model -> Game -> Game
updateGame mod g =
    let
        plr = change g.player
        brd = updateBoard g.board plr mod.action 
        state = checkGame brd plr
        newGame = { g | board = brd, player = plr, state = state, gameType = mod.gameType}
    in
    case mod.gameType of
        PlayerComputer -> 
            case state of
                On -> makeMove newGame
                _ -> newGame
        PlayerPlayer -> newGame
        Nil -> init


update : Model -> Game -> Game
update m g = 
    let
        tmp = (m.gameType, g.state)
    in
    case tmp of
        (Nil, _ ) -> init
        (_, On) -> updateGame m g
        (_, NewGame) -> updateGame m g
        (_, _) -> init


-- Adjust board style
checkIfMarked : Mark -> GameState -> List (Attribute Model)
checkIfMarked m s =
    let
        stl = if (m == Empty) 
            then [style "background-color" "#9ACD32"] 
            else [attribute "disabled" ""]
    in
    case s of
        On -> Style.btnStyle ++ stl
        _ -> Style.btnStyle ++ stl ++ [attribute "disabled" ""]


-- Creates a button which handles board markings
makeBoardButton : GameType -> Mark -> PlayerAction -> GameState -> Html Model
makeBoardButton gt mrk act state =
    let
        stl = checkIfMarked mrk state
        nm = { gameType = gt,  action = act}
    in
    button (stl ++ [ onClick nm ]) [text (fromMark mrk)]
    

isGameActive : GameState -> List (Attribute a)
isGameActive s = 
    case s of
        On -> [style "display" "none"]
        _ -> [style "display" "block"]
            

showGameButtons : GameState -> List (Attribute a)
showGameButtons s =
    case s of
        NewGame -> [style "display" "inline"]
        _ -> [style "display" "none"]
                            

showNewGameButtons : GameState -> List (Attribute a)
showNewGameButtons s =
    case s of
        NewGame -> [style "display" "none"]
        _ -> [style "display" "inline"]

-- VIEW

view : Game -> Html Model
view game =
    let 
        brd = game.board
        state = game.state
        mrk = game.player
        gt = game.gameType
        gameStatus = checkGameString state mrk
    in
    div []
        [ div (Style.overlay ++ isGameActive state)
            [ p [] [text gameStatus]
            , div (showGameButtons state) 
                [ button 
                    [ style "font-size" "12pt" 
                    , onClick ( {gameType = PlayerPlayer,  action = None} )] [text "Player vs. Player"]
                , button 
                    [ style "font-size" "12pt" 
                    , style "margin-left" "50px" 
                    , onClick ( {gameType = PlayerComputer,  action = None})] [text "Player vs. Computer"]
                ]
            , div (showNewGameButtons state) 
                [ button 
                    [ style "font-size" "12pt" 
                    , onClick ( {gameType = Nil,  action = None} )] [text "Nawe game!"]
                ]
            ]
        , div []
            [ div Style.boardStyle
                [ div [style "display" "table-row"] 
                    [ div Style.cellStyle 
                        [ makeBoardButton gt brd.a1 A1 state
                        , makeBoardButton gt brd.a2 A2 state
                        , makeBoardButton gt brd.a3 A3 state
                        ]

                    , div Style.cellStyle 
                        [ makeBoardButton gt brd.b1 B1 state
                        , makeBoardButton gt brd.b2 B2 state
                        , makeBoardButton gt brd.b3 B3 state
                        ]
                    , div Style.cellStyle 
                        [ makeBoardButton gt brd.c1 C1 state
                        , makeBoardButton gt brd.c2 C2 state
                        , makeBoardButton gt brd.c3 C3 state
                        ]
                    ]
                ]
            ]
    ]


-- GAME STRATEGY

edges : List (PlayerAction)
edges = [
    A2, B1, B3, C2]


corners : List (PlayerAction)
corners = 
    [A1, A3, C1, C3]


cornersValues : Board -> List (Mark)
cornersValues b = 
    [b.a1, b.a3, b.c1, b.c3]


edgesValues : Board -> List (Mark)
edgesValues b = 
    [b.a2, b.b1, b.b3, b.c2]


-- Opponents mark and board
checkOpInCorner : Mark -> Board -> Bool
checkOpInCorner m b = 
    member m (cornersValues b)


-- Check if middle is still free
isMiddleFree : Board -> Bool
isMiddleFree b =
    case b.b2 of
        Empty -> True
        _ -> False


isEdgeFree : Board -> Bool
isEdgeFree b = 
    member Empty (edgesValues b)


putInCorner : Mark -> Board -> Board
putInCorner m b = 
    let
        zipped = List.map2 (\x y -> (x, y)) (cornersValues b)  corners
        (p, q) = unzip (filter (\(x, y) -> if (x == Empty) then True else False) zipped)
    in
    case (length p) of
        0 -> b
        _ -> case (head q) of
            Just A1 -> {b | a1 = m}
            Just A3 -> {b | a3 = m}
            Just C1 -> {b | c1 = m}
            Just C3 -> {b | c3 = m}
            _ -> b


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
            Just A2 -> {b | a2 = m}
            Just B1 -> {b | b1 = m}
            Just B3 -> {b | b3 = m}
            Just C2 -> {b | c2 = m}
            _ -> b


decideOnMove : Mark -> Board -> Board
decideOnMove m b =
    let
        opMark = change m
    in
    case (isMiddleFree b) of
        True -> {b | b2 = opMark}
        False -> case (isEdgeFree b) of
            True -> putOnEdge opMark b
            False -> putInCorner opMark b
            

makeMove : Game -> Game
makeMove g =
    let
        brd = decideOnMove g.player g.board
        plr = change  g.player
        state = checkGame brd plr
    in
        { g | board = brd, player = plr, state = state}