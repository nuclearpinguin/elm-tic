import Browser
import Html exposing (Html, Attribute, button, div, text, p)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)
import List exposing (foldl, foldr)


import Models exposing (..)
import Helpers
import Strategy exposing (updateGame)
import Style 

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL
-- Model of empty board
emptyBoard : Board
emptyBoard = 
    { a1 = Empty, a2 = Empty, a3 = Empty
    , b1 = Empty, b2 = Empty, b3 = Empty
    , c1 = Empty, c2 = Empty, c3 = Empty
    }

-- Model of initial game 
init : Game
init = 
    { board = emptyBoard
    , player = O
    , state = On
    }

-- UPDATE
-- Msg representa field of the board or reset 
type Msg = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3 | Reset


-- Updates game on user input
update : Msg -> Game -> Game
update m g = 
    let 
        brd = updateBoard g.board plr m 
        plr = change g.player
        state = Helpers.checkGame brd plr
    in
    case m of
        Reset -> init
        _ -> case state of
            On -> updateGame { g | board = brd, player = plr, state = state}
            _ -> { g | board = brd, player = plr, state = state}


-- Updates board on user input
updateBoard : Board -> Mark -> Msg -> Board
updateBoard b mark msg =
    case msg of
        A1 -> { b | a1 = mark }
        A2 -> { b | a2 = mark }
        A3 -> { b | a3 = mark }
        B1 -> { b | b1 = mark }
        B2 -> { b | b2 = mark }
        B3 -> { b | b3 = mark }
        C1 -> { b | c1 = mark }
        C2 -> { b | c2 = mark }
        C3 -> { b | c3 = mark }
        Reset -> emptyBoard


-- Adjust board's buttons style
checkIfMarked : Mark -> GameState -> List (Attribute Msg)
checkIfMarked m s =
    let
        stl = if (m == Empty) 
            then [style "background-color" "#ffdb4d"] 
            else [attribute "disabled" ""]
    in
    case s of
        On -> Style.btnStyle ++ stl
        _ -> Style.btnStyle ++ stl ++ [attribute "disabled" ""]


-- Creates a button which handles board markings
makeBoardButton : Mark -> Msg -> GameState -> Html Msg
makeBoardButton mrk pos state =
    let
        stl = checkIfMarked mrk state
    in
    button (stl ++ [ onClick pos ]) [text (fromMark mrk)]
    

-- VIEW
view : Game -> Html Msg
view game =
    let 
        brd = game.board
        state = game.state
        mrk = game.player
        gameStatus = Helpers.checkGameString game.state mrk
    in
    div Style.bodyStyle
        [ p [style "width" "500px"] [text gameStatus]
        , div [style "display" "table-row" ] 
            [ div Style.cellStyle 
                [ makeBoardButton brd.a1 A1 state
                , makeBoardButton brd.a2 A2 state
                , makeBoardButton brd.a3 A3 state
                ]

            , div Style.cellStyle 
                [ makeBoardButton brd.b1 B1 state
                , makeBoardButton brd.b2 B2 state
                , makeBoardButton brd.b3 B3 state
                ]
            , div Style.cellStyle 
                [ makeBoardButton brd.c1 C1 state
                , makeBoardButton brd.c2 C2 state
                , makeBoardButton brd.c3 C3 state
                ]
            ]
        , div [] [button [ onClick Reset ] [ text "New Game!" ]]
        ]
