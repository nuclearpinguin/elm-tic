module Main             exposing ( .. )

import Browser
import Html             exposing ( Html, Attribute, button, div, text, p )
import Html.Attributes  exposing ( style, attribute, hidden )
import Html.Events      exposing ( onClick )
import List             exposing ( .. )


import Game             exposing ( .. )
import Strategy         exposing ( makeMove )
import Style            


main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Msg = 
    { gameType : GameType,  action : PlayerAction}



-- UPDATE

updateGame : Msg -> Game -> Game
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


update : Msg -> Game -> Game
update m g = 
    let
        tmp = (m.gameType, g.state)
    in
    case tmp of
        (Nil, _ ) -> init
        (_, On) -> updateGame m g
        (_, NewGame) -> updateGame m g
        (_, _) -> init


changeBoardStyle : Mark -> GameState -> List (Attribute Msg)
changeBoardStyle m s =
    let
        stl = if (m == Empty) 
            then [style "background-color" "#9ACD32"] 
            else [attribute "disabled" ""]
    in
    case s of
        On -> Style.btn ++ stl
        _ -> Style.btn ++ stl ++ [attribute "disabled" ""]


makeBoardButton : GameType -> Mark -> PlayerAction -> GameState -> Html Msg
makeBoardButton gt mrk act state =
    let
        stl = changeBoardStyle mrk state
        nm = { gameType = gt,  action = act}
    in
    button (stl ++ [ onClick nm ]) [text (fromMark mrk)]
    

changeGameStyle : GameState -> List (Attribute a)
changeGameStyle s = 
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

view : Game -> Html Msg
view game =
    let 
        brd = game.board
        state = game.state
        mrk = game.player
        gt = game.gameType
        gameStatus = stateToString state mrk
    in
    div []
        [ div (Style.overlay ++ changeGameStyle state)
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
                    , onClick ( {gameType = Nil,  action = None} )] [text "New game!"]
                ]
            ]
        , div []
            [ div Style.board
                [ div [style "display" "table-row"] 
                    [ div Style.cell 
                        [ makeBoardButton gt brd.a1 A1 state
                        , makeBoardButton gt brd.a2 A2 state
                        , makeBoardButton gt brd.a3 A3 state
                        ]

                    , div Style.cell 
                        [ makeBoardButton gt brd.b1 B1 state
                        , makeBoardButton gt brd.b2 B2 state
                        , makeBoardButton gt brd.b3 B3 state
                        ]
                    , div Style.cell 
                        [ makeBoardButton gt brd.c1 C1 state
                        , makeBoardButton gt brd.c2 C2 state
                        , makeBoardButton gt brd.c3 C3 state
                        ]
                    ]
                ]
            ]
    ]
