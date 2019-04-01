module Style            exposing (..)

import Html             exposing (Attribute)
import Html.Attributes  exposing (style)


board : List (Attribute a)
board = 
    [ style "width" "500px"
    , style "display" "table"
    , style "table-layout" "fixed"
    ]   


cell : List (Attribute a)
cell = 
    [ style "display" "table-cell"
    , style "border" "none"
    , style "text-align" "center"
    ]   

    
btn : List (Attribute a) 
btn = 
    [  style "width" "100px"
    , style "height" "100px"
    , style "margin-bottom" "20px"
    , style "font-size" "24pt"
    , style "text-align" "center"
    , style "vertical-align" "middle"
    ]

overlay : List (Attribute a)
overlay =
    [ style "height" "500px"
    , style "width" "500px"
    , style "vertical-align" "middle"
    , style "text-align" "center"
    , style "font-size" "24pt"
    , style "font-family" "Arial"
    , style "position" "fixed"
    , style "z-index" "2"
    , style "background-color" "rgba(255,255,255,0.8)"
    ]
    