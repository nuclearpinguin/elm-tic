module Style            exposing (..)

import Html             exposing (Attribute)
import Html.Attributes  exposing (style)


boardStyle : List (Attribute a)
boardStyle = 
    [ style "width" "500px"
    , style "display" "table"
    , style "table-layout" "fixed"
    ]   


cellStyle : List (Attribute a)
cellStyle = 
    [ style "display" "table-cell"
    , style "border" "none"
    , style "text-align" "center"
    ]   

    
btnStyle : List (Attribute a) 
btnStyle = 
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