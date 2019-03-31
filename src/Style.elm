module Style exposing (..)
import Html exposing (Attribute)
import Html.Attributes exposing (style)

bodyStyle : List (Attribute a)
bodyStyle = [ style "width" "500px"
            , style "display" "table"
            , style "table-layout" "fixed"
            , style "margin" "0 auto"
            , style "font-size" "24pt"
            , style "font-family" "Arial"
            , style "text-align" "center"
            ]   

cellStyle : List (Attribute a)
cellStyle = [  style "width" "100%"
            , style "height" "100%"
            , style "display" "table-cell"
            , style "border" "none"
            , style "height" "100%"
            , style "text-align" "center"
            ]   
btnStyle : List (Attribute a) 
btnStyle = [  style "width" "100px"
            , style "height" "100px"
            , style "margin-bottom" "20px"
            , style "font-size" "24pt"
            , style "text-align" "center"
            , style "vertical-align" "middle"
            ]
