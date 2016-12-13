module Toolbar exposing (toolbar)

import Html.Attributes as A exposing (style, value)
import Model exposing (..)
import Html
import Html.Events exposing (onInput, onClick, onWithOptions)
import List exposing (..)
import Json.Decode as Json


getColorBoxStyle : String -> Html.Attribute a
getColorBoxStyle color =
    style
        [ ( "background", color )
        , ( "width", "2rem" )
        , ( "height", "2rem" )
        , ( "display", "inline-block" )
        ]


decoder : a -> Msg
decoder =
    (always NoOp)


createColorBox : String -> Html.Html Msg
createColorBox color =
    Html.div
        [ (getColorBoxStyle color)
        , (onWithOptions "click" { preventDefault = True, stopPropagation = True } (Json.succeed (ChangeColor color)))
        ]
        []


createColorBoxes : List String -> List (Html.Html Msg)
createColorBoxes colors =
    map createColorBox colors


colors : List String
colors =
    [ "#2a2c2e"
    , "#e20800"
    , "#d0021b"
    , "#5cbd5c"
    , "#5cb85c"
    , "#6dc20f"
    , "#ffffff"
    , "#a8a5a6"
    , "#e34a3e"
    , "#f47f30"
    , "#ffc200"
    , "#91c591"
    , "#c0e3c0"
    , "#4d91e2"
    , "#f5f9fd"
    , "#387ecf"
    , "#4a90e2"
    , "#475b72"
    , "#304155"
    , "#1d7ab7"
    , "#2e71b2"
    , "#223246"
    , "#a0a9ba"
    , "#596479"
    , "#000000"
    , "#dddddd"
    , "#ececec"
    , "#fcfcfc"
    , "#a4a4a4"
    , "#acacac"
    , "#cccccc"
    , "#969696"
    , "#e5e5e5"
    , "#f5f5f5"
    , "#888888"
    , "#e6e5e5"
    , "#e1e1e1"
    , "#ea4ccb"
    , "#f46c30"
    , "#edbc64"
    , "#6495ed"
    , "#f9f0e6"
    ]


rangeSlider : String -> Html.Html Msg
rangeSlider v =
    Html.input
        [ A.type_ "range"
        , A.min "0"
        , A.max "40"
        , value v
        , onInput ChangeStroke
        ]
        []


toolbarStyle : Html.Attribute a
toolbarStyle =
    style
        [ ( "position", "absolute" )
        , ( "right", "0" )
        , ( "width", "10rem" )
        , ( "z-index", "1" )
        ]


toolbar model =
    Html.div
        [ toolbarStyle
        , A.class "toolbar"
        , (onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (NoOp)))
        , (onWithOptions "mouseup" { preventDefault = False, stopPropagation = True } (Json.succeed (NoOp)))
        ]
        [ Html.div [] (createColorBoxes colors)
        , Html.input [ value model.currentColor, onInput ChangeColor, A.type_ "color" ] []
        , (rangeSlider model.currentStroke)
        ]
