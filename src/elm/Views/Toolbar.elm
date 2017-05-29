module Views.Toolbar exposing (toolbar)

import Html.Attributes as A exposing (style, value)
import Model exposing (..)
import Html
import Html.Events exposing (onInput, onClick, onWithOptions, on)
import List exposing (..)
import Json.Decode as Json


-- import Material.Icons.Content exposing (save)

import Color exposing (black)
import FontAwesome exposing (mouse_pointer, pencil, square, circle, floppy_o, folder_open)
import SvgAst as S
import SvgAstOperations exposing (getNodeId)


fontSize =
    20


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


details : List S.SvgAst -> Html.Html Msg
details asts =
    case asts of
        [] ->
            Html.div [] [ Html.text "Nothing selected" ]

        [ ast ] ->
            let
                selectedId =
                    getNodeId ast
            in
                Html.div [] [ Html.text selectedId ]

        _ ->
            Html.div [] [ Html.text ((toString (length asts)) ++ " elements selected") ]


toolbar model =
    Html.div
        [ A.class "toolbar"
        , (onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (NoOp)))
        , (onWithOptions "mouseup" { preventDefault = False, stopPropagation = True } (Json.succeed (NoOp)))
        ]
        [ Html.div [ A.class "section" ]
            [ Html.input [ value model.currentColor, onInput ChangeColor, A.type_ "color" ] [] ]
        , Html.div [ A.class "section" ]
            [ (rangeSlider model.currentStroke) ]
        , Html.div [ A.class "section" ]
            [ Html.button
                [ onClick (ToggleFunction Select)
                , (if model.functionToggled == Select then
                    A.class "active"
                   else
                    A.class ""
                  )
                ]
                [ mouse_pointer black fontSize ]
            , Html.button
                [ onClick (ToggleFunction Draw)
                , (if model.functionToggled == Draw then
                    A.class "active"
                   else
                    A.class ""
                  )
                ]
                [ pencil black fontSize ]
            , Html.button
                [ onClick (ToggleFunction Line)
                , (if model.functionToggled == Line then
                    A.class "active"
                   else
                    A.class ""
                  )
                ]
                [ Html.div [ A.class "line-icon" ] [] ]
            , Html.button
                [ onClick (ToggleFunction Rect)
                , (if model.functionToggled == Rect then
                    A.class "active"
                   else
                    A.class ""
                  )
                ]
                [ square black fontSize ]
            , Html.button
                [ onClick (ToggleFunction Circle)
                , (if model.functionToggled == Circle then
                    A.class "active"
                   else
                    A.class ""
                  )
                ]
                [ (circle black fontSize) ]
            ]
        , Html.div [ A.class "section" ]
            [ Html.button [ onClick Save ] [ (floppy_o black fontSize) ]
            , Html.button [ onClick Load ] [ folder_open black fontSize ]
            ]
        , Html.div [ A.class "section" ]
            [ details model.selected
            ]
        , Html.div [ A.class "section bottom" ]
            [ Html.div []
                [ Html.input [ A.value (toString model.scale), onInput ChangeScale ] []
                , Html.text "x"
                ]
            , Html.input
                [ A.value (toString model.width), onInput ChangeWidth ]
                []
            , Html.text "x"
            , Html.input [ A.value (toString model.height), onInput ChangeHeight ] []
            ]
        ]
