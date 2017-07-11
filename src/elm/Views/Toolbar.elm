module Views.Toolbar exposing (toolbar)

import Html.Attributes as A exposing (style, value)
import Model exposing (..)
import Html
import Html.Events exposing (onInput, onClick, onWithOptions, on)
import List exposing (..)
import Json.Decode as Json
import Maybe as M
import Result as R


-- import Material.Icons.Content exposing (save)

import Color exposing (black, rgb)
import FontAwesome exposing (mouse_pointer, pencil, square, circle, floppy_o, folder_open, search_minus, search_plus)
import SvgAst as S
import SvgAstOperations exposing (getNodeId)

foregroundColor = rgb 242 241 239

fontSize =
    14


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
    Html.div [] [
         Html.input
             [ A.type_ "range"
             , A.min "0"
             , A.max "40"
             , value v
             , onInput ChangeStroke
             ]
             []
       , Html.text v
             ]


objectDetailRow : String -> S.SvgAst -> Html.Html Msg
objectDetailRow attribute ast =
    let
        value =
            S.getAttributeString attribute ast |> M.withDefault "undefined"
    in
        Html.tr []
            [ Html.td [] [ Html.text attribute ]
            , Html.td [] [ Html.input [ onInput (ChangeDetail attribute), A.value value ] [] ]
            ]


objectDetails : S.SvgAst -> Html.Html Msg
objectDetails ast =
    Html.div [ A.class "details" ]
        [ Html.h4 [] [Html.text "Details"]
        , Html.table []
            [ Html.tr []
                  [ objectDetailRow "id" ast
                  , objectDetailRow "stroke" ast
                  , objectDetailRow "stroke-width" ast
                  , objectDetailRow "fill" ast
                  ]
            ]
        ]


details : List S.SvgAst -> Html.Html Msg
details asts =
    let header = Html.h4 [] [ Html.text "Details"] in
    case asts of
        [] ->
            Html.div [] [ header, Html.text "Nothing selected" ]

        [ ast ] ->
            objectDetails ast

        _ ->
            Html.div [] [ header, Html.text ((toString (length asts)) ++ " elements selected") ]


toolbar model =
    let scale = model.scale |> String.toFloat |> R.withDefault 1 in
    Html.div
        [ A.class "toolbar"
        , (onWithOptions "keydown" { preventDefault = False, stopPropagation = True } (Json.succeed (NoOp)))
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
            [ Html.div [ A.class "scale" ]
                [ Html.button [ A.class "zoom", onClick (ChangeScale (scale - 0.01 |> toString)) ] [ search_minus foregroundColor fontSize ]
                , Html.input
                     [ A.type_ "range"
                     , A.min "0.01"
                     , A.step "0.01"
                     , A.max "10"
                     , onInput ChangeScale
                     , A.value model.scale
                     ] []
                , scale * 100 |> floor |> toString |> \x -> x ++ "%" |> Html.text
                , Html.button [ A.class "zoom", onClick (ChangeScale (scale + 0.01 |> toString)) ] [ search_plus foregroundColor fontSize ]
                ]
            , Html.div [ A.class "size" ]
                [ Html.input
                    [ A.value (toString model.width), onInput ChangeWidth ]
                    []
                , Html.text "x"
                , Html.input [ A.value (toString model.height), onInput ChangeHeight ] []
                ]
            , Html.div [ A.class "position"]
                [ model.x |> floor |> toString |> Html.text
                , Html.text "x"
                , model.y |> floor |> toString |> Html.text
                ]
            ]
        ]
