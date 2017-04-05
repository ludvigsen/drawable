module Views.Drawing exposing (drawing)

import List as L exposing (..)
import Maybe as M
import Dict as D
import Model exposing (..)
import Svg exposing (node, svg)
import Svg.Attributes as SA exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes as A exposing (style, value, attribute)
import Html.Events exposing (onClick, onWithOptions)
import String
import SvgAst as S
import Maybe
import Json.Decode as Json
import Dict exposing (Dict)
import Result as R
import SvgAstOperations exposing (getBoundingRect)
import Html exposing (Html, text, div)


makeCoord : ( Float, Float ) -> String
makeCoord ( x, y ) =
    (toString x) ++ " " ++ (toString y)


makePathSegment : List ( Float, Float ) -> String
makePathSegment coords =
    "M" ++ String.join " L " (map makeCoord coords)


parseDElement : S.DElement -> String
parseDElement el =
    case el of
        S.M x y ->
            "M " ++ (toString x) ++ " " ++ (toString y)

        S.L x y ->
            "L " ++ (toString x) ++ " " ++ (toString y)


parseAttribute : ( S.Key, S.Value ) -> Svg.Attribute msg
parseAttribute attr =
    case attr of
        ( key, S.D elms ) ->
            d (String.join " " (map parseDElement elms))

        ( string, S.Value val ) ->
            A.attribute string val


parseAttributes : Dict S.Key S.Value -> List (Svg.Attribute msg)
parseAttributes attrs =
    Dict.toList attrs |> map parseAttribute


getId : Dict S.Key S.Value -> String
getId xs =
    let
        value =
            Dict.get "id" xs |> Maybe.withDefault (S.Value "defaultId")
    in
        case value of
            S.Value val ->
                val

            _ ->
                "defaultId"


drawSvgAst svgs =
    case svgs of
        S.Tag name attrs children ->
            let
                id =
                    getId attrs
            in
                -- if (toString selectedId) == id then
                -- node name  (((onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (SelectElement id)))::(parseAttributes attrs)) ++ ([fill "black"])) (drawSvgAsts selectedId children)
                --else
                node name (parseAttributes attrs) (drawSvgAsts children)

        -- ((onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (SelectElement id)))::(parseAttributes attrs)) (drawSvgAsts selectedId children)
        _ ->
            Svg.desc [] []


drawSvgAsts =
    map (drawSvgAst)


valueToInt : S.Value -> Int
valueToInt val =
    case val of
        S.Value str ->
            R.withDefault -1 (String.toInt str)

        _ ->
            -1


isSelected : Int -> S.SvgAst -> S.SvgAst -> S.SvgAst
isSelected id ast ast2 =
    case ast of
        S.Tag name attrs _ ->
            let
                elemId =
                    D.get "id" attrs |> M.withDefault (S.Value "-1") |> valueToInt
            in
                if id == elemId then
                    ast
                else
                    ast2

        _ ->
            ast2


selectedElements svg id =
    L.map (S.fold (isSelected id) (S.Comment "default")) svg


unpackVal val =
    case val of
        S.Value str ->
            str

        _ ->
            ""


getAttrs : S.SvgAst -> List (Svg.Attribute msg)
getAttrs ast =
    case ast of
        S.Tag n attrs o ->
            let
                ( x1, y1, x2, y2 ) =
                    getBoundingRect ast |> M.withDefault ( 0, 0, 0, 0 )
            in
                [ SA.x (toString x1), SA.y (toString y1), SA.width (toString (x2 - x1)), SA.height (toString (y2 - y1)) ]

        _ ->
            []


drawBoundingBox : S.SvgAst -> Svg.Svg msg
drawBoundingBox ast =
    let
        attrs =
            getAttrs ast
    in
        Svg.rect (attrs ++ [ SA.strokeDasharray "5, 5", SA.stroke "#4169E1", SA.fill "none", SA.strokeWidth "2" ]) []


drawSelected : Model -> List (Svg.Svg msg)
drawSelected model =
    let
        selected =
            model.selected

        selectingBox =
            if model.selecting then
                Svg.rect [ SA.strokeDasharray "5, 5", SA.stroke "#4169E1", SA.fill "none", SA.strokeWidth "2", SA.x (toString model.downX), SA.y (toString model.downY), SA.width (toString (model.x - model.downX)), SA.height (toString (model.y - model.downY)) ] []
            else
                Svg.g [] []
    in
        selectingBox :: (L.map drawBoundingBox selected)


containerStyles =
    style
        [ ( "position", "absolute" )
        , ( "right", "10rem" )
        , ( "left", "0" )
        , ( "top", "0" )
        , ( "bottom", "0" )
        ]


svgStyles model =
    style
        [ ( "width", "calc(" ++ (toString (model.width * 1)) ++ "px - 10rem)" )
        , ( "height", (toString (model.height * 1)) ++ "px" )
        , ( "background-color", "white" )
        ]


drawing model =
    div [ containerStyles ]
        [ svg [ A.id "image", svgStyles model ]
            ((drawSvgAsts ((Maybe.withDefault (S.Tag "g" Dict.empty []) model.currentObject) :: model.svg)) ++ (drawSelected model))
        ]
