module Drawing exposing (drawing)

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

makeCoord : ( Float, Float ) -> String
makeCoord ( x, y ) =
    (toString x) ++ " " ++ (toString y)

makePathSegment : List ( Float, Float ) -> String
makePathSegment coords =
    "M" ++ String.join " L " (map makeCoord coords)

parseDElement : S.DElement -> String
parseDElement el =
    case el of
        (S.M x y) ->
            "M " ++ (toString x) ++ " " ++ (toString y)
        (S.L x y) ->
            "L " ++ (toString x) ++ " " ++ (toString y)

parseAttribute : (S.Key, S.Value) -> Svg.Attribute msg
parseAttribute attr =
    case attr of
        (key, S.D elms) ->
            d (String.join " " (map parseDElement elms))
        (string, S.Value val) ->
            A.attribute string val

parseAttributes : Dict S.Key S.Value -> List (Svg.Attribute msg)
parseAttributes attrs =
    Dict.toList attrs |> map parseAttribute

getId : (Dict S.Key S.Value) -> String
getId xs =
    let value = Dict.get "id" xs |> Maybe.withDefault (S.Value "defaultId") in
    case value of
        S.Value val -> val
        _ -> "defaultId"

drawSvgAst selectedId svgs =
    case svgs of
        (S.Tag name attrs children) ->
            let id = getId attrs in
            -- if (toString selectedId) == id then
              -- node name  (((onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (SelectElement id)))::(parseAttributes attrs)) ++ ([fill "black"])) (drawSvgAsts selectedId children)
            --else
            node name
                ((onWithOptions "mousedown" { preventDefault = False, stopPropagation = True } (Json.succeed (SelectElement id)))::(parseAttributes attrs)) (drawSvgAsts selectedId children)
        _ -> Svg.desc [] []

drawSvgAsts selectedId = map (drawSvgAst selectedId)

valueToInt : S.Value -> Int
valueToInt val =
    case val of
        S.Value str ->
          R.withDefault -1 (String.toInt str)
        _ -> -1

isSelected : Int -> S.SvgAst -> S.SvgAst -> S.SvgAst
isSelected id ast ast2 =
    case ast of
        S.Tag name attrs _ ->
            let elemId = D.get "id" attrs |> M.withDefault (S.Value "-1") |> valueToInt in
            if id == elemId then
                ast
            else
                ast2
        _ -> ast2

selectedElements svg id = L.map (S.fold (isSelected id) (S.Comment "default")) svg

getFromDElement : Bool -> S.DElement -> Float
getFromDElement fst elem =
    case elem of
        S.M x y -> if fst then x else y
        S.L x y -> if fst then x else y

getYOfDElement : S.DElement -> Float
getYOfDElement = getFromDElement False

getXOfDElement : S.DElement -> Float
getXOfDElement = getFromDElement True

getMaxY : List S.DElement -> Float
getMaxY =
    L.map getYOfDElement >> L.maximum >> M.withDefault 0

getMaxX : List S.DElement -> Float
getMaxX =
    L.map getXOfDElement >> L.maximum >> M.withDefault 0

getMinX : List S.DElement -> Float
getMinX =
    L.map getXOfDElement >> L.minimum >> M.withDefault 0

getMinY : List S.DElement -> Float
getMinY =
    L.map getYOfDElement >> L.minimum >> M.withDefault 0

getDElement : S.Value -> List S.DElement
getDElement elem =
    case elem of
        S.D delems -> delems
        _ -> []

snd (_, val) = val

unpackVal val =
    case val of
        S.Value str -> str
        _ -> ""

getAttrsForPath : D.Dict S.Key S.Value -> List (Svg.Attribute msg)
getAttrsForPath attrs =
    let d = D.get "d" attrs in
    case d of
        Just (S.D ds) ->
            let x = (getMinX ds) - 10
                y = (getMinY ds) - 10
                width = ((getMaxX ds) - x) + 10
                height = ((getMaxY ds) - y) + 10
                transform = D.get "transform" attrs |> M.withDefault (S.Value "") |> unpackVal in
            [SA.x (toString x), SA.y (toString y), SA.width (toString width), SA.height (toString height), SA.transform transform]
        _ -> []

getIntAttr : String -> D.Dict S.Key S.Value -> Int
getIntAttr key attrs =
    S.getStringAttribute key attrs
        |> M.map String.toInt
        |> M.map R.toMaybe
        |> M.map (M.withDefault 0)
        |> M.withDefault 0

getAttrsForRect : D.Dict S.Key S.Value -> List (Svg.Attribute msg)
getAttrsForRect attrs =
    let x = (getIntAttr "x" attrs) - 10
        y = (getIntAttr "y" attrs) - 10
        width = (getIntAttr "width" attrs) + 20
        height = (getIntAttr "height" attrs) + 20 in
    [SA.x (toString x), SA.y (toString y), SA.width (toString width), SA.height (toString height)]

getAttrs : S.SvgAst -> List (Svg.Attribute msg)
getAttrs ast =
    case ast of
        S.Tag "path" attrs _ ->
            getAttrsForPath attrs
        S.Tag "rect" attrs _ ->
            getAttrsForRect attrs
        _ -> []

drawBoundingBox : S.SvgAst -> Svg.Svg msg
drawBoundingBox ast =
    let attrs = getAttrs ast in
    Svg.rect (attrs ++ [SA.strokeDasharray "5, 5", SA.stroke "#4169E1", SA.fill "none", SA.strokeWidth "2"]) []

drawSelected : Model -> List (Svg.Svg msg)
drawSelected model =
    let selected = selectedElements model.svg model.selectedId in
    L.map drawBoundingBox selected

drawing model =
    svg [ A.id "image", style [ ( "width", (toString (model.width * 1)) ++ "px" ), ( "height", (toString (model.height * 1)) ++ "px" ) ] ]
        ((drawSvgAsts model.selectedId ((Maybe.withDefault (S.Tag "g" Dict.empty []) model.currentObject)::model.svg)) ++ (drawSelected model))
