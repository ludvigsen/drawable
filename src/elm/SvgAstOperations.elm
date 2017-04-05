module SvgAstOperations exposing (getBoundingRect)

import SvgAst exposing (SvgAst(..), Key, Value(..), DElement(..), getStringAttribute)
import Dict exposing (get, Dict)
import List as L
import Maybe as M
import Result as R
import Char

unpackVal val =
    case val of
        Value str -> str
        _ -> ""

getFromDElement : Bool -> DElement -> Float
getFromDElement fst elem =
    case elem of
        M x y -> if fst then x else y
        L x y -> if fst then x else y

getYOfDElement : DElement -> Float
getYOfDElement = getFromDElement False

getXOfDElement : DElement -> Float
getXOfDElement = getFromDElement True

getMaxY : List DElement -> Float
getMaxY =
    L.map getYOfDElement >> L.maximum >> M.withDefault 0

getMaxX : List DElement -> Float
getMaxX =
    L.map getXOfDElement >> L.maximum >> M.withDefault 0

getMinX : List DElement -> Float
getMinX =
    L.map getXOfDElement >> L.minimum >> M.withDefault 0

getMinY : List DElement -> Float
getMinY =
    L.map getYOfDElement >> L.minimum >> M.withDefault 0


getBoundingRectForPath : Dict Key Value -> Maybe (Float, Float, Float, Float)
getBoundingRectForPath attrs =
    let d = get "d" attrs
        transform = Debug.log "transform" (getStringAttribute "transform" attrs |> M.withDefault "" |> String.split "," |> L.map (String.filter (\c -> (Char.isDigit c) || (c == '-'))) |> L.map String.toFloat |> L.map (R.withDefault 0)) in
    case d of
        Just (D ds) ->
            case transform of
                [transformX, transformY] ->
                    Just((getMinX ds) + transformX, (getMinY ds) + transformY, (getMaxX ds) + transformX, (getMaxY ds) + transformY)
                _ ->
                    Just(getMinX ds, getMinY ds, getMaxX ds, getMaxY ds)
        _ -> Nothing

getIntAttr : String -> Dict Key Value -> Float
getIntAttr key attrs =
    getStringAttribute key attrs
        |> M.map String.toFloat
        |> M.map R.toMaybe
        |> M.map (M.withDefault 0)
        |> M.withDefault 0

getBoundingRectForRect : Dict Key Value -> (Float, Float, Float, Float)
getBoundingRectForRect attrs =
    let x1 = (getIntAttr "x" attrs)
        y1 = (getIntAttr "y" attrs)
        x2 = x1 + (getIntAttr "width" attrs)
        y2 = y1 + (getIntAttr "height" attrs) in
    (x1, y1, x2, y2)

getBoundingRect : SvgAst -> Maybe (Float, Float, Float, Float)
getBoundingRect ast =
    case ast of
        Tag "path" attrs _ ->
            getBoundingRectForPath attrs
        Tag "rect" attrs _ ->
            Just <| getBoundingRectForRect attrs
        Tag "ellipse" attrs _ ->
            Just <| getBoundingRectForRect attrs
        _ -> Nothing
