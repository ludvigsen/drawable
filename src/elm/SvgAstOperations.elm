module SvgAstOperations exposing (getBoundingRect, removeLists, getId, getNodeId)

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

getTransform : Dict Key Value -> (Float, Float)
getTransform attrs =
    let transform = (getStringAttribute "transform" attrs |> M.withDefault "" |> String.split "," |> L.map (String.filter (\c -> (Char.isDigit c) || (c == '-'))) |> L.map String.toFloat |> L.map (R.withDefault 0)) in
    case transform of
        [transformX, transformY] ->
            (transformX, transformY)
        _ ->
            (0, 0)


getBoundingRectForPath : Dict Key Value -> Maybe (Float, Float, Float, Float)
getBoundingRectForPath attrs =
    let d = get "d" attrs in
    case d of
        Just (D ds) ->
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

getBoundingRectForEllipse : Dict Key Value -> (Float, Float, Float, Float)
getBoundingRectForEllipse attrs =
    let rx = (getIntAttr "rx" attrs)
        ry = (getIntAttr "ry" attrs)
        cx = (getIntAttr "cx" attrs)
        cy = (getIntAttr "cy" attrs) in
    (cx - rx, cy - ry, cx + rx, cy + ry)


-- TODO: RIGHT NOW add transform to bounding rect

getBoundingRectHelper : SvgAst -> Maybe (Float, Float, Float, Float)
getBoundingRectHelper ast =
    case ast of
        Tag "path" attrs _ ->
            getBoundingRectForPath attrs
        Tag "rect" attrs _ ->
            Just <| getBoundingRectForRect attrs
        Tag "ellipse" attrs _ ->
            Just <| getBoundingRectForEllipse attrs
        _ -> Nothing

getBoundingRect : SvgAst -> Maybe (Float, Float, Float, Float)
getBoundingRect ast =
    let boundingRect = getBoundingRectHelper ast in
    case boundingRect of
        Just (x1, y1, x2, y2) ->
            case ast of
                Tag name attrs _ ->
                    let (transformX, transformY) = getTransform attrs in
                    Just (x1 + transformX, y1 + transformY,
                              x2 + transformX, y2 + transformY)
                _ -> boundingRect
        Nothing ->
            Nothing



equalId : SvgAst -> SvgAst -> Bool
equalId ast1 ast2 =
    case ast1 of
        Tag _ attr1 _ ->
            case ast2 of
                Tag _ attr2 _ ->
                    let id1 = Debug.log "ID1: " <| getId attr1
                        id2 = Debug.log "ID2: " <| getId attr2
                    in
                        id1 == id2 && id1 /= "defaultId"
                _ -> False
        _ -> False


idExists : List SvgAst -> SvgAst -> Bool
idExists removing list =
    L.foldl (\curr res -> (equalId list curr) || res) False removing

removeList : List SvgAst -> SvgAst -> SvgAst -> SvgAst
removeList removing list base =
    case list of
        Tag name attrs children ->
            Tag name attrs (L.filter (\el -> idExists removing el |> not) children)
        _ -> list


removeLists : List SvgAst -> List SvgAst -> List SvgAst
removeLists removing lists =
    L.map (SvgAst.fold (removeList (Debug.log "REMOVING" removing)) (Comment "removed")) lists

getId : Dict Key Value -> String
getId xs =
    let
        value =
            Dict.get "id" xs |> Maybe.withDefault (Value "defaultId")
    in
        case value of
            Value val ->
                val

            _ ->
                "defaultId"


getNodeId : SvgAst -> String
getNodeId ast =
    case ast of
        Tag name attrs _ ->
            getId attrs
        _ -> "Unkown object without id"
