module UpdateModel exposing (updatePosition, updateDownPosition, updateUpPosition, updateSelected)

import SvgAst as S
--import ParseSVG exposing (..)
import List as L exposing (..)
import Model exposing (..)
import Maybe as M
import Html.Attributes as A
import Dict as D
import Result as R

updateLast xs x =
    let list =
      take ((length xs) - 1) xs
    in
      list ++ x

dot x y =
    [ [ ( x, y + 1 )
      , ( x + 1, y + 1 )
      , ( x + 1, y )
      , ( x + 1, y - 1 )
      , ( x, y - 1 )
      , ( x - 1, y - 1 )
      , ( x - 1, y )
      , ( x - 1, y + 1 )
      , ( x, y )
      ]
    ]

updatePathValue : S.Value -> Float -> Float -> S.Value
updatePathValue value x y =
    case value of
        (S.D elms) -> (S.D (elms ++ [(S.L x y)]))
        _ -> value

updateValueHelper : (String, S.Value) -> (String, S.Value) -> (String, S.Value)
updateValueHelper (key, value) (newKey, newValue) =
    if key == newKey then
        (newKey, newValue)
     else
         (key, value)

updateValue : (String, S.Value) -> List (String, S.Value) -> List (String, S.Value)
updateValue (newKey, newValue) xs =
    List.map (\(key, value) -> (key, (if key == newKey then newValue else value))) xs

valueToFloat : Float -> S.Value -> Float
valueToFloat default value =
    case value of
        S.Value v -> String.toFloat v |> Result.withDefault default
        _ -> default

updateCurrentObjectHelper : S.SvgAst -> Float -> Float -> S.SvgAst
updateCurrentObjectHelper object x y =
    case object of
        S.Tag "path" attrs objs ->
            S.Tag "path" (D.map (\key value -> updatePathValue value x y) attrs) objs
        S.Tag "ellipse" attrs objs ->
            let currentX = D.get "cx" attrs |> M.withDefault (S.Value (toString x)) |> valueToFloat x
                currentY = D.get "cy" attrs |> M.withDefault (S.Value (toString y)) |> valueToFloat y
                height = D.get "ry" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0
                width = D.get "rx" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0 in
            let newX = (if x < currentX then x else currentX)
                newY = (if y < currentY then y else currentY)
                newWidth = (if x < currentX then width + (currentX - x) else x - currentX)
                newHeight = (if y < currentY then height + (currentY - y) else y - currentY) in
            S.Tag "ellipse" (D.update "cy" (\_ -> Just (S.Value (toString newY))) attrs |> D.update "ry" (\_ -> Just (S.Value (toString newHeight))) |> D.update "rx" (\_ -> (Just (S.Value (toString newWidth)))) |> D.update "cx" (\_ -> Just (S.Value (toString newX)))) objs

        S.Tag "rect" attrs objs ->
            let currentX = D.get "x" attrs |> M.withDefault (S.Value (toString x)) |> valueToFloat x
                currentY = D.get "y" attrs |> M.withDefault (S.Value (toString y)) |> valueToFloat y
                height = D.get "height" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0
                width = D.get "width" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0 in
            let newX = (if x < currentX then x else currentX)
                newY = (if y < currentY then y else currentY)
                newWidth = (if x < currentX then width + (currentX - x) else x - currentX)
                newHeight = (if y < currentY then height + (currentY - y) else y - currentY) in
            S.Tag "rect" (D.update "y" (\_ -> Just (S.Value (toString newY))) attrs |> D.update "height" (\_ -> Just (S.Value (toString newHeight))) |> D.update "width" (\_ -> (Just (S.Value (toString newWidth)))) |> D.update "x" (\_ -> Just (S.Value (toString newX)))) objs

        _ -> object


defaultObject : S.SvgAst
defaultObject = S.Tag "Path" D.empty []

updateCurrentObject model x y =
    { model
        | x = x
        , y = y
        , currentObject = Just (updateCurrentObjectHelper (M.withDefault defaultObject model.currentObject) x y)
    }

updateLastPointOfCurrentObject model x y =
    { model
        | x = x
        , y = y
        , currentObject = Just (updateCurrentObjectHelper (M.withDefault defaultObject model.currentObject) x y)
    }

parseAst : List S.SvgAst -> S.SvgAst -> List S.SvgAst
parseAst svg object =
    case svg of
        (x::xs) ->
            case x of
                S.Tag "g" attr objects ->
                    (S.Tag "g" attr (object::objects))::xs
                _ -> x::(parseAst svg object)
        [] -> [(S.Tag "g" D.empty [object])]

addIdToObject : S.SvgAst -> Int -> S.SvgAst
addIdToObject svgast id =
    case svgast of
        (S.Tag name attrs children) ->
            S.Tag name (D.update "id" (\_ -> Just (S.Value (toString id))) attrs) children
        _ -> svgast

insertCurrentObject : List S.SvgAst -> M.Maybe S.SvgAst -> Int -> List S.SvgAst
insertCurrentObject svg object id =
    case object of
        (Just obj) ->
            parseAst svg (addIdToObject obj id)
        Nothing -> svg

newObjectAtPosition : Model -> Float -> Float -> Model
newObjectAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "path" (D.fromList [("stroke-width", S.Value model.currentStroke), ("stroke", S.Value model.currentColor), ("fill", S.Value "none"), ("stroke-linejoin", S.Value "round"), ("d", S.D [S.M x y])]) []), downX = x, downY = y }

newRectAtPosition : Model -> Float -> Float -> Model
newRectAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "rect" (D.fromList [("stroke-width", S.Value model.currentStroke), ("stroke", S.Value model.currentColor), ("stroke-linejoin", S.Value "round"), ("x", S.Value (toString x)), ("y", S.Value (toString y)), ("width", S.Value "0"), ("height", S.Value "0")]) []), downX = x, downY = y }

newCircleAtPosition : Model -> Float -> Float -> Model
newCircleAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "ellipse" (D.fromList [("stroke-width", S.Value model.currentStroke), ("stroke", S.Value model.currentColor), ("stroke-linejoin", S.Value "round"), ("cx", S.Value (toString x)), ("cy", S.Value (toString y)), ("rx", S.Value "0"), ("ry", S.Value "0")]) []), downX = x, downY = y }


updateDownPosition : Model -> Float -> Float -> Model
updateDownPosition model x y =
    case model.functionToggled of
        Line ->
            model
        Rect ->
            newRectAtPosition model x y
        Circle ->
            newCircleAtPosition model x y
        Draw ->
            newObjectAtPosition model x y
        Select ->
            { model | isDown = True,  downX = x, downY = y }

updateUpPosition : Model -> Float -> Float -> Model
updateUpPosition model x y =
    case model.functionToggled of
        Line ->
            case model.currentObject of
                Nothing ->
                    newObjectAtPosition model x y
                Just object ->
                    updateCurrentObject model x y
        Rect ->
            { model | x = x, y = y, isDown = False, currentObject = Nothing, currentId = model.currentId + 1,
               svg = insertCurrentObject model.svg model.currentObject model.currentId }

        Circle ->
            { model | x = x, y = y, isDown = False, currentObject = Nothing, currentId = model.currentId + 1,
               svg = insertCurrentObject model.svg model.currentObject model.currentId }


        Draw ->
            { model | x = x, y = y, isDown = False, currentObject = Nothing, currentId = model.currentId + 1,
               svg = insertCurrentObject model.svg model.currentObject model.currentId}
        Select ->
            { model | isDown = False }

valueToInt : S.Value -> Int
valueToInt val =
    case val of
        S.Value str ->
          Result.withDefault -1 (String.toInt str)
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

updateIfSameId : S.SvgAst -> S.SvgAst -> S.SvgAst
updateIfSameId a1 a2 =
    case a1 of
        S.Tag _ attr1 _ ->
            case a2 of
                S.Tag _ attr2 _ ->
                    let id1 = S.getStringAttribute "id" attr1 |> M.withDefault "0"
                        id2 = S.getStringAttribute "id" attr2 |> M.withDefault "1" in
                    if id1 == id2 then
                        a1
                    else
                        a2
                _ -> a2
        _ -> a2

updateAst : S.SvgAst -> S.SvgAst -> S.SvgAst
updateAst new ast =
    S.map (updateIfSameId new) ast

updateAsts : List S.SvgAst -> S.SvgAst -> List S.SvgAst
updateAsts asts new =
    L.map (updateAst new) asts

addToValue : Float ->  S.Value -> S.Value
addToValue dv val =
    case val of
        S.Value v ->
            R.map ((+) dv) (String.toFloat v) |> R.withDefault 0 |> toString |> S.Value
        _ -> val

shiftObject : Float -> Float -> List S.SvgAst -> List S.SvgAst
shiftObject dx dy =
    L.map (S.updateAttribute "x" (addToValue dx) >> S.updateAttribute "y" (addToValue dy))

updateSelectedObject model x y =
    let selected = selectedElements model.svg model.selectedId
        updatedSelected = shiftObject ((x - model.downX)) ((y - model.downY)) selected in
    { model | svg = updateAsts model.svg (L.head updatedSelected |> M.withDefault (S.Comment "default")), downY = y, downX = x, selected = selected }

updatePosition model x y =
    if model.isDown then
        case model.functionToggled of
            Draw ->
                updateCurrentObject model x y
            Line ->
                updateLastPointOfCurrentObject model x y
            Rect ->
                updateCurrentObject model x y
            Circle ->
                updateCurrentObject model x y
            Select -> updateSelectedObject model x y
    else
        { model | x = x, y = y }


updateSelected model id =
    { model | selectedId = Result.withDefault -1 (String.toInt id), test = id }
