module UpdateModel exposing (updatePosition, updateDownPosition, updateUpPosition, updateKeyDown)

import SvgAst as S
import SvgAstOperations exposing (getBoundingRect)


--import ParseSVG exposing (..)

import List as L exposing (..)
import Model exposing (..)
import Maybe as M
import Html.Attributes as A
import Dict as D
import Result as R
import Char
import Keyboard exposing (KeyCode)


updateLast xs x =
    let
        list =
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
        S.D elms ->
            (S.D (elms ++ [ (S.L x y) ]))

        _ ->
            value


updateValueHelper : ( String, S.Value ) -> ( String, S.Value ) -> ( String, S.Value )
updateValueHelper ( key, value ) ( newKey, newValue ) =
    if key == newKey then
        ( newKey, newValue )
    else
        ( key, value )


updateValue : ( String, S.Value ) -> List ( String, S.Value ) -> List ( String, S.Value )
updateValue ( newKey, newValue ) xs =
    List.map
        (\( key, value ) ->
            ( key
            , (if key == newKey then
                newValue
               else
                value
              )
            )
        )
        xs


valueToFloat : Float -> S.Value -> Float
valueToFloat default value =
    case value of
        S.Value v ->
            String.toFloat v |> Result.withDefault default

        _ ->
            default


updateCurrentObjectHelper : S.SvgAst -> Float -> Float -> S.SvgAst
updateCurrentObjectHelper object x y =
    case object of
        S.Tag "path" attrs objs ->
            S.Tag "path" (D.map (\key value -> updatePathValue value x y) attrs) objs

        S.Tag "ellipse" attrs objs ->
            let
                currentX =
                    D.get "cx" attrs |> M.withDefault (S.Value (toString x)) |> valueToFloat x

                currentY =
                    D.get "cy" attrs |> M.withDefault (S.Value (toString y)) |> valueToFloat y

                height =
                    D.get "ry" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0

                width =
                    D.get "rx" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0
            in
                let
                    newX =
                        (if x < currentX then
                            x
                         else
                            currentX
                        )

                    newY =
                        (if y < currentY then
                            y
                         else
                            currentY
                        )

                    newWidth =
                        (if x < currentX then
                            width + (currentX - x)
                         else
                            x - currentX
                        )

                    newHeight =
                        (if y < currentY then
                            height + (currentY - y)
                         else
                            y - currentY
                        )
                in
                    S.Tag "ellipse" (D.update "cy" (\_ -> Just (S.Value (toString newY))) attrs |> D.update "ry" (\_ -> Just (S.Value (toString newHeight))) |> D.update "rx" (\_ -> (Just (S.Value (toString newWidth)))) |> D.update "cx" (\_ -> Just (S.Value (toString newX)))) objs

        S.Tag "rect" attrs objs ->
            let
                currentX =
                    D.get "x" attrs |> M.withDefault (S.Value (toString x)) |> valueToFloat x

                currentY =
                    D.get "y" attrs |> M.withDefault (S.Value (toString y)) |> valueToFloat y

                height =
                    D.get "height" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0

                width =
                    D.get "width" attrs |> M.withDefault (S.Value (toString 0)) |> valueToFloat 0
            in
                let
                    newX =
                        (if x < currentX then
                            x
                         else
                            currentX
                        )

                    newY =
                        (if y < currentY then
                            y
                         else
                            currentY
                        )

                    newWidth =
                        (if x < currentX then
                            width + (currentX - x)
                         else
                            x - currentX
                        )

                    newHeight =
                        (if y < currentY then
                            height + (currentY - y)
                         else
                            y - currentY
                        )
                in
                    S.Tag "rect" (D.update "y" (\_ -> Just (S.Value (toString newY))) attrs |> D.update "height" (\_ -> Just (S.Value (toString newHeight))) |> D.update "width" (\_ -> (Just (S.Value (toString newWidth)))) |> D.update "x" (\_ -> Just (S.Value (toString newX)))) objs

        _ ->
            object


defaultObject : S.SvgAst
defaultObject =
    S.Tag "Path" D.empty []


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
        x :: xs ->
            case x of
                S.Tag "g" attr objects ->
                    (S.Tag "g" attr (object :: objects)) :: xs

                _ ->
                    x :: (parseAst svg object)

        [] ->
            [ (S.Tag "g" D.empty [ object ]) ]


addIdToObject : S.SvgAst -> Int -> S.SvgAst
addIdToObject svgast id =
    case svgast of
        S.Tag name attrs children ->
            S.Tag name (D.update "id" (\_ -> Just (S.Value (toString id))) attrs) children

        _ ->
            svgast


insertCurrentObject : List S.SvgAst -> M.Maybe S.SvgAst -> Int -> List S.SvgAst
insertCurrentObject svg object id =
    case object of
        Just obj ->
            parseAst svg (addIdToObject obj id)

        Nothing ->
            svg


newObjectAtPosition : Model -> Float -> Float -> Model
newObjectAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "path" (D.fromList [ ( "stroke-width", S.Value model.currentStroke ), ( "stroke", S.Value model.currentColor ), ( "fill", S.Value "none" ), ( "stroke-linejoin", S.Value "round" ), ( "d", S.D [ S.M x y ] ) ]) []), downX = x, downY = y }


newRectAtPosition : Model -> Float -> Float -> Model
newRectAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "rect" (D.fromList [ ( "stroke-width", S.Value model.currentStroke ), ( "stroke", S.Value model.currentColor ), ( "stroke-linejoin", S.Value "round" ), ( "x", S.Value (toString x) ), ( "y", S.Value (toString y) ), ( "width", S.Value "0" ), ( "height", S.Value "0" ) ]) []), downX = x, downY = y }


newCircleAtPosition : Model -> Float -> Float -> Model
newCircleAtPosition model x y =
    { model | isDown = True, currentObject = Just (S.Tag "ellipse" (D.fromList [ ( "stroke-width", S.Value model.currentStroke ), ( "stroke", S.Value model.currentColor ), ( "stroke-linejoin", S.Value "round" ), ( "cx", S.Value (toString x) ), ( "cy", S.Value (toString y) ), ( "rx", S.Value "0" ), ( "ry", S.Value "0" ) ]) []), downX = x, downY = y }


intersects : ( Float, Float, Float, Float ) -> S.SvgAst -> Bool
intersects ( x1, y1, x2, y2 ) ast =
    case ast of
        S.Tag name attributes _ ->
            let
                ( ix1, iy1, ix2, iy2 ) =
                    getBoundingRect ast |> M.withDefault ( 0, 0, 0, 0 )
            in
                if
                    ((x1 <= ix1 && ix1 <= x2) || (x1 <= ix2 && ix2 <= x2))
                        && ((y1 <= iy1 && iy1 <= y2) || (y1 <= iy2 && iy2 <= y2))
                then
                    True
                else
                    False

        _ ->
            False


isInside : Maybe ( Float, Float, Float, Float ) -> Float -> Float -> Bool
isInside rect x y =
    case rect of
        Just ( x1, y1, x2, y2 ) ->
            if x < x1 || x > x2 || y < y1 || y > y2 then
                False
            else
                True

        Nothing ->
            False


updateDownPosition : Model -> Float -> Float -> Model
updateDownPosition model x y =
    if isInside (Just ( toFloat model.width, toFloat model.height, toFloat (model.width + 20), toFloat (model.height + 20) )) x y then
        { model | resizing = True }
    else
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
                let
                    selected =
                        L.map
                            (S.fold
                                (\ast asts ->
                                    if isInside (getBoundingRect ast) x y then
                                        (ast :: asts)
                                    else
                                        asts
                                )
                                []
                            )
                            model.svg
                            |> L.concat

                    selectedIds =
                        L.map (\ast -> (S.getAttributeString "id" ast) |> M.withDefault "-1") selected
                in
                    { model
                        | isDown = True
                        , downX = x
                        , downY = y
                        , selected =
                            if L.length model.selected > 1 && L.length selected > 0 then
                                model.selected
                            else
                                selected
                        , selectedIds = selectedIds
                        , selecting = L.length selected == 0
                    }


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
            { model
                | x = x
                , y = y
                , isDown = False
                , resizing = False
                , currentObject = Nothing
                , currentId = model.currentId + 1
                , svg = insertCurrentObject model.svg model.currentObject model.currentId
            }

        Circle ->
            { model
                | x = x
                , y = y
                , isDown = False
                , resizing = False
                , currentObject = Nothing
                , currentId = model.currentId + 1
                , svg = insertCurrentObject model.svg model.currentObject model.currentId
            }

        Draw ->
            { model
                | x = x
                , y = y
                , isDown = False
                , resizing = False
                , currentObject = Nothing
                , currentId = model.currentId + 1
                , svg = insertCurrentObject model.svg model.currentObject model.currentId
            }

        Select ->
            { model | isDown = False, selecting = False, resizing = False }


valueToInt : S.Value -> Int
valueToInt val =
    case val of
        S.Value str ->
            Result.withDefault -1 (String.toInt str)

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


updateIfSameId : S.SvgAst -> S.SvgAst -> S.SvgAst
updateIfSameId a1 a2 =
    case a1 of
        S.Tag _ attr1 _ ->
            case a2 of
                S.Tag _ attr2 _ ->
                    let
                        id1 =
                            S.getStringAttribute "id" attr1 |> M.withDefault "-1"

                        id2 =
                            S.getStringAttribute "id" attr2 |> M.withDefault "-2"
                    in
                        if id1 == id2 then
                            a1
                        else
                            a2

                _ ->
                    a2

        _ ->
            a2


updateAst : S.SvgAst -> S.SvgAst -> S.SvgAst
updateAst new ast =
    S.map (updateIfSameId new) ast


updateAsts : List S.SvgAst -> List S.SvgAst -> List S.SvgAst
updateAsts asts newAsts =
    L.foldr (\ast updated -> L.map (updateAst ast) updated) asts newAsts


addToValue : Float -> S.Value -> S.Value
addToValue dv val =
    case val of
        S.Value v ->
            R.map ((+) dv) (String.toFloat v) |> R.withDefault 0 |> toString |> S.Value

        _ ->
            val


mtox : S.Value -> Float
mtox el =
    case el of
        S.D ((S.M x _) :: rest) ->
            x

        _ ->
            0


mtoy : S.Value -> Float
mtoy el =
    case el of
        S.D ((S.M _ y) :: rest) ->
            y

        _ ->
            0


shiftObject : Float -> Float -> Float -> Float -> S.SvgAst -> S.SvgAst
shiftObject x dx y dy obj =
    case obj of
        S.Tag "path" attr objs ->
            let
                pathX =
                    D.get "d" attr |> M.withDefault (S.D []) |> mtox

                pathY =
                    D.get "d" attr |> M.withDefault (S.D []) |> mtoy

                transform =
                    Debug.log "transform" (S.getStringAttribute "transform" attr |> M.withDefault "" |> String.split "," |> L.map (String.filter (\c -> (Char.isDigit c) || (c == '-'))) |> L.map String.toFloat |> L.map (R.withDefault 0))
            in
                case transform of
                    [ transformX, transformY ] ->
                        S.Tag "path" (D.update "transform" (\_ -> Just (S.Value ("translate(" ++ (toString (transformX + (x - dx))) ++ "," ++ (toString (transformY + (y - dy))) ++ ")"))) attr) objs

                    _ ->
                        S.Tag "path" (D.update "transform" (\_ -> Just (S.Value ("translate(" ++ (toString (x - dx)) ++ "," ++ (toString (y - dy)) ++ ")"))) attr) objs

        _ ->
            (S.updateAttribute "x" (addToValue (x - dx)) >> S.updateAttribute "y" (addToValue (y - dy))) obj


shiftObjects : Float -> Float -> Float -> Float -> List S.SvgAst -> List S.SvgAst
shiftObjects x dx y dy =
    L.map (shiftObject x dx y dy)


updateSelectedObject model x y =
    if model.selecting then
        let
            selected =
                L.map
                    (S.fold
                        (\ast asts ->
                            if intersects ( model.downX, model.downY, x, y ) ast then
                                Debug.log "ASTS: " (ast :: asts)
                            else
                                asts
                        )
                        []
                    )
                    model.svg
                    |> L.concat
        in
            { model | x = x, y = y, selected = selected }
    else
        let
            updatedSelected =
                Debug.log "updateSelected: " <| shiftObjects x model.downX y model.downY model.selected
        in
            { model | svg = updateAsts model.svg updatedSelected, downY = y, downX = x, selected = updatedSelected }


updatePosition : Model -> Float -> Float -> Model
updatePosition model x y =
    if model.isDown then
        if model.resizing then
            { model | width = truncate ((toFloat model.width) + (x - model.downX)), height = truncate ((toFloat model.height) + (y - model.downY)), downX = x, downY = y }
        else
            case model.functionToggled of
                Draw ->
                    updateCurrentObject model x y

                Line ->
                    updateLastPointOfCurrentObject model x y

                Rect ->
                    updateCurrentObject model x y

                Circle ->
                    updateCurrentObject model x y

                Select ->
                    updateSelectedObject model x y
    else
        { model | x = x, y = y }


--updateId : List S.SvgAst -> List S.SvgAst
--updateId xs =

paste : List S.SvgAst -> List S.SvgAst -> Float -> Float -> List S.SvgAst
paste selected ast x y =
    (S.Tag "g" (D.fromList [("transform", S.Value ("translate(" ++ (toString (x)) ++ "," ++ (toString (y)) ++ ")"))]) selected)::ast


updateKeyDown : Model -> KeyCode -> Model
updateKeyDown model code =
    case Debug.log "keydown" model.currentKey of
        91 ->
            case Debug.log "KEYDOWN" code of
                -- Copy
                67 ->
                    {model | clipboard = model.selected }
                -- Paste
                86 ->
                    {model | svg = paste model.clipboard model.svg model.x model.y }
                _ ->
                    model
        _ ->
          { model | currentKey = code }
