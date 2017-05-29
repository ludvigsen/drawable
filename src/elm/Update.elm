module Update exposing (update)

import Ports exposing (..)
import SvgAst as S
import SvgAstOperations exposing (getBoundingRect, removeLists)
import Navigation exposing (newUrl)
import List as L exposing (..)
import Model exposing (..)
import Maybe as M
import Html.Attributes as A
import Dict as D
import Result as R
import Char
import Keyboard exposing (KeyCode)
import WebSocket
import Uuid
import Random.Pcg exposing (step, initialSeed)
import Json.Encode as JE
import Json.Decode as JD
import SvgAst.Parser as SP
import SvgAst.Decode as SD
import SvgAst.Encode as SE
import Config exposing (backend)


sendInsertMessage : Model -> String
sendInsertMessage model =
    JE.encode 0 <|
        JE.object
            [ ( "event", JE.string "insert" )
            , ( "documentId", JE.string <| M.withDefault "" model.documentId )
            , ( "user", JE.string (M.withDefault "" (M.map Uuid.toString model.uuid)) )
            , ( "payload", (M.withDefault JE.null (M.map SE.encode model.currentObject)) )
            ]


sendUpdateMessage : Model -> List S.SvgAst -> String
sendUpdateMessage model asts =
    JE.encode 0 <|
        JE.object
            [ ( "event", JE.string "update" )
            , ( "documentId", JE.string <| M.withDefault "" model.documentId )
            , ( "user", JE.string (M.withDefault "" (M.map Uuid.toString model.uuid)) )
            , ( "payload", JE.list (L.map SE.encode asts) )
            ]


sendDeleteMessage : Model -> List S.SvgAst -> String
sendDeleteMessage model asts =
    JE.encode 0 <|
        JE.object
            [ ( "event", JE.string "delete" )
            , ( "documentId", JE.string <| M.withDefault "" model.documentId )
            , ( "user", JE.string (M.withDefault "" (M.map Uuid.toString model.uuid)) )
            , ( "payload", JE.list (L.map SE.encode asts) )
            ]


sendConnectMessage : Maybe String -> Uuid.Uuid -> String
sendConnectMessage documentId uuid =
    JE.encode 0 <|
        JE.object
            [ ( "event", JE.string "connect" )
            , ( "documentId", JE.string (M.withDefault "" documentId) )
            , ( "user", JE.string <| Uuid.toString uuid )
            , ( "payload", JE.string <| Uuid.toString uuid )
            ]


type Payload
    = Ast S.SvgAst
    | AstList (List S.SvgAst)
    | Uuid String
    | Empty


type alias Event =
    { documentId : Maybe String
    , user : Maybe String
    , event : String
    , payload : Payload
    }


defaultEvent : Event
defaultEvent =
    { event = ""
    , user = Nothing
    , documentId = Nothing
    , payload = Empty
    }


decodePayload : JD.Decoder Payload
decodePayload =
    JD.oneOf [ JD.map AstList (JD.list SD.decode), JD.map Ast SD.decode, JD.map Uuid JD.string, JD.null Empty ]


decodeEvent : String -> Result String Event
decodeEvent str =
    JD.decodeString
        (JD.map4 Event
            (JD.maybe <| JD.field "documentId" JD.string)
            (JD.maybe <| JD.field "user" JD.string)
            (JD.field "event" JD.string)
            (JD.field "payload" decodePayload)
        )
        str


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


insertObject : List S.SvgAst -> M.Maybe S.SvgAst -> Int -> List S.SvgAst
insertObject svg object id =
    case object of
        Just obj ->
            parseAst svg (addIdToObject obj id)

        Nothing ->
            svg


newLine : Model -> ( Float, Float ) -> ( Float, Float ) -> S.SvgAst
newLine model ( x1, y1 ) ( x2, y2 ) =
    S.Tag "path" (D.fromList [ ( "stroke-width", S.Value model.currentStroke ), ( "stroke", S.Value model.currentColor ), ( "fill", S.Value "none" ), ( "stroke-linejoin", S.Value "round" ), ( "d", S.D [ S.M x1 y1, S.L x2 y2 ] ) ]) []


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
updateDownPosition model unscaledX unscaledY =
    let
        x =
            unscaledX / model.scale

        y =
            unscaledY / model.scale
    in
        if x > toFloat model.width - (20 / model.scale) && x < toFloat (model.width) && y > toFloat model.height - (20 / model.scale) && y < toFloat model.height then
            { model | resizing = True, isDown = True, downX = x, downY = y }
        else
            case model.functionToggled of
                Line ->
                    { model | isDown = True, downX = x, downY = y }

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


updateUpPosition : Model -> Float -> Float -> ( Model, Cmd msg )
updateUpPosition model unscaledX unscaledY =
    let
        x =
            unscaledX / model.scale

        y =
            unscaledY / model.scale
    in
        case model.functionToggled of
            Line ->
                ( { model
                      | x = x
                      , y = y
                      , isDown = False
                      , resizing = False
                      , currentObject = Nothing
                      , currentId = model.currentId + 1
                      , svg = insertObject model.svg model.currentObject model.currentId
                  }
                , WebSocket.send backend (sendInsertMessage model)
                )

            Rect ->
                ( { model
                    | x = x
                    , y = y
                    , isDown = False
                    , resizing = False
                    , currentObject = Nothing
                    , currentId = model.currentId + 1
                    , svg = insertObject model.svg model.currentObject model.currentId
                  }
                , WebSocket.send backend (sendInsertMessage model)
                )

            Circle ->
                ( { model
                    | x = x
                    , y = y
                    , isDown = False
                    , resizing = False
                    , currentObject = Nothing
                    , currentId = model.currentId + 1
                    , svg = insertObject model.svg model.currentObject model.currentId
                  }
                , WebSocket.send backend (sendInsertMessage model)
                )

            Draw ->
                ( { model
                    | x = x
                    , y = y
                    , isDown = False
                    , resizing = False
                    , currentObject = Nothing
                    , currentId = model.currentId + 1
                    , svg = insertObject model.svg model.currentObject model.currentId
                  }
                , WebSocket.send backend (sendInsertMessage model)
                )

            Select ->
                ( { model | isDown = False, selecting = False, resizing = False }
                , Cmd.none
                )


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
        S.Tag name attr objs ->
            let
                transform =
                    (S.getStringAttribute "transform" attr |> M.withDefault "" |> String.split "," |> L.map (String.filter (\c -> (Char.isDigit c) || (c == '-'))) |> L.map String.toFloat |> L.map (R.withDefault 0))
            in
                case transform of
                    [ transformX, transformY ] ->
                        S.Tag name (D.update "transform" (\_ -> Just (S.Value ("translate(" ++ (toString (transformX + (x - dx))) ++ "," ++ (toString (transformY + (y - dy))) ++ ")"))) attr) objs

                    _ ->
                        S.Tag name (D.update "transform" (\_ -> Just (S.Value ("translate(" ++ (toString (x - dx)) ++ "," ++ (toString (y - dy)) ++ ")"))) attr) objs

        _ ->
            obj


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
                                (ast :: asts)
                            else
                                asts
                        )
                        []
                    )
                    model.svg
                    |> L.concat
        in
            ( { model | x = x, y = y, selected = selected }, Cmd.none )
    else
        let
            updatedSelected =
                shiftObjects x model.downX y model.downY model.selected
        in
            ( { model | svg = updateAsts model.svg updatedSelected, downY = y, downX = x, selected = updatedSelected }, WebSocket.send backend (sendUpdateMessage model updatedSelected) )


updatePosition : Model -> Float -> Float -> ( Model, Cmd msg )
updatePosition model unscaledX unscaledY =
    let
        x =
            unscaledX / model.scale

        y =
            unscaledY / model.scale
    in
        if model.isDown then
            if model.resizing then
                ( { model | width = truncate ((toFloat model.width) + (x - model.downX)), height = truncate ((toFloat model.height) + (y - model.downY)), downX = x, downY = y }, Cmd.none )
            else
                case model.functionToggled of
                    Draw ->
                        ( updateCurrentObject model x y, Cmd.none )

                    Line ->
                        if model.isDown then
                            ( { model
                                  | currentObject =
                                    Just (newLine model ( model.downX, model.downY ) ( x, y ))
                              }
                            , Cmd.none
                            )
                        else
                            ( model, Cmd.none )


                    Rect ->
                        ( updateCurrentObject model x y, Cmd.none )

                    Circle ->
                        ( updateCurrentObject model x y, Cmd.none )

                    Select ->
                        updateSelectedObject model x y
        else
            ( { model | x = x, y = y }, Cmd.none )


paste : List S.SvgAst -> List S.SvgAst -> Float -> Float -> List S.SvgAst
paste selected ast x y =
    (S.Tag "g" (D.fromList [ ( "transform", S.Value ("translate(" ++ (toString (x)) ++ "," ++ (toString (y)) ++ ")") ) ]) selected) :: ast


updateKeyDown : Model -> KeyCode -> ( Model, Cmd msg )
updateKeyDown model code =
    case Debug.log "keydown" model.currentKey of
        91 ->
            case Debug.log "KEYDOWN" code of
                -- Copy
                67 ->
                    ( { model | clipboard = model.selected }, Cmd.none )

                -- Paste
                86 ->
                    ( { model | svg = paste model.clipboard model.svg model.x model.y }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- Delete
        _ ->
            case code of
                8 ->
                    ( { model | svg = removeLists model.selected model.svg }, WebSocket.send backend (sendDeleteMessage model model.selected) )

                _ ->
                    ( { model | currentKey = code }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangeScale string ->
            let
                newScale =
                    String.toFloat string |> R.withDefault model.scale
            in
                ( { model | scale = newScale }, Cmd.none )

        ChangeWidth string ->
            let
                newWidth =
                    String.toInt string |> R.withDefault model.width
            in
                ( { model | width = newWidth }, Cmd.none )

        ChangeHeight string ->
            let
                newHeight =
                    String.toInt string |> R.withDefault model.height
            in
                ( { model | height = newHeight }, Cmd.none )

        KeyDown code ->
            updateKeyDown model code

        Resize s ->
            ( model, Cmd.none )

        Position x y ->
            updatePosition model x y

        DownPosition x y ->
            ( updateDownPosition model x y, Cmd.none )

        UpPosition x y ->
            updateUpPosition model x y

        LoadedSVG html ->
            ( { model | svg = (SP.parse html) |> R.withDefault [] }, Cmd.none )

        Size width height ->
            ( { model | height = height - 70, width = width - 240 }, Cmd.none )

        ChangeColor color ->
            ( { model | currentColor = color }, Cmd.none )

        ChangeStroke value ->
            ( { model | currentStroke = value }, Cmd.none )

        ToggleFunction function ->
            ( { model | functionToggled = function }, Cmd.none )

        Save ->
            ( model, save "Save" )

        Load ->
            ( model, load "Load" )

        SendMessage str ->
            ( model, WebSocket.send backend str )

        NewMessage str ->
            let
                result =
                    Debug.log "Decoded" <| decodeEvent str
            in
                case result of
                    Ok decoded ->
                        case decoded.event of
                            "delete" ->
                                case decoded.payload of
                                    AstList payload ->
                                        ( { model | svg = removeLists payload model.svg }, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            "update" ->
                                case decoded.payload of
                                    AstList payload ->
                                        ( { model | svg = updateAsts model.svg payload }, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            "insert" ->
                                case decoded.payload of
                                    Ast payload ->
                                        ( { model | svg = insertObject model.svg (Just payload) model.currentId, currentId = model.currentId + 1 }, Cmd.none )

                                    _ ->
                                        ( model, Cmd.none )

                            "user-joined" ->
                                ( { model | documentId = decoded.documentId }, newUrl <| M.withDefault "" decoded.documentId )

                            _ ->
                                ( model, Cmd.none )

                    Err error ->
                        ( model, Cmd.none )

        NewSeed seed ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator (initialSeed seed)
            in
                ( { model | uuid = Just newUuid }, WebSocket.send backend (sendConnectMessage model.documentId newUuid) )

        SelectElement id ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
