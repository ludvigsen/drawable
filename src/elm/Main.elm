port module Main exposing (..)

import Html.Attributes as A exposing (style, value)
import Html exposing (Html, text, div)
import Html
import Mouse exposing (..)
import List exposing (..)
import Maybe as M
import Window
import Task
import Model exposing (..)
import Toolbar exposing (toolbar)
import Drawing exposing (drawing)
import Result
import Element exposing (show, toHtml)
import ParseSVG exposing (..)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
initialModel : Model
initialModel =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , currentColor = "#000000"
    , isDown = False
    , currentStroke = "40"
    , svg = []
    , currentObject = Nothing
    --, objects = []
    , functionToggled = NoFunction
    --, loadedSvg = []
    }

defaultObject : SvgAst
defaultObject = Tag "path" [] []
    {-
      shape = Path []
    , color = "#000"
    , stroke = "40"
    -}

init : ( Model , Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> Size size.width size.height) Window.size )



-- UPDATE
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

updatePathValue : Value -> Float -> Float -> Value
updatePathValue value x y =
    case value of
        (D elms) -> (D (elms ++ [(L x y)]))
        _ -> value

updateCurrentObjectHelper : SvgAst -> Float -> Float -> SvgAst
updateCurrentObjectHelper object x y =
    case object of
        Tag "path" attrs objs ->
            Tag "path" (List.map (\(key, value) -> (key, updatePathValue value x y)) attrs) objs
        _ -> object


updateCurrentObject model x y =
    case model.functionToggled of
        Line ->
            (model, Cmd.none)
        NoFunction ->
            ( { model
                | x = x
                , y = y
                , currentObject = Just (updateCurrentObjectHelper (M.withDefault defaultObject model.currentObject) x y)
              }
            , Cmd.none
            )


port save : String -> Cmd msg
port load : String -> Cmd msg
port loadedSvg : (String -> msg) -> Sub msg

updateObjectPoints x y object =
    let (Path points) = object.shape in
    { object | shape = Path (points ++ [(x, y)])}

unwrapSvg result =
    case result of
        (Ok [svg]) ->
            case svg of
                Tag "svg" _ children ->
                    children
                _ ->
                    []
        _ ->
            []

parseAst : List SvgAst -> SvgAst -> List SvgAst
parseAst svg object =
    case svg of
        (x::xs) ->
            case x of
                Tag "g" attr objects ->
                    (Tag "g" attr (object::objects))::xs
                _ -> x::(parseAst svg object)
        [] -> [(Tag "g" [] [object])]

insertCurrentObject : List SvgAst -> M.Maybe SvgAst -> List SvgAst
insertCurrentObject svg object =
    case object of
        (Just obj) ->
            parseAst svg obj
        Nothing -> svg

updateDownPosition : Model -> Float -> Float -> Model
updateDownPosition model x y =
    { model | isDown = True, currentObject = Just (Tag "path" [("stroke-width", Value model.currentStroke), ("stroke", Value model.currentColor), ("fill", Value "none"), ("stroke-linejoin", Value "round"), ("d", D [M x y])] []) }

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Position x y ->
            if model.isDown then
                updateCurrentObject model x y
            else
                ( { model | x = x, y = y }, Cmd.none )

        DownPosition x y ->
            (updateDownPosition model x y, Cmd.none)

        UpPosition x y ->
            ({ model | x = x, y = y, isDown = False, currentObject = Nothing,
               svg = insertCurrentObject model.svg model.currentObject }, Cmd.none)

        LoadedSVG html ->
            ( { model | svg = (unwrapSvg (parse html)) }, Cmd.none)

        Size width height ->
            ( { model | height = height, width = width }, Cmd.none )

        ChangeColor color ->
            ( { model | currentColor = color }, Cmd.none )

        ChangeStroke value ->
            ( { model | currentStroke = value }, Cmd.none )
        ToggleFunction function ->
            ({ model | functionToggled =
                   (if model.functionToggled == function then NoFunction else function) }, Cmd.none )
        Save ->
            (model, save "Save")
        Load ->
            (model, load "Load")
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (\{ x, y } -> Model.Position (toFloat x) (toFloat y))
        , Mouse.downs (\{ x, y } -> DownPosition (toFloat x) (toFloat y))
        , Mouse.ups (\{ x, y } -> UpPosition (toFloat x) (toFloat y))
        , Window.resizes (\{ width, height } -> Size width height)
        , loadedSvg (\html -> LoadedSVG html)
        ]

getValueWithDefault : String -> Value -> String
getValueWithDefault default val =
    case val of
        (Value string) -> string
        _ -> default

getValue : (List ParseSVG.Attribute) -> String -> String
getValue xs v =
    case xs of
        [] -> "10"
        ((key, value)::rest) ->
            if key == v then
                getValueWithDefault "10" value
            else
                getValue rest v

convertPathAttributesToObject : (List ParseSVG.Attribute) -> DrawingObject
convertPathAttributesToObject attrs =
    {
      shape = Path []
    , color = getValue attrs "color"
    , stroke = getValue attrs "stroke-width"
    }

svgAstToObject : SvgAst -> Maybe DrawingObject
svgAstToObject tag =
    case tag of
        Tag "path" attrs objects ->
            Nothing
        _ ->
            Nothing

toDrawingObject : Result String (List SvgAst) -> List (Maybe DrawingObject)
toDrawingObject res =
    case res of
      Result.Ok xs ->
          case xs of
              [x] ->
                  case x of
                      Tag "svg" attrs objects ->
                          case objects of
                              [objs] ->
                                  case objs of
                                    Tag "g" a os ->
                                      map svgAstToObject xs
                                    _ -> []
                              _ -> []
                      _ -> []
              _ -> []
      Result.Err _ -> []

-- VIEW
view : Model -> Html Msg
view model =
    Html.div [ A.class "container" ]
        --[ Html.div [] [(toHtml (show model.loadedSvg))]
        [ (toolbar model)
        , (drawing model)
        ]
