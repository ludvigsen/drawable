port module Main exposing (..)

import Html.Attributes as A exposing (style, value)
import Html exposing (Html, text, div)
import Html
import Mouse exposing (..)
import Maybe as M
import Window
import Task
import Model exposing (..)
import Toolbar exposing (toolbar)
import Drawing exposing (drawing)
import ParseSVG exposing (..)
import UpdateModel exposing (..)

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
    , functionToggled = NoFunction
    }

init : ( Model , Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> Size size.width size.height) Window.size )

-- PORTS
port save : String -> Cmd msg
port load : String -> Cmd msg
port loadedSvg : (String -> msg) -> Sub msg

-- UPDATE
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Position x y ->
            (updatePosition model x y, Cmd.none)
        DownPosition x y ->
            (updateDownPosition model x y, Cmd.none)

        UpPosition x y ->
            (updateUpPosition model x y, Cmd.none)

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

-- VIEW
view : Model -> Html Msg
view model =
    Html.div [ A.class "container" ]
        --[ Html.div [] [(toHtml (show model.loadedSvg))]
        [ (toolbar model)
        , (drawing model)
        ]
