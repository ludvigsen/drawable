module Main exposing (..)

import Ports exposing (..)

import Navigation
import Html.Attributes as A exposing (style, value, class)
import Html.Events exposing (onInput, onClick, onWithOptions, on)
import Html exposing (Html, text, div)
import Html
import Mouse exposing (..)
import Maybe as M
import Result as R
import Window
import Task
import Model exposing (..)
import Views.Toolbar exposing (toolbar)
import SvgAst as S
import SvgAst.Parser as SP
import SvgAst.Render exposing (render)
import SvgAst.Decode as SD
import SvgAst.Encode as SE
import Update exposing (..)
import WebSocket
import Random
import Element exposing (show, toHtml)
import Svg
import Svg.Attributes as SA
import Views.Drawing exposing (drawing)
import List as L
import Dict as D
import Keyboard as K


main =
    Navigation.program
        (\_ -> NoOp)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


initialModel : Navigation.Location -> Model
initialModel location =
    { x = 0
    , y = 0
    , downX = 0
    , downY = 0
    , width = 0
    , height = 0
    , scale = 1.0
    , currentColor = "#000000"
    , isDown = False
    , currentStroke = "10"
    , svg = []
    , currentObject = Nothing
    , functionToggled = Select
    , test = "test"
    , uuid = Nothing
    , documentId = Just location.pathname
    , currentId = 0
    , selectedIds = []
    , selected = []
    , selecting = False
    , resizing = False
    , currentKey = -1
    , clipboard = []
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initialModel location
    , Cmd.batch
        [ Task.perform (\size -> Size size.width size.height) Window.size
        , Random.generate NewSeed (Random.int Random.minInt Random.maxInt)
        ]
    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (\{ x, y } -> Model.Position (toFloat x) (toFloat y))
        , Mouse.downs (\{ x, y } -> DownPosition (toFloat x) (toFloat y))
        , Mouse.ups (\{ x, y } -> UpPosition (toFloat x) (toFloat y))
        , K.downs (\keyCode -> KeyDown keyCode)
        , loadedSvg (\html -> LoadedSVG html)
        , WebSocket.listen "ws://localhost:9162/" NewMessage
        ]


getDrawObjects : Model -> List S.SvgAst
getDrawObjects model =
    case model.currentObject of
        Just obj ->
            obj :: model.svg

        _ ->
            model.svg



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ A.class "container"]
        [(toolbar model)
        , drawing model
        ]
