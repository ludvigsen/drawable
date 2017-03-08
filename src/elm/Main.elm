port module Main exposing (..)

import Html.Attributes as A exposing (style, value)
import Html.Events exposing (onInput, onClick, onWithOptions, on)
import Html exposing (Html, text, div)
import Html
import Mouse exposing (..)
import Maybe as M
import Result as R
import Window
import Task
import Model exposing (..)
import Toolbar exposing (toolbar)
import SvgAst as S
import SvgAst.Parser as SP
import SvgAst.Render exposing (render)
import SvgAst.Decode as SD
import SvgAst.Encode as SE
import UpdateModel exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import WebSocket
import Uuid
import Random
import Random.Pcg exposing (step, initialSeed)
import Element exposing (show, toHtml)
import Svg
import Svg.Attributes as SA
import Drawing exposing (drawing)
import List as L
import Dict as D

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
    , downX = 0
    , downY = 0
    , width = 0
    , height = 0
    , currentColor = "#000000"
    , isDown = False
    , currentStroke = "10"
    , svg = []
    , currentObject = Nothing
    , functionToggled = Select
    , test = "test"
    , uuid = Nothing
    , currentId = 0
    , selectedId = -1
    , selected = []
    }

init : ( Model , Cmd Msg )
init =
    (
      initialModel
    , Cmd.batch [
           Task.perform (\size -> Size size.width size.height) Window.size
         , Random.generate NewSeed (Random.int Random.minInt Random.maxInt)]
    )

-- PORTS
port save : String -> Cmd msg
port load : String -> Cmd msg
port loadedSvg : (String -> msg) -> Sub msg


sendInsertMessage : Model -> String
sendInsertMessage model =
    JE.encode 0 <|
    JE.object [
         ("event", JE.string "insert")
       , ("payload", (M.withDefault JE.null (M.map SE.encode model.currentObject)))
        ]

sendConnectMessage : Uuid.Uuid -> String
sendConnectMessage uuid =
    JE.encode 0 <|
    JE.object [
         ("event", JE.string "connect")
       , ("payload", JE.string <| Uuid.toString uuid)
        ]

type alias Event = {
        event: String,
        payload: Maybe S.SvgAst
    }

defaultEvent: Event
defaultEvent = {
    event = "",
    payload = Nothing
  }

decodeEvent : String -> Result String Event
decodeEvent str =
    JD.decodeString
        (JD.map2 Event
             (JD.field "event" JD.string)
             (JD.field "payload" (JD.map Just SD.decode))) str

-- UPDATE
update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Position x y ->
            (updatePosition model x y, Cmd.none)
        DownPosition x y ->
            (updateDownPosition model x y, Cmd.none)

        UpPosition x y ->
            (updateUpPosition model x y, WebSocket.send "ws://localhost:9160/" (sendInsertMessage model))

        LoadedSVG html ->
            ( { model | svg = (SP.parse html) |> R.withDefault [] }, Cmd.none)

        Size width height ->
            ( { model | height = height, width = width }, Cmd.none )

        ChangeColor color ->
            ( { model | currentColor = color }, Cmd.none )

        ChangeStroke value ->
            ( { model | currentStroke = value }, Cmd.none )
        ToggleFunction function ->
            ({ model | functionToggled = function }, Cmd.none )
        Save ->
            (model, save "Save")
        Load ->
            (model, load "Load")
        SendMessage str ->
            (model, WebSocket.send "ws://localhost:9160/" str)
        NewMessage str ->
            let result = decodeEvent str in
            case result of
                Ok decoded ->
                    case decoded.event of
                      "insert" ->
                        (updateUpPosition {model | currentObject = decoded.payload} 0 0, Cmd.none)
                      _ ->
                        (model, Cmd.none)
                Err error ->
                    (model, Cmd.none)
        NewSeed seed ->
            let (newUuid, newSeed) = step Uuid.uuidGenerator (initialSeed seed) in
            ({model | uuid = Just newUuid}, WebSocket.send "ws://localhost:9160/" (sendConnectMessage newUuid))
        SelectElement id ->
            (updateSelected model id, Cmd.none)
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
        , WebSocket.listen "ws://localhost:9160/" NewMessage
        ]

getDrawObjects : Model -> List S.SvgAst
getDrawObjects model =
    case model.currentObject of
        Just obj -> obj::model.svg
        _ -> model.svg

svgStyle : Model -> String
svgStyle model =
    "width:"
    ++ (toString model.width) ++ "px;height:"
        ++ (toString model.height) ++ "px"


-- VIEW
view : Model -> Html Msg
view model =
    Html.div [ A.class "container" ]
        [
         -- Html.div [] [(Html.button [onClick (SendMessage <| M.withDefault "" <| M.map ((++) "Hi! I am ") <| M.map Uuid.toString model.uuid)] [Html.text "msg"]), (Html.button [onClick (SendMessage "12321")] [Html.text "msg2"])]
        --, toHtml (show (model))
        --, Html.div [] [Html.text model.test]
         (toolbar model)
        , drawing model
        --, Html.div [] (drawSelected model)
        --, Svg.svg [(SA.style (svgStyle model))] (List.map render (getDrawObjects model))
        ]
