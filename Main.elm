import Visualization.Path as P exposing (begin, moveTo, lineTo, close, toAttrString)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes as A exposing (style, value)
import Html exposing (Html, text, div)
import Html.Events exposing (onInput, onClick, onWithOptions)
import Html.App as Html
import Mouse exposing (..)
import List exposing (..)
import Window
import Task
import Collage
import Element
import Color
import String
import Json.Decode as Json

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL
type alias Model = {
    x: Float
  , y: Float
  , width: Int
  , height: Int
  , downs: List (List (Float, Float))
  , currentColor: String
  , colors: List String
  , isDown: Bool
  , currentStroke: String
  , strokes: List String
  }

initialModel: Model
initialModel =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , downs = []
    , currentColor = "#000"
    , colors = []
    , isDown = False
    , currentStroke = "40"
    , strokes = []
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Task.perform (\_ -> NoOp) (\size -> Size size.width size.height) Window.size)

-- UPDATE

type Msg
    = Position Float Float
    | DownPosition Float Float
    | UpPosition Float Float
    | Size Int Int
    | ChangeColor String
    | ChangeStroke String
    | NoOp

updateLast xs x =
    let
        list = take ((length xs) - 1) xs
        rest = drop ((length xs) - 1) xs
        last = case (head rest) of
               (Just l) -> l
               Nothing -> []
    in
        list ++ [(last ++ [x])]

update msg model =
    case msg of
        Position x y ->
            if model.isDown then
              ({model | x=x, y = y, downs = updateLast model.downs (x, y)}, Cmd.none)
            else
              ({model | x = x, y = y} , Cmd.none)
        DownPosition x y ->
            ({model |
                  x = x, y = y,
                  colors = model.colors ++ [model.currentColor],
                  isDown = True,
                  downs = model.downs ++ [[(x,y),(x+1,y+1),(x+1,y-1),(x-1,y+1), (x+1,y), (x-1,y)]],
                  strokes = model.strokes ++ [model.currentStroke]
             }, Cmd.none)
        UpPosition x y ->
            ({model | x = x, y = y, isDown = False}, Cmd.none)
        Size width height ->
            ({model | height = height, width = width}, Cmd.none)
        ChangeColor color ->
            ({model | currentColor = color}, Cmd.none)
        ChangeStroke value ->
            ({model | currentStroke = value}, Cmd.none)
        NoOp ->
            (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Mouse.moves (\{x, y} -> Position (toFloat x) (toFloat y))
        , Mouse.downs (\{x, y} -> DownPosition (toFloat x) (toFloat y))
        , Mouse.ups (\{x, y} -> UpPosition (toFloat x) (toFloat y))
        , Window.resizes (\{width, height} -> Size width height)
        ]

makeCoord: (Float, Float) -> String
makeCoord (x, y) = (toString x) ++ " " ++ (toString y)

makePathSegment: List (Float, Float) -> String
makePathSegment coords = "M"  ++ String.join " L " (map makeCoord coords)

makePath p color strokeW = path [ d (makePathSegment p), stroke color, fill "none", strokeLinejoin "round", strokeWidth strokeW ] []

viewDown: List (Float, Float) -> Html a
viewDown down = Html.div [] (map (\(x,y) -> Html.text ((toString x) ++ "," ++ (toString y))) down)

viewDowns: List (List (Float, Float)) -> Html a
viewDowns downs = Html.div [] (map viewDown downs)

getColorBoxStyle color =
    style
        [
          ("background", color)
        , ("width", "2rem")
        , ("height", "2rem")
        , ("display", "inline-block")
        ]

decoder = (always NoOp) 

createColorBox color = Html.div [
                        (getColorBoxStyle color)
                       , (onWithOptions "click" {preventDefault = False, stopPropagation = True} (Json.succeed (ChangeColor color)))
                       ] []

createColorBoxes colors = map createColorBox colors

colors = ["#2a2c2e", "#e20800", "#d0021b", "#5cbd5c", "#5cb85c", "#6dc20f", "#ffffff", "#a8a5a6", "#e34a3e", "#f47f30",
          "#ffc200", "#91c591", "#c0e3c0", "#4d91e2", "#f5f9fd", "#387ecf", "#4a90e2", "#475b72", "#304155", "#1d7ab7",
          "#2e71b2", "#223246", "#a0a9ba", "#596479", "#000000", "#dddddd", "#ececec", "#fcfcfc", "#a4a4a4", "#acacac",
          "#cccccc", "#969696", "#e5e5e5", "#f5f5f5", "#888888", "#e6e5e5", "#e1e1e1", "#ea4ccb", "#f46c30", "#edbc64",
          "#6495ed", "#f9f0e6"]

rangeSlider v =
    Html.input
        [ A.type' "range"
        , A.min "0"
        , A.max "40"
        , value v
        , onInput ChangeStroke
        ] []

-- VIEW
view: Model -> Html Msg
view model =
    Html.div [] [
      Html.div [] [
           Html.div [] (createColorBoxes colors),
           Html.input [value model.currentColor, onInput ChangeColor] [],
           (rangeSlider model.currentStroke)
          ],
      svg [ style [ ( "width", (toString (model.width * 2)) ++ "px" ), ( "height", (toString (model.height * 2)) ++ "px" ) ] ]
          [ g [ transform "translate(0,0)" ]
              (map3 makePath model.downs model.colors model.strokes)
          ]
    ]
