import Visualization.Path as P exposing (begin, moveTo, lineTo, close, toAttrString)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes exposing (style, value)
import Html exposing (Html, text, div)
import Html.Events exposing (onInput)
import Html.App as Html
import Mouse exposing (..)
import List exposing (..)
import Window
import Task
import Collage
import Element
import Color
import String

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
            ({model | x = x, y = y, colors = model.colors ++ [model.currentColor], isDown = True, downs = model.downs ++ [[(x,y),(x+1,y+1),(x+1,y-1),(x-1,y+1), (x+1,y), (x-1,y)]]}, Cmd.none)
        UpPosition x y ->
            ({model | x = x, y = y, isDown = False}, Cmd.none)
        Size width height ->
            ({model | height = height, width = width}, Cmd.none)
        ChangeColor color ->
            ({model | currentColor = color}, Cmd.none)
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

makePath p color = path [ d (makePathSegment p), stroke color, fill "none", strokeLinejoin "round", strokeWidth "40" ] []

viewDown: List (Float, Float) -> Html a
viewDown down = Html.div [] (map (\(x,y) -> Html.text ((toString x) ++ "," ++ (toString y))) down)

viewDowns: List (List (Float, Float)) -> Html a
viewDowns downs = Html.div [] (map viewDown downs)

-- VIEW
view: Model -> Html Msg
view model =
    Html.div [] [
      Html.div [] [
           Html.input [value model.currentColor, onInput ChangeColor] []
          ],
      svg [ style [ ( "width", (toString (model.width * 2)) ++ "px" ), ( "height", (toString (model.height * 2)) ++ "px" ) ] ]
          [ g [ transform "translate(0,0)" ]
              (map2 makePath model.downs model.colors)
          ]
    ]
