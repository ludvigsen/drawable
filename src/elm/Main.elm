module Main exposing (..)

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
import FunctionToggles exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


initialFunctionToggles : FunctionToggles
initialFunctionToggles =
    { line = False
    }


initialModel : Model
initialModel =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    , downs = []
    , currentColor = "#000000"
    , colors = []
    , isDown = False
    , currentStroke = "40"
    , strokes = []
    , functionToggles = initialFunctionToggles
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> Size size.width size.height) Window.size )



-- UPDATE


updateLast xs x =
    let
        list =
            take ((length xs) - 1) xs

        rest =
            drop ((length xs) - 1) xs

        last =
            case (head rest) of
                Just l ->
                    l

                Nothing ->
                    []
    in
        list ++ [ (last ++ x ) ]


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


update msg model =
    case msg of
        Position x y ->
            if model.isDown then
                ( { model | x = x, y = y, downs = updateLast model.downs [( x, y )] }, Cmd.none )
            else
                ( { model | x = x, y = y }, Cmd.none )

        DownPosition x y ->
            ( { model
                | x = x
                , y = y
                , colors = model.colors ++ [ model.currentColor ]
                , isDown = True
                , downs = model.downs ++ (dot x y)
                , strokes = model.strokes ++ [ model.currentStroke ]
              }
            , Cmd.none
            )

        UpPosition x y ->
            ( { model | x = x, y = y, isDown = False, downs = updateLast model.downs (M.withDefault [] (head (dot x y))) }, Cmd.none )

        Size width height ->
            ( { model | height = height, width = width }, Cmd.none )

        ChangeColor color ->
            ( { model | currentColor = color }, Cmd.none )

        ChangeStroke value ->
            ( { model | currentStroke = value }, Cmd.none )

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
        ]



-- VIEW

view : Model -> Html Msg
view model =
    Html.div [ A.class "container" ]
        [ (toolbar model)
        , (drawing model)
        ]
