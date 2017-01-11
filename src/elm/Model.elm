module Model exposing (..)
import Html exposing (Attribute)
import Svg exposing (Svg)
import Result
import ParseSVG exposing (SvgAst)

type Function
    = Line
    | NoFunction

type Shape =
    Path (List (Float, Float))

type alias DrawingObject =
    { color : String
    , stroke : String
    , shape : Shape
    }

type alias Model =
    { x : Float
    , y : Float
    , width : Int
    , height : Int
    , currentColor : String
    , isDown : Bool
    , currentStroke : String
    , svg : List SvgAst
    , currentObject : Maybe SvgAst
    --, objects : List DrawingObject
    , functionToggled: Function
    --, loadedSvg : List SvgAst
    }


type Msg
    = Position Float Float
    | DownPosition Float Float
    | UpPosition Float Float
    | Size Int Int
    | ChangeColor String
    | ChangeStroke String
    | ToggleFunction Function
    | Save
    | Load
    | LoadedSVG String
    | NoOp
