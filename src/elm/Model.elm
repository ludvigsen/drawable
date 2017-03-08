module Model exposing (..)
--import ParseSVG exposing (SvgAst)
import Uuid
import SvgAst exposing (SvgAst)

type Function
    = Line
    | Rect
    | Select
    | Draw
    | Circle

--type Shape =
    --Path (List (Float, Float))

{- type alias DrawingObject =
    { color : String
    , stroke : String
    , shape : Shape
    } -}

type alias Model =
    { x : Float
    , y : Float
    , downX : Float
    , downY : Float
    , width : Int
    , height : Int
    , currentColor : String
    , isDown : Bool
    , currentStroke : String
    , svg : List SvgAst
    , currentObject : Maybe SvgAst
    , currentId : Int
    , selectedId : Int
    , selected : List SvgAst
    --, objects : List DrawingObject
    , functionToggled: Function
    , test : String
    , uuid: Maybe Uuid.Uuid
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
    | NewMessage String
    | SendMessage String
    | NewSeed Int
    | SelectElement String
    | NoOp
