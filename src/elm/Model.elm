module Model exposing (..)

import Uuid
import SvgAst exposing (SvgAst)
import Keyboard exposing (KeyCode)


type Function
    = Line
    | Rect
    | Select
    | Draw
    | Circle


type alias Model =
    { x : Float
    , y : Float
    , downX : Float
    , downY : Float
    , width : Int
    , height : Int
    , scale : String
    , currentColor : String
    , isDown : Bool
    , currentStroke : String
    , svg : List SvgAst
    , currentObject : Maybe SvgAst
    , currentId : Int
    , selectedIds : List String
    , selected : List SvgAst
    , functionToggled : Function
    , test : String
    , uuid : Maybe Uuid.Uuid
    , documentId : Maybe String
    , selecting : Bool
    , resizing : Bool
    , currentKey : KeyCode
    , clipboard : List SvgAst
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
    | Resize String
    | KeyDown KeyCode
    | ChangeHeight String
    | ChangeWidth String
    | ChangeScale String
    | ChangeDetail String String
    | NoOp
