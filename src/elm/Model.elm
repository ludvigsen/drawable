module Model exposing (..)

type Function
    = Line
    | NoFunction

type alias Model =
    { x : Float
    , y : Float
    , width : Int
    , height : Int
    , downs : List (List ( Float, Float ))
    , currentColor : String
    , colors : List String
    , isDown : Bool
    , currentStroke : String
    , strokes : List String
    , functionToggled: Function
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
    | NoOp
