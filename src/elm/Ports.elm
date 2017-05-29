port module Ports exposing (..)

port load : String -> Cmd msg
port save : String -> Cmd msg
port loadedSvg : (String -> msg) -> Sub msg
