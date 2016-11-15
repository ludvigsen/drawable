module Drawing exposing (drawing)
import List exposing (..)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes as A exposing (style, value)
import String

makeCoord: (Float, Float) -> String
makeCoord (x, y) = (toString x) ++ " " ++ (toString y)

makePathSegment: List (Float, Float) -> String
makePathSegment coords = "M"  ++ String.join " L " (map makeCoord coords)

makePath p color strokeW = path [ d (makePathSegment p), stroke color, fill "none", strokeLinejoin "round", strokeWidth strokeW ] []

drawing model = svg [ style [ ( "width", (toString (model.width * 1)) ++ "px" ), ( "height", (toString (model.height * 1)) ++ "px" ) ] ]
          [ g [ transform "translate(0,0)" ]
              (map3 makePath model.downs model.colors model.strokes)
          ]
