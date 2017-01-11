module Drawing exposing (drawing)

import List exposing (..)
import Model exposing (..)
import Svg exposing (svg, g, path)
import Svg.Attributes exposing (transform, d, stroke, fill, strokeLinejoin, strokeWidth)
import Html.Attributes as A exposing (style, value, attribute)
import Html
import String
import ParseSVG
import Maybe


makeCoord : ( Float, Float ) -> String
makeCoord ( x, y ) =
    (toString x) ++ " " ++ (toString y)


makePathSegment : List ( Float, Float ) -> String
makePathSegment coords =
    "M" ++ String.join " L " (map makeCoord coords)


makePath object =
    let (Path points) = object.shape in
    path [ d (makePathSegment points), stroke object.color, fill "none", strokeLinejoin "round", strokeWidth object.stroke ] []

parseDElement : ParseSVG.DElement -> String
parseDElement el =
    case el of
        (ParseSVG.M x y) ->
            "M " ++ (toString x) ++ " " ++ (toString y)
        (ParseSVG.L x y) ->
            "L " ++ (toString x) ++ " " ++ (toString y)

parseAttribute : ParseSVG.Attribute -> Svg.Attribute msg
parseAttribute attr =
    case attr of
        (key, ParseSVG.D elms) ->
            d (String.join " " (map parseDElement elms))
        (string, ParseSVG.Value val) ->
            A.attribute string val

parseAttributes : List ParseSVG.Attribute -> List (Svg.Attribute msg)
parseAttributes attrs =
    map parseAttribute attrs

drawSvgAst : ParseSVG.SvgAst -> Svg.Svg msg
drawSvgAst svgs =
    case svgs of
        (ParseSVG.Tag "g" attrs objs) ->
            g (parseAttributes attrs) (drawSvgAsts objs)
        (ParseSVG.Tag "path" attrs _) ->
            path (parseAttributes attrs) []
        _ -> Svg.desc [] []

drawSvgAsts = map drawSvgAst

drawing : Model -> Html.Html msg
drawing model =
    svg [ A.id "image", style [ ( "width", (toString (model.width * 1)) ++ "px" ), ( "height", (toString (model.height * 1)) ++ "px" ) ] ]
        (drawSvgAsts ((Maybe.withDefault (ParseSVG.Tag "g" [] []) model.currentObject)::model.svg))
{-
drawing model =
    svg [ A.id "image", style [ ( "width", (toString (model.width * 1)) ++ "px" ), ( "height", (toString (model.height * 1)) ++ "px" ) ] ]
        [ g [ transform "translate(0,0)" ]
            (map makePath model.objects)
        ]
-}
