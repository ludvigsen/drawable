module LoadSVG exposing ( loadSVG )
import Html
import Native.LoadSVG


loadSVG : String -> Html.Html msg
loadSVG =
    Native.LoadSVG.loadSVG
