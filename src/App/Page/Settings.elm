module App.Page.Settings exposing (view)

import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)


-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Settings"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Settings" ]
            , div [ class "row" ]
                [ ]
            ]
    }
