module App.Page.Home exposing (view)

import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)


-- VIEW


view : { title : String, content : Html msg }
view =
    { title = "Home Page"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Home Page" ]
            , div [ class "row" ]
                [ ]
            ]
    }
