module App.Page.NotFound exposing (view)

import App.Types exposing (..)
import Browser exposing (..)
import Html exposing (Html, a, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, href, id, src, tabindex)



-- VIEW


view : { title : String, body : List (Html msg) }
view =
    { title = "Page Not Found"
    , body =
        [ main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ a [ href "/" ]
                    [ text "Docs" ]
                ]
            ]
        ]
    }
