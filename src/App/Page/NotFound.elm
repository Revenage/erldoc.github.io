module App.Page.NotFound exposing (view)

import Html exposing (Html, div, h1, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import Browser exposing (..)
import App.Types exposing (..)
-- VIEW


view : Browser.Document Msg
view =
    { title = "Page Not Found"
    , body = [
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ ]
            ]]
    }
