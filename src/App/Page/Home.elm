module App.Page.Home exposing (view)

import Html exposing (Html, div, h1,h3, img, main_, text)
import Html.Attributes exposing (alt, class, id, src, tabindex)
import App.I18n as I18n
import App.Types exposing (..)
import Http
import Browser exposing (..)
import App.Decoders exposing (decodeDocs)

view : Browser.Document Msg
view =
    { title = "Home Page"
    , body = [
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Home Page" ]
            , div [ class "row" ] []
                -- [ h3 [] [text (I18n.get translations "commits-refresh")] ]
            ]]
    }


