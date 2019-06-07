module App.Page.Home exposing (..)

import Html exposing (Html, div, h1,h3, img, main_, text, input)
import Html.Attributes exposing (alt, class, id, src, tabindex, placeholder, value)
import Html.Events exposing (onInput)
import App.I18n as I18n
import Http
import Browser exposing (..)
import App.Decoders exposing (decodeTag)
import App.Types exposing (..)
import App.Model exposing (..)

init : Model -> ( Model, Cmd Msg )
init model =
    ( model
    , Http.get
        { url = "/content/tags.json"
        , expect = Http.expectJson HandleTagResponse decodeTag
        }
    )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home Page"
    , content =
      main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Home Page" ]
            , div [ class "row" ] [ h3 [] [text "AAA"]
            , h3 [] [text "TEST"] ]
            , input [ placeholder "Search: ", value model.home.search, onInput TypeSearch ] []
            ]
    }


