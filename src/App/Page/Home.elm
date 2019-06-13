module App.Page.Home exposing (Model, Msg(..), init, update, view)

import App.Decoders exposing (decodeTag)
import App.I18n as I18n
import App.Types exposing (..)
import Browser exposing (..)
import Html exposing (Html, div, h1, h3, img, input, main_, text)
import Html.Attributes exposing (alt, class, id, placeholder, src, tabindex, value)
import Html.Events exposing (onInput)
import Http


type alias Model =
    { search : String
    , tags : Tags
    }


type Msg
    = HandleTagResponse (Result Http.Error Tags)
    | TypeSearch String


init : Model -> ( Model, Cmd Msg )
init model =
    ( { search = model.search
      , tags = {}
      }
    , Http.get
        { url = "/content/tags.json"
        , expect = Http.expectJson HandleTagResponse decodeTag
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleTagResponse result ->
            case result of
                Ok tags ->
                    ( { model | tags = tags }, Cmd.none )

                Err _ ->
                    ( { model | tags = {} }, Cmd.none )

        TypeSearch text ->
            ( { model | search = text }
            , Cmd.none
            )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home Page"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Home Page" ]
            , div [ class "row" ]
                [ h3 [] [ text "AAA" ]
                , h3 [] [ text "TEST" ]
                ]
            , input [ placeholder "Search: ", value model.search, onInput TypeSearch ] []
            ]
    }
