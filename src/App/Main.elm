module Main exposing (..)
import Dict exposing (Dict)

import App.Page.Home as Home
import App.Page.NotFound as NotFound
import App.Page.Settings as Settings
import App.Router exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)
import Json.Decode exposing (Decoder, field, string, dict)
import App.Types exposing (..)
import App.Decoders exposing (decodeTranslations)
import App.Model exposing (..)

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


getLangString : Language -> String
getLangString lang =
    case lang of
        English ->
            "en"

        Russian ->
            "ru"

        Ukrainian ->
            "uk"



init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , translateStatus = Loading
      , language = English
      , settings = {
          darkMode = False
      }
      , home = {
          search = ""
      }
      , tags = []
      }
    , Http.get
        { url = "/translations/en.json"
        , expect = Http.expectJson HandleTranslateResponse decodeTranslations
        }
    )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        HandleTranslateResponse result ->
            case result of
                Ok translation ->
                    ( { model | translateStatus = Success translation }, Cmd.none )

                Err _ ->
                    ( { model | translateStatus = Failure }, Cmd.none )

        HandleTagResponse result ->
            case result of
                Ok tags ->
                    ( { model | tags = tags }, Cmd.none )

                Err _ ->
                    ( { model | tags = [] }, Cmd.none )

        ChangeMode ->

            let oldSettings = model.settings
                newSettings = { oldSettings | darkMode = not model.settings.darkMode }
            in
                ({ model | settings = newSettings }
            , Cmd.none)


        TypeSearch text ->

            let oldHome = model.home
                newHome = { oldHome | search = text }
            in
                ({ model | home = newHome }
            , Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

mainView: { title : String, content : Html Msg } -> Model -> Browser.Document Msg
mainView pageview model = {
                    title = pageview.title
                    , body = [
                        nav model
                        , pageview.content
                        , footer model
                    ]
                    }

nav : Model -> Html Msg
nav model =
    header [ class "navbar navbar-fixed-top navbar-inverse" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ] []
            , Html.nav [ class "collapse navbar-collapse", id "myNavBar" ]
                [ ul [ class "nav navbar-nav navbar-right" ]
                    [ li []
                        [ a [ href "/" ]
                            [ text "Docs" ]
                        ]
                    , li []
                         [ a [ href "/settings" ]
                            [ text "Settings" ]
                        ]
                    ]
                ]
            ]
        ]

footer : Model -> Html Msg
footer model =
    Html.footer [ class "container" ]
        [ small [] [ text "Copyright © 2019" ]
        , Html.nav []
            [ ul []
                [ li []
                    [ a [ href "/about" ]
                        [ text "About" ]
                    ]
                , li []
                    [ a [ href "/contact" ]
                        [ text "Contact" ]
                    ]
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    case model.translateStatus of
        Loading ->
            { title = "Loading"
            , body = [ text "Loading" ]
            }

        Success _ ->
            case toRoute model.url of

                Home -> mainView (Home.view model) model

                Settings -> mainView (Settings.view model) model

                NotFound -> NotFound.view

        Failure ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }
