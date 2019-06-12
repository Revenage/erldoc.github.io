module Main exposing (..)

import App.Decoders exposing (decodeTranslations)
import App.Page.Home as Home
import App.Page.NotFound as NotFound
import App.Page.Settings as Settings
import App.Router exposing (..)
import App.Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, dict, field, string)
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , translateStatus : RespondStatus
    , language : Language
    , home : Home.Model
    , settings : Settings.Model
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translations)
    | ChangeMode
    | GotHomeMsg Home.Msg
    | GotSettingsMsg Settings.Msg


type alias PageView = { title : String, content : Html Msg }


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
      , settings =
            { darkMode = False
            }
      , home =
            { search = ""
            , tags = []
            }
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

        ChangeMode ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | darkMode = not model.settings.darkMode }
            in
            ( { model | settings = newSettings }
            , Cmd.none
            )

        ( GotSettingsMsg subMsg) ->
            Settings.update subMsg model.settings
                |> updateWith model.settings GotSettingsMsg model

        -- ( GotHomeMsg subMsg ) ->
        --     Home.update subMsg model.home
        --         |> updateWith Home GotHomeMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


mainView : PageView -> Model -> Browser.Document Msg
mainView pageview model =
    { title = pageview.title
    , body =
        [ nav model
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
                Home ->
                    mainView (Home.view model.home) model

                Settings ->
                    mainView (Settings.view model.settings) model

                NotFound ->
                    NotFound.view

        Failure ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }
