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
import Http
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)
import Json.Decode exposing (Decoder, field, string, dict)
import App.Types exposing (..)
import App.Decoders exposing (decodeTranslations)

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.translateStatus of
        Loading ->
            { title = "Loading"
            , body = [ text "Loading" ]
            }

        Success _ ->
            case toRoute model.url of

                Home -> Home.view

                Settings -> Settings.view

                NotFound -> NotFound.view

        Failure ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }
