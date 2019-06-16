port module Main exposing (Model, Msg(..), PageView, footer, getLangString, init, main, nav, subscriptions, update, view)

-- import App.Page.Home as Home
-- import App.Page.NotFound as NotFound
-- import App.Page.Settings as Settings

import App.Decoders exposing (..)
import App.Router exposing (..)
import App.Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, dict, field, string)
import Json.Encode as Encode
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)


port settings : Encode.Value -> Cmd msg


type alias InitialData =
    { settings : Maybe SettingsModel
    }


type alias HomeModel =
    { search : String
    , tags : HandleTagResponse
    }


type alias SettingsModel =
    { darkMode : Bool
    , language : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , translation : RespondStatus
    , home : HomeModel
    , settings : SettingsModel
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translation)
    | HandleTagResponse (Result Http.Error Tags)
    | ChangeMode
    | TypeSearch String


type alias PageView =
    { title : String, content : Html Msg }


main : Program InitialData Model Msg
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


init : InitialData -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , translation = Loading
      , settings =
            Maybe.withDefault
                { darkMode = False
                , language = getLangString English
                }
                flags.settings
      , home =
            { search = ""
            , tags = TagLoading
            }
      }
    , loadPageData
    )


loadPageData : Cmd Msg
loadPageData =
    Cmd.batch
        [ Http.get
            { url = "/translations/en.json"
            , expect = Http.expectJson HandleTranslateResponse decodeTranslations
            }
        , Http.get
            { url = "/content/tags.json"
            , expect = Http.expectJson HandleTagResponse decodeTag
            }
        ]



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
                    ( { model | translation = Success translation }, Cmd.none )

                Err _ ->
                    ( { model | translation = Failure }, Cmd.none )

        HandleTagResponse result ->
            case result of
                Ok tags ->
                    let
                        oldmodel =
                            model.home

                        newmodel =
                            { oldmodel | tags = TagSuccess tags }
                    in
                    ( { model | home = newmodel }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        ChangeMode ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | darkMode = not model.settings.darkMode }
            in
            ( { model | settings = newSettings }
            , saveSettings newSettings
            )

        TypeSearch text ->
            let
                oldmodel =
                    model.home

                newmodel =
                    { oldmodel | search = text }
            in
            ( { model | home = newmodel }
            , Cmd.none
            )


saveSettings : SettingsModel -> Cmd msg
saveSettings model =
    let
        value =
            Encode.object
                [ ( "darkMode", Encode.bool model.darkMode )
                , ( "language", Encode.string model.language )
                ]
    in
    settings value



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW
-- mainView : PageView -> Model -> Browser.Document Msg
-- mainView pageview model =
--     { title = pageview.title
--     , body =
--         [ nav model
--         , pageview.content
--         , footer model
--         ]
--     }


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
    case model.translation of
        Loading ->
            { title = "Loading"
            , body = [ text "Loading" ]
            }

        Success _ ->
            let
                viewPage pageview =
                    let
                        { title, content } =
                            pageview model
                    in
                    { title = title
                    , body =
                        [ div
                            []
                            -- [ classList
                            --     [ ( "app", True )
                            --     , ( "dark", model.settings.darkMode )
                            --     , ( "light", not model.settings.darkMode )
                            --     ]
                            -- ]
                            [ nav model, content, footer model ]
                        ]
                    }
            in
            case toRoute model.url of
                Home ->
                    viewPage homeView

                Settings ->
                    viewPage settingsView

                NotFound ->
                    viewPage notFoundView

        Failure ->
            { title = "Failure"
            , body = [ text "The application failed to initialize. " ]
            }


settingsView : Model -> { title : String, content : Html Msg }
settingsView model =
    let
        { darkMode } =
            model.settings
    in
    { title = "Settings"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Settings" ]
            , div [ class "row" ]
                [ div [ class "toggle-list" ]
                    [ input
                        [ attribute "checked" ""
                        , class "ios-toggle"
                        , id "red"
                        , name "test"
                        , type_ "checkbox"
                        , checked <| model.settings.darkMode
                        , onClick <| ChangeMode
                        ]
                        []
                    , label [ class "checkbox-label", attribute "data-off" "Light theme", attribute "data-on" "Dark theme", for "red" ] []
                    ]
                ]
            ]
    }


homeView : Model -> { title : String, content : Html Msg }
homeView model =
    let
        { search, tags } =
            model.home
    in
    { title = "Home Page"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Home Page" ]
            , input [ placeholder "Search: ", value search, onInput TypeSearch ] []
            , div [ class "row" ]
                [ h3 [] [ text "Modules:" ]
                , ul [] (renderList tags)
                ]
            ]
    }


renderList : HandleTagResponse -> List (Html Msg)
renderList tags =
    case tags of
        TagSuccess tagList ->
            List.map toLi tagList

        TagLoading ->
            [ li [] [ text "Loading" ] ]

        TagFailure ->
            [ li [] [ text "Failure" ] ]


toLi : String -> Html Msg
toLi item =
    li [] [ text item ]


notFoundView : Model -> { title : String, content : Html msg }
notFoundView model =
    { title = "Page Not Found"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            , div [ class "row" ]
                [ a [ href "/" ]
                    [ text "Docs" ]
                ]
            ]
    }
