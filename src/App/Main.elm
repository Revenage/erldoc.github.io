port module Main exposing (Model, Msg(..), PageView, footer, getLangString, init, main, nav, subscriptions, update, view)

import App.Decoders exposing (..)
import App.I18n as I18n exposing (..)
import App.Router exposing (..)
import App.Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode exposing (Decoder, dict, field, string)
import Json.Encode as Encode
import Url
import Url.Parser exposing (Parser, map, oneOf, parse, s, string, top)



-- import App.Page.Home as Home
-- import App.Page.NotFound as NotFound
-- import App.Page.Settings as Settings


port settings : Encode.Value -> Cmd msg


port translations : Encode.Value -> Cmd msg


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


type alias DocumentModel =
    { summary : String
    , description : String
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , translation : RespondStatus
    , home : HomeModel
    , settings : SettingsModel
    , document : DocumentModel
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translation)
    | HandleTagResponse (Result Http.Error Tags)
    | HandleDocResponse (Result Http.Error Doc)
    | ChangeMode
    | ChangeLanguage String
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
    let
        route =
            toRoute url

        initialSetting =
            Maybe.withDefault
                { darkMode = False
                , language = getLangString English
                }
                flags.settings
    in
    ( { key = key
      , url = url
      , route = route
      , translation = Loading
      , settings = initialSetting
      , home =
            { search = ""
            , tags = TagLoading
            }
      , document =
            { summary = ""
            , description = ""
            }
      }
    , loadPageData route initialSetting.language
    )


loadPageData : Route -> String -> Cmd Msg
loadPageData route lang =
    Cmd.batch
        (List.append
            [ getTranslation lang ]
            [ initialRequests route lang ]
        )


initialRequests route lang =
    case route of
        Home ->
            getTags

        Document name ->
            getDoc name lang

        _ ->
            Cmd.none


getTranslation : String -> Cmd Msg
getTranslation lang =
    Http.get
        { url = "/erldoc/translations/" ++ lang ++ ".json"
        , expect = Http.expectJson HandleTranslateResponse decodeTranslations
        }


getDoc : String -> String -> Cmd Msg
getDoc name lang =
    Http.get
        { url = "/erldoc/content/" ++ lang ++ "/" ++ name ++ ".json"
        , expect = Http.expectJson HandleDocResponse decodeDocument
        }


getTags : Cmd Msg
getTags =
    Http.get
        { url = "/erldoc/content/tags.json"
        , expect = Http.expectJson HandleTagResponse decodeTag
        }



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
            let
                route =
                    toRoute url
            in
            ( { model | url = url, route = route }
            , requestOnUrlChanged route model.settings.language
            )

        HandleTranslateResponse result ->
            case result of
                Ok translation ->
                    ( { model | translation = Success translation }, Cmd.none )

                Err _ ->
                    ( { model | translation = Failure }, Cmd.none )

        HandleDocResponse result ->
            case result of
                Ok doc ->
                    ( { model | document = doc }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

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

        ChangeLanguage select ->
            let
                oldSettings =
                    model.settings

                newSettings =
                    { oldSettings | language = select }
            in
            ( { model | settings = newSettings }
            , Cmd.batch [ saveSettings newSettings, getTranslation select ]
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


requestOnUrlChanged route lang =
    case route of
        Document name ->
            getDoc name lang

        Home ->
            getTags

        _ ->
            Cmd.none


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
    let
        headetClass =
            checkColapse "navbar" model
    in
    header []
        [ Html.nav [ class headetClass, id "myNavBar" ]
            [ ul [ class "nav" ]
                [ li []
                    [ a [ href "/" ]
                        [ span [] [ text (I18n.get model.translation "DOCS") ] ]
                    ]
                , li []
                    [ a [ href "/settings" ]
                        [ span [] [ text (I18n.get model.translation "SETTINGS") ] ]
                    ]
                ]
            ]
        ]


innerNav : Model -> String -> Html Msg
innerNav model name =
    let
        headetClass =
            checkColapse "navbar" model
    in
    header []
        [ Html.nav [ class "navbar inner", id "myNavBar" ]
            [ ul [ class "nav" ]
                [ li []
                    [ a [ href "/" ]
                        [ span [] [ text "‹" ]
                        , span [] [ text (I18n.get model.translation "BACK") ]
                        ]
                    ]
                , li [] [ span [] [ text name ] ]
                ]
            ]
        ]


checkColapse : String -> Model -> String
checkColapse baseclass model =
    if model.route == Settings then
        String.join " " [ baseclass, "collapse" ]

    else
        baseclass


footer : Model -> Html Msg
footer model =
    Html.footer [ class "container" ]
        [ Html.nav []
            [ ul []
                [ li []
                    [ a [ href "/about" ]
                        [ text (I18n.get model.translation "ABOUT") ]
                    ]
                , li []
                    [ a [ href "/contact" ]
                        [ text (I18n.get model.translation "CONTACT") ]
                    ]
                ]
            ]
        , small [] [ text ("Copyright © 2019" ++ "  " ++ I18n.get model.translation "TEST") ]
        ]


view : Model -> Browser.Document Msg
view model =
    case model.translation of
        Loading ->
            { title = I18n.get model.translation "LOADING"
            , body = loader
            }

        Success _ ->
            let
                viewPage pageview =
                    let
                        { title, content } =
                            pageview model
                    in
                    { title = title
                    , body = [ nav model, content, footer model ]
                    }
            in
            case model.route of
                Home ->
                    viewPage homeView

                Document name ->
                    documentView model name

                Settings ->
                    viewPage settingsView

                NotFound ->
                    notFoundView model

        Failure ->
            { title = I18n.get model.translation "FAILURE"
            , body = loader
            }


loader =
    [ div [ class "loader" ] [] ]


settingsView : Model -> { title : String, content : Html Msg }
settingsView model =
    let
        { darkMode } =
            model.settings
    in
    { title = I18n.get model.translation "SETTINGS"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ div [ class "row" ]
                [ switcher
                    ChangeMode
                    model.settings.darkMode
                    (I18n.get model.translation "DARK.THEME")
                    (I18n.get model.translation "LIGHT.THEME")
                ]
            , div [ class "row" ]
                [ languageSelect ChangeLanguage [ "en", "ru", "uk" ] model.settings.language
                ]
            ]
    }


switcher : msg -> Bool -> String -> String -> Html msg
switcher onChange value textOn textOff =
    div [ class "toggle-list" ]
        [ input
            [ attribute "checked" ""
            , class "ios-toggle"
            , id "red"
            , name "test"
            , type_ "checkbox"
            , checked <| value
            , onClick <| onChange
            ]
            []
        , label
            [ class "checkbox-label"
            , attribute "data-off" textOff
            , attribute "data-on" textOn
            , for "red"
            ]
            []
        ]


languageSelect : (String -> msg) -> List String -> String -> Html msg
languageSelect onSelect options selectedValue =
    select [ class "select-css", onInput <| onSelect ]
        (List.map (\opt -> option [ value opt, selected (selectedValue == opt) ] [ text opt ]) options)


homeView : Model -> { title : String, content : Html Msg }
homeView model =
    let
        { search, tags } =
            model.home
    in
    { title = I18n.get model.translation "HOME"
    , content =
        main_ [ id "content", class "container home", tabindex -1 ]
            [ input [ placeholder "Search: ", value search, onInput TypeSearch ] []
            , div [ class "row" ]
                [ ul [] (renderList tags)
                ]
            ]
    }


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


documentView : Model -> String -> Browser.Document Msg
documentView model name =
    let
        { summary, description } =
            model.document
    in
    { title = String.join " " [ I18n.get model.translation "DOCS.TITLE", name ]
    , body =
        [ innerNav model name
        , main_ [ id "content", class "container document", tabindex -1 ]
            [ div [ class "row" ]
                [ h1 [] [ text summary ] ]
            , div [ class "row" ] (textHtml description)
            ]
        , footer model
        ]
    }


renderList : HandleTagResponse -> List (Html Msg)
renderList tags =
    case tags of
        TagSuccess tagList ->
            List.map toLi tagList

        TagLoading ->
            loader

        TagFailure ->
            loader


toLi : String -> Html Msg
toLi item =
    li [] [ a [ href ("/docs/" ++ item) ] [ text item ] ]


notFoundView : Model -> { title : String, body : List (Html msg) }
notFoundView model =
    { title = I18n.get model.translation "NOT.FOUND"
    , body =
        [ main_ [ id "content", class "container page404", tabindex -1 ]
            [ div [ class "row" ]
                [ h1 [ class "title" ] [ text (I18n.get model.translation "NOT.FOUND") ]
                ]
            , div [ class "row" ]
                [ div [ class "image404" ] []
                ]
            , div [ class "row" ]
                [ a [ class "back", href "/" ]
                    [ text (I18n.get model.translation "BACK.HOME") ]
                ]
            ]
        ]
    }



-- i18get : Model -> String -> String
-- i18get model key =
--     case model.translation of
--         Success translate ->
--             translate
--                 |> Dict.get key
--                 |> Maybe.withDefault key
--         Failure ->
--             "qwer"
--         Loading ->
--             "qwer"
