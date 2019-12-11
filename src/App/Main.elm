port module Main exposing (DocumentModel, HomeModel, InitialData, Model, Msg(..), PageView, SettingsModel, calculateProgress, checkColapse, doOffline, documentView, findTags, footer, getDoc, getLangString, getTags, getTranslation, homeView, init, initialRequests, innerNav, languageSelect, loadPageData, loader, main, nav, renderList, rendetTagsList, requestOnUrlChanged, saveSettings, settings, settingsView, subscriptions, tagStr, textHtml, toLi, toTaggedLi, translations, uniq, update, view)

import Decoders exposing (..)
import I18n exposing (..)
import Router exposing (..)
import Types exposing (..)

import Components.Switcher as Switcher

import Helpers.AssetsUrl exposing (assetsUrl)

import Pages.NotFound as NotFound

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode exposing (Decoder, dict, field, string)
import Json.Encode as Encode
import Regex exposing (..)
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
    , tags : TagStatus
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
    , translation : TranslateStatus
    , home : HomeModel
    , settings : SettingsModel
    , document : DocStatus
    , loadedDocumentsList : List String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translation)
    | HandleTagStatus (Result Http.Error Tags)
    | HandleDocResponse (Result Http.Error Doc)
    | ChangeMode Switcher.Msg
    | ChangeLanguage String
    | TypeSearch String
    | Back
    | LoadAllDocuments
    | MsgNotFound NotFound.Msg


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
      , translation = TranslateLoading
      , settings = initialSetting
      , home =
            { search = ""
            , tags = TagLoading
            }
      , document = DocLoading
      , loadedDocumentsList = []
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
        { url = assetsUrl "/translations/" ++ lang ++ ".json"
        , expect = Http.expectJson HandleTranslateResponse decodeTranslations
        }


getDoc : String -> String -> Cmd Msg
getDoc name lang =
    Http.get
        { url = assetsUrl "/content/" ++ lang ++ "/" ++ name ++ ".json"
        , expect = Http.expectJson HandleDocResponse decodeDocument
        }


getTags : Cmd Msg
getTags =
    Http.get
        { url = assetsUrl "/content/tags.json"
        , expect = Http.expectJson HandleTagStatus decodeTag
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgNotFound _ ->
            ( model
            , Cmd.none
            )

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
                    ( { model | translation = TranslateSuccess translation }, Cmd.none )

                Err _ ->
                    ( { model | translation = TranslateFailure }, Cmd.none )

        HandleDocResponse result ->
            case result of
                Ok doc ->
                    ( { model
                        | document = DocSuccess doc
                        , loadedDocumentsList = uniq <| doc.name :: model.loadedDocumentsList
                      }
                    , Cmd.none
                    )

                Err err ->
                    let error = Debug.log "error" err in
                    ( { model | document = DocFailure }, Cmd.none )

        HandleTagStatus result ->
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

        ChangeMode Switcher.ToggleChange ->
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
                    { oldmodel | search = String.toLower text }
            in
            ( { model | home = newmodel }
            , Cmd.none
            )

        Back ->
            ( model
            , Nav.back model.key 1
            )

        LoadAllDocuments ->
            case model.home.tags of
                TagSuccess tags ->
                    let
                        names =
                            List.map (\item -> Maybe.withDefault "" (List.head item)) tags
                    in
                    ( model
                    , Cmd.batch (List.map (\n -> getDoc n model.settings.language) names)
                    )

                _ ->
                    ( model, Cmd.none )


calculateProgress : TagStatus -> List String -> String
calculateProgress tagStatus loadedList =
    case tagStatus of
        TagSuccess tags ->
            let
                full =
                    tags |> List.length |> toFloat

                loaded =
                    loadedList |> List.length |> toFloat

                precent =
                    round ((loaded / full) * 100)
            in
            String.fromInt precent ++ "%"

        _ ->
            "0%"


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


nav : Model -> Html Msg
nav model =
    let
        headetClass =
            checkColapse "navbar" model

        trans =
            I18n.get model.translation
    in
    header []
        [ Html.nav [ class headetClass, id "myNavBar" ]
            [ ul [ class "nav" ]
                [ li []
                    [ a [ href <| assetsUrl "/" ]
                        [ span [] [ text (trans "DOCS") ] ]
                    ]
                , li []
                    [ a [ href <| assetsUrl "/settings" ]
                        [ span [] [ text (trans "SETTINGS") ] ]
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
                    [ span
                        [ onClick Back
                        ]
                        [ span [] [ text "‹" ]
                        , span [] [ text (I18n.get model.translation "BACK") ]
                        ]
                    ]
                , li [] [ span [] [ text name ] ]
                ]
            ]
        ]



-- import Browser.Navigation as Nav


checkColapse : String -> Model -> String
checkColapse baseclass model =
    if model.route == Settings then
        String.join " " [ baseclass, "collapse" ]

    else
        baseclass


footer : Model -> Html Msg
footer model =
    let
        trans =
            I18n.get model.translation
    in
    Html.footer [ class "container" ]
        [ Html.nav []
            [ ul []
                [ li []
                    [ a [ href <| assetsUrl "/about" ]
                        [ text (trans "ABOUT") ]
                    ]
                , li []
                    [ a [ href <| assetsUrl "/contact" ]
                        [ text (trans "CONTACT") ]
                    ]
                ]
            ]
        , small [] [ text "Copyright © 2019" ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        trans =
            I18n.get model.translation
    in
    case model.translation of
        TranslateLoading ->
            { title = trans "LOADING"
            , body = [ loader ]
            }

        TranslateSuccess _ ->
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
                    let
                        { title, body } =
                            NotFound.view (NotFound.Model model.translation)
                    in
                    { title = title
                    , body = List.map (Html.map MsgNotFound) body
                    }

        TranslateFailure ->
            { title = trans "FAILURE"
            , body = [ loader ]
            }


loader =
    div [ class "loader" ] []


settingsView : Model -> { title : String, content : Html Msg }
settingsView model =
    let
        { darkMode } =
            model.settings

        trans =
            I18n.get model.translation
    in
    { title = trans "SETTINGS"
    , content =
        main_ [ id "content", class "container settings", tabindex -1 ]
            [ div [ class "row" ]
                [ Html.map ChangeMode
                    (Switcher.view
                        { value = model.settings.darkMode
                        , textOn = trans "DARK.THEME"
                        , textOff = trans "LIGHT.THEME"
                        }
                    )
                ]
            , div [ class "row" ]
                [ languageSelect ChangeLanguage [ "en", "ru", "uk" ] model.settings.language
                ]
            , div [ class "row" ]
                [ doOffline model LoadAllDocuments
                ]
            ]
    }


doOffline : Model -> msg -> Html msg
doOffline model click =
    let
        progress =
            calculateProgress model.home.tags model.loadedDocumentsList
    in
    div [ class "offline-btn", onClick click ]
        [ span [] [ text <| String.join ": " [ I18n.get model.translation "OFFLINE.DOCS", progress ] ]
        , div
            [ class "loaded"
            , style "width" progress
            ]
            []
        ]



-- switcher : msg -> Bool -> String -> String -> Html msg
-- switcher onChange value textOn textOff =
--     div [ class "toggle-list" ]
--         [ input
--             [ attribute "checked" ""
--             , class "ios-toggle"
--             , id "red"
--             , name "test"
--             , type_ "checkbox"
--             , checked <| value
--             , onClick <| onChange
--             ]
--             []
--         , label
--             [ class "checkbox-label"
--             , attribute "data-off" textOff
--             , attribute "data-on" textOn
--             , for "red"
--             ]
--             []
--         ]


languageSelect : (String -> msg) -> List String -> String -> Html msg
languageSelect onSelect options selectedValue =
    select [ class "select-css", onInput <| onSelect ]
        (List.map (\opt -> option [ value opt, selected (selectedValue == opt) ] [ text opt ]) options)


homeView : Model -> { title : String, content : Html Msg }
homeView model =
    let
        { search, tags } =
            model.home

        trans =
            I18n.get model.translation
    in
    { title = trans "HOME"
    , content =
        main_ [ id "content", class "container home", tabindex -1 ]
            [ div [ class "input-container" ]
                [ label [ class "icon-search", for "search" ] [ text (trans "SEARCH") ]
                , input
                    [ class "input-field"
                    , name "search"
                    , id "search"
                    , placeholder "Type for search"
                    , type_ "text"
                    , value search
                    , autofocus False
                    , onInput TypeSearch
                    ]
                    []
                ]
            , div [ class "row" ]
                [ ul [] (renderList tags search)
                ]
            ]
    }


textHtml : String -> List (Html.Html msg)
textHtml t =
   
    [div [ class "testtext" ] []]
    -- let adsf = Debug.log "t" t in
    -- case Html.Parser.run t of
    --     Ok nodes ->
    --         Html.Parser.Util.toVirtualDom nodes

    --     Err err ->
    --         []


documentView : Model -> String -> Browser.Document Msg
documentView model name =
    let
        trans =
            I18n.get model.translation
    in
    case model.document of
        DocLoading ->
            { title = trans "LOADING"
            , body = [ innerNav model name, loader, footer model ]
            }

        DocSuccess document ->
            let
                { summary, description, funcs } =
                    document
            in
            { title = String.join " " [ trans "DOCS.TITLE", name ]
            , body =
                [ innerNav model name
                , main_ [ id "content", class "container document", tabindex -1 ]
                    [ div [ class "row summary" ]
                        [ h1 [] [ text summary ] ]
                    , div [ class "row description" ]
                        (textHtml description)
                    , div [ class "row funcs" ] (textHtml funcs)
                    ]
                , footer model
                ]
            }

        DocFailure ->
            { title = trans "FAILURE"
            , body = [ innerNav model name, div [] [text (trans "FAILURE")], footer model ]
            }


renderList : TagStatus -> String -> List (Html Msg)
renderList tags search =
    case tags of
        TagSuccess tagList ->
            case search of
                "" ->
                    List.map (toLi "") tagList

                s ->
                    List.map (toTaggedLi s) tagList

        TagLoading ->
            [ loader ]

        TagFailure ->
            [ loader ]


tagStr : String -> Regex.Regex
tagStr str =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = True, multiline = True }
            ("(\\w+)?"
                ++ str
                ++ "(\\w+)?"
            )


findTags : String -> String -> List String
findTags search str =
    let
        matches =
            Regex.find (tagStr search) str
    in
    List.map (\i -> i.match) matches


uniq : List String -> List String
uniq list =
    List.foldr
        (\curr result ->
            if List.member (String.toLower curr) (List.map String.toLower result) then
                result

            else
                curr :: result
        )
        []
        list


toLi : String -> List String -> Html Msg
toLi search item =
    let
        modulename =
            Maybe.withDefault "" (List.head item)
    in
    li [] [ a [ href <| assetsUrl ("/docs/" ++ modulename) ] [ text modulename ] ]


toTaggedLi : String -> List String -> Html Msg
toTaggedLi search item =
    let
        modulename =
            Maybe.withDefault "" (List.head item)
    in
    let
        tagsList =
            if List.length item > 1 then
                item
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
                    |> findTags search
                    |> List.filter (\lf -> String.toLower lf /= modulename)
                    |> uniq

            else
                []
    in
    if List.length tagsList > 0 then
        li []
            [ a [ href <| assetsUrl ("/docs/" ++ modulename) ] [ text modulename ]
            , ul [ class "tag-list" ] (rendetTagsList tagsList)
            ]

    else
        li [] []


rendetTagsList : List String -> List (Html msg)
rendetTagsList list =
    List.map (\item -> li [ class "tag" ] [ text item ]) list



-- notFoundView : Model -> { title : String, body : List (Html msg) }
-- notFoundView model =
--     let
--         trans =
--             I18n.get model.translation
--     in
--     { title = trans "NOT.FOUND"
--     , body =
--         [ main_ [ id "content", class "container page404", tabindex -1 ]
--             [ div [ class "row" ]
--                 [ h1 [ class "title" ] [ text (trans "NOT.FOUND") ]
--                 ]
--             , div [ class "row" ]
--                 [ div [ class "image404" ] []
--                 ]
--             , div [ class "row" ]
--                 [ a [ class "back", href <| assetsUrl "/" ]
--                     [ text (trans "BACK.HOME") ]
--                 ]
--             ]
--         ]
--     }
