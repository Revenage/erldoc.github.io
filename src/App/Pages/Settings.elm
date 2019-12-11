port module Pages.Settings exposing (Model, Msg(..), calculateProgress, doOffline, getDoc, getTranslation, init, languageSelect, saveSettings, settings, settingsPage, subscriptions, update, view)

import Components.Switcher as Switcher
import Decoders exposing (..)
import Helpers.AssetsUrl exposing (assetsUrl)
import Helpers.Uniq exposing (uniq)
import I18n as I18n exposing (..)
import Types exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode


port settings : Encode.Value -> Cmd msg


type alias Model =
    { darkMode : Bool
    , language : String
    , translation : TranslateStatus
    , loadedDocumentsList : List String
    , tags : TagStatus
    }


type Msg
    = ChangeMode Switcher.Msg
    | ChangeLanguage String
    | LoadAllDocuments
    | HandleTranslateResponse (Result Http.Error Translation)
    | HandleTagStatus (Result Http.Error Tags)
    | HandleDocResponse (Result Http.Error Doc)


settingsPage : Program Model Model Msg
settingsPage =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Model -> ( Model, Cmd Msg )
init flags =
    ( flags
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMode Switcher.ToggleChange ->
            let
                newSettings =
                    { model | darkMode = not model.darkMode }
            in
            ( newSettings
            , saveSettings newSettings
            )

        ChangeLanguage select ->
            let
                newSettings =
                    { model | language = select }
            in
            ( newSettings
            , Cmd.batch [ saveSettings newSettings, getTranslation select ]
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
                        | loadedDocumentsList = uniq <| doc.name :: model.loadedDocumentsList
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        HandleTagStatus result ->
            case result of
                Ok tags ->
                    ( { model | tags = TagSuccess tags }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        LoadAllDocuments ->
            case model.tags of
                TagSuccess tags ->
                    let
                        names =
                            List.map (\item -> Maybe.withDefault "" (List.head item)) tags
                    in
                    ( model
                    , Cmd.batch (List.map (\n -> getDoc n model.language) names)
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        { darkMode, language, translation } =
            model

        trans =
            I18n.get translation
    in
    { title = trans "SETTINGS"
    , body =
        [ main_ [ id "content", class "container settings", tabindex -1 ]
            [ div [ class "row" ]
                [ Html.map ChangeMode
                    (Switcher.view
                        { value = darkMode
                        , textOn = trans "DARK.THEME"
                        , textOff = trans "LIGHT.THEME"
                        }
                    )
                ]
            , div [ class "row" ]
                [ languageSelect ChangeLanguage [ "en", "ru", "uk" ] language
                ]
            , div [ class "row" ]
                [ doOffline model LoadAllDocuments
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


saveSettings : Model -> Cmd msg
saveSettings model =
    let
        value =
            Encode.object
                [ ( "darkMode", Encode.bool model.darkMode )
                , ( "language", Encode.string model.language )
                ]
    in
    settings value


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


languageSelect : (String -> msg) -> List String -> String -> Html msg
languageSelect onSelect options selectedValue =
    select [ class "select-css", onInput <| onSelect ]
        (List.map (\opt -> option [ value opt, selected (selectedValue == opt) ] [ text opt ]) options)


doOffline : Model -> msg -> Html msg
doOffline model click =
    let
        { tags, loadedDocumentsList, translation } =
            model

        progress =
            calculateProgress tags loadedDocumentsList
    in
    div [ class "offline-btn", onClick click ]
        [ span [] [ text <| String.join ": " [ I18n.get translation "OFFLINE.DOCS", progress ] ]
        , div
            [ class "loaded"
            , style "width" progress
            ]
            []
        ]
