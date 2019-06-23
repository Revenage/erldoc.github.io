module App.Pages.NotFound exposing (Model, Msg(..), init, notFound, subscriptions, update, view)

import App.Helpers.AssetsUrl exposing (assetsUrl)
import App.I18n as I18n exposing (..)
import App.Types exposing (..)
import Browser
import Html exposing (Html, a, div, h1, input, main_, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { translation : TranslateStatus }


type Msg
    = NoOp


notFound : Program Model Model Msg
notFound =
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
    ( model
    , Cmd.none
    )


view : Model -> { title : String, body : List (Html Msg) }
view model =
    let
        trans =
            I18n.get model.translation
    in
    { title = trans "NOT.FOUND"
    , body =
        [ main_ [ id "content", class "container page404", tabindex -1 ]
            [ div [ class "row" ]
                [ h1 [ class "title" ] [ text (trans "NOT.FOUND") ]
                ]
            , div [ class "row" ]
                [ div [ class "image404" ] []
                ]
            , div [ class "row" ]
                [ a [ class "back", href <| assetsUrl "/" ]
                    [ text (trans "BACK.HOME") ]
                ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
