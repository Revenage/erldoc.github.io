module Components.Switcher exposing (Model, Msg(..), init, subscriptions, switcher, update, view)

import Browser
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


switcher : Program Model Model Msg
switcher =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { value : Bool
    , textOn : String
    , textOff : String
    }


init : Model -> ( Model, Cmd Msg )
init flags =
    ( flags
    , Cmd.none
    )



-- UPDATE


type Msg
    = ToggleChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleChange ->
            ( { model | value = not model.value }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "toggle-list" ]
        [ input
            [ attribute "checked" ""
            , class "ios-toggle"
            , id "red"
            , name "test"
            , type_ "checkbox"
            , checked <| model.value
            , onClick <| ToggleChange
            ]
            []
        , label
            [ class "checkbox-label"
            , attribute "data-off" model.textOff
            , attribute "data-on" model.textOn
            , for "red"
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
