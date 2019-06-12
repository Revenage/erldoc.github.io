module App.Page.Settings exposing (..)

import App.Types exposing (..)
import Browser exposing (..)
import Html exposing (Html, div, h1, img, input, label, main_, text)
import Html.Attributes exposing (alt, checked, class, id, src, tabindex, type_)
import Html.Events exposing (onClick)


type alias Model =
    { darkMode : Bool
    }


type Msg
    = ChangeMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMode ->
            ( { model | darkMode = model.darkMode }
            , Cmd.none
            )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Settings" ]
            , div [ class "row" ]
                [ label [ class "checkbox" ]
                    [ input
                        [ type_ "checkbox"
                        , checked <| model.darkMode
                        , onClick <| ChangeMode
                        ]
                        []
                    , text "dark mode"
                    ]
                ]
            ]
    }
