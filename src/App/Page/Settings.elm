module App.Page.Settings exposing (..)

import Html exposing (Html, div, h1, img, main_, text, label, input)
import Html.Attributes exposing (alt, class, id, src, tabindex, type_, checked)
import Html.Events exposing (onClick)
import Browser exposing (..)
import App.Model exposing (..)
import App.Types exposing (..)


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Settings" ]
            , div [ class "row" ]
                [label [ class "checkbox" ]
                [ input
                    [ type_ "checkbox"
                    , checked <| model.settings.darkMode
                    , onClick <| ChangeMode
                    ]
                    []
                , text  "dark mode"
                ] ]
            ]
    }
