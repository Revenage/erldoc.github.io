module App.I18n exposing (get)

import App.Types exposing (..)
import Dict exposing (Dict)


type Language
    = English
    | Russian
    | Ukrainian


get : TranslateStatus -> String -> String
get status key =
    case status of
        TranslateSuccess translate ->
            translate
                |> Dict.get key
                |> Maybe.withDefault key

        TranslateFailure ->
            ""

        TranslateLoading ->
            ""
