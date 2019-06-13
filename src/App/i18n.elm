module App.I18n exposing (get)

import App.Types exposing (..)
import Dict exposing (Dict)


type Language
    = English
    | Russian
    | Ukrainian


get : Translations -> String -> String
get dict key =
    dict
        |> Dict.get key
        |> Maybe.withDefault key
