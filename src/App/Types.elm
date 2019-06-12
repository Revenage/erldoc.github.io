module App.Types exposing (Language(..), RespondStatus(..), Tags, Translations)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Url


type RespondStatus
    = Failure
    | Loading
    | Success Translations


type HandleTagResponse
    = TagFailure
    | TagLoading
    | TagSuccess


type Language
    = English
    | Russian
    | Ukrainian


type alias Translations =
    Dict String String


type alias Tags =
    List String
