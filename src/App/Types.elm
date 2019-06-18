module App.Types exposing (Doc, HandleTagResponse(..), Language(..), RespondStatus(..), Tags, Translation)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Url


type RespondStatus
    = Failure
    | Loading
    | Success Translation


type HandleTagResponse
    = TagFailure
    | TagLoading
    | TagSuccess Tags


type Language
    = English
    | Russian
    | Ukrainian


type alias Translation =
    Dict String String


type alias Translations =
    Dict String Translation


type alias Tags =
    List String


type alias Doc =
    { modulesummary : String }
