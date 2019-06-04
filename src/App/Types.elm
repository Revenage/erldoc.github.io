module App.Types exposing (..)
import Url
import Dict exposing (Dict)

import Browser.Navigation as Nav
import Browser
import Http

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translations)


type RespondStatus
    = Failure
    | Loading
    | Success Translations


type Language
    = English
    | Russian
    | Ukrainian

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , translateStatus : RespondStatus
    , language : Language
    }

type alias Translations =
    Dict String String
