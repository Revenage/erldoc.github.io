module App.Types exposing (..)
import Url
import Dict exposing (Dict)
import Html exposing (Html)

import Browser.Navigation as Nav
import Browser
import Http

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleTranslateResponse (Result Http.Error Translations)
    | HandleTagResponse (Result Http.Error Tags)
    | ChangeMode
    | TypeSearch String


type RespondStatus
    = Failure
    | Loading
    | Success Translations


type Language
    = English
    | Russian
    | Ukrainian

type alias Translations =
    Dict String String

type alias Tags = List String


type alias PageView = { title : String, content : Html Msg }
