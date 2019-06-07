module App.Model exposing (..)
import App.Types exposing (..)
import Url
import Browser.Navigation as Nav

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , translateStatus : RespondStatus
    , language : Language
    , home: {
        search: String
    }
    , settings: {
        darkMode: Bool
    }
    , tags: List String
    }

