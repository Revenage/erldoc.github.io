module App.Router exposing (Route(..), route, toRoute)

-- import App.Page.Home as Home
-- import App.Page.Settings as Settings
-- import App.Page.NotFound as NotFound

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = Home
    | Settings
    | NotFound
    | Document String


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Home (s "erldoc")
        , map Settings (s "erldoc" </> s "settings")
        , map NotFound (s "erldoc" </> s "404")
        , map Document (s "erldoc" </> s "docs" </> string)
        ]



-- MAIN


toRoute : Url.Url -> Route
toRoute string =
    case string.path of
        "" ->
            NotFound

        _ ->
            Maybe.withDefault NotFound (parse route string)
