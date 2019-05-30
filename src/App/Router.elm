module App.Router exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

import App.Page.Home as Home
import App.Page.Settings as Settings
import App.Page.NotFound as NotFound

import Url.Parser exposing (Parser, parse, map, oneOf, s, string, top)

type Route
  = Home
  | Settings
  | NotFound

route : Parser (Route -> a) a
route =
  oneOf
    [ map Home      top
    , map Settings  (s "settings")
    , map NotFound  (s "404")
    ]
-- MAIN
toRoute : Url.Url -> Route
toRoute string =
  case string.path of
    "" ->
      NotFound

    _ ->
      Maybe.withDefault NotFound (parse route string)
