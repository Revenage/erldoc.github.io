module Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]
