module Decoders exposing (decodeDocs, decodeDocument, decodeTag, decodeTranslations)

<<<<<<< HEAD
import Types exposing (..)
import Json.Decode exposing (Decoder, array, dict, field, index, list, map, map2, maybe, oneOf, string)
=======
import App.Types exposing (..)
import Json.Decode exposing (Decoder, array, dict, field, index, list, map, map4, maybe, oneOf, string)
>>>>>>> f02441e54c8a83fa6f304c7ae5cca4a7b52824bf


decodeTranslations : Decoder Translation
decodeTranslations =
    dict string


decodeDocs : Decoder Translation
decodeDocs =
    dict string


decodeTag : Decoder Tags
decodeTag =
    list (list string)


decodeDocument : Decoder Doc
decodeDocument =
    map4 Doc
        (field "name" string)
        (field "summary" string)
        (field "description" string)
        (field "funcs" string)
