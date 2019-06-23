module App.Decoders exposing (decodeDocs, decodeDocument, decodeTag, decodeTranslations)

import App.Types exposing (..)
import Json.Decode exposing (Decoder, array, dict, field, index, list, map, map4, maybe, oneOf, string)


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
