module App.Decoders exposing (decodeDocs, decodeDocument, decodeTag, decodeTranslations)

import App.Types exposing (..)
import Json.Decode exposing (Decoder, dict, field, list, map, string)


decodeTranslations : Decoder Translation
decodeTranslations =
    dict string


decodeDocs : Decoder Translation
decodeDocs =
    dict string


decodeTag : Decoder Tags
decodeTag =
    list string


decodeDocument : Decoder Doc
decodeDocument =
    field "erlref"
        (map Doc
            (field "modulesummary" string)
        )
