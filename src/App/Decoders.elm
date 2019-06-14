module App.Decoders exposing (decodeDocs, decodeTag, decodeTranslations)

import App.Types exposing (..)
import Json.Decode exposing (Decoder, dict, field, list, string)


decodeTranslations : Decoder Translations
decodeTranslations =
    dict string


decodeDocs : Decoder Translations
decodeDocs =
    dict string


decodeTag : Decoder Tags
decodeTag =
    list string
