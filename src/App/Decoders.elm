module App.Decoders exposing (..)

import Json.Decode exposing (Decoder, dict, string, field, list)
import App.Types exposing (..)

decodeTranslations : Decoder Translations
decodeTranslations =
    dict string

decodeDocs: Decoder Translations
decodeDocs =
    dict string

decodeTag : Decoder Tags
decodeTag =
    list string
