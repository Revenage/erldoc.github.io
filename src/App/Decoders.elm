module App.Decoders exposing (decodeTranslations, decodeDocs)

import Json.Decode exposing (Decoder, dict, string, field)
import App.Types exposing (..)

decodeTranslations : Decoder Translations
decodeTranslations =
    dict string

decodeDocs: Decoder Translations
decodeDocs =
    dict string
