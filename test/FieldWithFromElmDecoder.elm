module FieldWithFromElmDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import FieldWithFromElmType exposing (..)


decodeFieldWithFromElm : Decoder FieldWithFromElm
decodeFieldWithFromElm =
    succeed FieldWithFromElm
        |> required "fieldWithFromElm" decodeFromElm
