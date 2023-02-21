module AesonValueDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import AesonValueType exposing (..)


decodeAesonValue : Decoder AesonValue
decodeAesonValue =
    succeed AesonValue
        |> required "aesonValue" value
