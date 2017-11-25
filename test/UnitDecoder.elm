module UnitDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import UnitType exposing (..)


decodeUnit : Decoder Unit
decodeUnit =
    succeed Unit
