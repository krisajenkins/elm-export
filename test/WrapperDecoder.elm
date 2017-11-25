module WrapperDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import WrapperType exposing (..)


decodeWrapper : Decoder Wrapper
decodeWrapper =
    int
        |> map Wrapper
