module UselessDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import UselessType exposing (..)


decodeUseless : Decoder Useless
decodeUseless =
    Json.Decode.succeed Useless
        (succeed ())
