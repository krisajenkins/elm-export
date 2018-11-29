module UselessEncoder exposing (..)

import Json.Encode
import UselessType exposing (..)


encodeUseless : Useless -> Json.Encode.Value
encodeUseless x =
    Json.Encode.list identity []
