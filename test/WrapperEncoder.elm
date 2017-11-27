module WrapperEncoder exposing (..)

import Json.Encode
import WrapperType exposing (..)


encodeWrapper : Wrapper -> Json.Encode.Value
encodeWrapper x =
    case x of
        Wrapper y0 ->
            Json.Encode.int y0
