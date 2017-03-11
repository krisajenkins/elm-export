module PositionEncoder exposing (..)

import Json.Encode
import PositionType exposing (..)


encodePosition : Position -> Json.Encode.Value
encodePosition x =
    case x of
        Beginning ->
            Json.Encode.string "Beginning"

        Middle ->
            Json.Encode.string "Middle"

        End ->
            Json.Encode.string "End"
