module AesonValueEncoder exposing (..)

import Json.Encode
import AesonValueType exposing (..)


encodeAesonValue : AesonValue -> Json.Encode.Value
encodeAesonValue x =
    Json.Encode.object
        [ ( "aesonValue", identity x.aesonValue )
        ]
