module AesonValueType exposing (..)

import Json.Decode


type alias AesonValue =
    { aesonValue : Json.Decode.Value
    }
