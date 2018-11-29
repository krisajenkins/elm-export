module UnitEncoder exposing (..)

import Json.Encode
import UnitType exposing (..)


encodeUnit : Unit -> Json.Encode.Value
encodeUnit x =
    Json.Encode.list identity []
