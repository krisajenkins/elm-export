module ShadowingEncoder exposing (..)

import Json.Encode
import ShadowingType exposing (..)


encodeShadowing : Shadowing -> Json.Encode.Value
encodeShadowing x =
    Json.Encode.object
        [ ( "prop", (\(m0, n0) -> Json.Encode.list identity [ (\(m1, n1) -> Json.Encode.list identity [ Json.Encode.int m1, Json.Encode.int n1 ]) m0, (\(m1, n1) -> Json.Encode.list identity [ Json.Encode.string m1, Json.Encode.string n1 ]) n0 ]) x.prop )
        ]
