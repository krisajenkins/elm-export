module ShadowingDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import ShadowingType exposing (..)


decodeShadowing : Decoder Shadowing
decodeShadowing =
    succeed Shadowing
        |> required "prop" (map2 Tuple.pair (index 0 (map2 Tuple.pair (index 0 int) (index 1 int))) (index 1 (map2 Tuple.pair (index 0 string) (index 1 string))))
