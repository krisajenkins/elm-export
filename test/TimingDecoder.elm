module TimingDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import TimingType exposing (..)


decodeTiming : Decoder Timing
decodeTiming =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "Start" ->
                        Json.Decode.succeed Start

                    "Continue" ->
                        Json.Decode.succeed Continue
                            |> required "contents" float

                    "Stop" ->
                        Json.Decode.succeed Stop

                    _ ->
                        fail "Constructor not matched"
            )
