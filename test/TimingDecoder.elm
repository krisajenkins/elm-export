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
                        decode Start

                    "Continue" ->
                        decode Continue
                            |> required "contents" float

                    "Stop" ->
                        decode Stop

                    _ ->
                        fail "Constructor not matched"
            )
