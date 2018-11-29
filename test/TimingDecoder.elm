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
                        succeed Start

                    "Continue" ->
                        succeed Continue
                            |> required "contents" float

                    "Stop" ->
                        succeed Stop

                    _ ->
                        fail "Constructor not matched"
            )
