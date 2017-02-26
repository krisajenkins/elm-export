module TimingDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import TimingType exposing (..)


decodeTiming : Decoder Timing
decodeTiming =
    field "tag" string |> andThen ( \x ->
        if x == "Start" then decode Start
        else if x == "Continue" then decode Continue |> required "contents" float
        else if x == "Stop" then decode Stop
        else fail "Constructor not matched" )
