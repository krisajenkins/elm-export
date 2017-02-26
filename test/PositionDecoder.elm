module PositionDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PositionType exposing (..)


decodePosition : Decoder Position
decodePosition =
    string |> andThen ( \x ->
        if x == "Beginning" then decode Beginning
        else if x == "Middle" then decode Middle
        else if x == "End" then decode End
        else fail "Constructor not matched" )
